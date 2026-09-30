-- | Identify dictionary insertion sites and local binders before typechecking.
module Unison.Typechecker.GivenSites
  ( PreparedTerm,
    prepare,
    preparedTerm,
    binderRenamings,
    renameLocals,
    binderDepths,
  )
where

import Control.Monad.State.Strict (evalState, state)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann, SiteId (..), atSite, siteId)
import Unison.Prelude
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Var (Var)
import Unison.Var qualified as Var

data PreparedTerm v = PreparedTerm
  { preparedTerm :: Term v Ann,
    binderRenamings :: Map SiteId v,
    binderDepths :: Map SiteId Int
  }

-- | Assign identities even when nodes share a source range. Plan fresh local
-- names without changing binders yet: duplicate binding/pattern diagnostics
-- must run on the original names. File-level names remain unchanged.
prepare :: (Var v) => Term v Ann -> PreparedTerm v
prepare original = PreparedTerm numbered renamings (depths 0 numbered)
  where
    numbered = flip evalState 0 $ ABT.rewriteDown_ identify original
    identify node = state $ \next ->
      (ABT.annotate (atSite (SiteId next) (ABT.annotation node)) node, next + 1)
    nodes node = node : foldMap nodes (ABT.out node)
    allNodes = nodes numbered
    topSites =
      Set.fromList
        [ site
        | Term.LetRecNamedAnnotatedTop' True _ bindings _ <- allNodes,
          ((location, _), _) <- bindings,
          Just site <- [siteId location]
        ]
    locals =
      [ (site, v)
      | node <- allNodes,
        ABT.Abs v _ <- [ABT.out node],
        Just site <- [siteId (ABT.annotation node)],
        Set.notMember site topSites
      ]
    depths depth node = case node of
      Term.LetRecNamedAnnotatedTop' top _ bindings body ->
        let here = if top then depth else depth + 1
         in Map.fromList [(site, here) | ((location, _), _) <- bindings, Just site <- [siteId location]]
              <> foldMap (depths here) (body : map snd bindings)
      _ -> case ABT.out node of
        ABT.Abs _ body ->
          Map.fromList [(site, depth + 1) | Just site <- [siteId (ABT.annotation node)]] <> depths (depth + 1) body
        other -> foldMap (depths depth) other
    renamings = Map.fromList $
      flip evalState (Set.fromList (ABT.allVars original)) $
        for locals \(site, v) -> state $ \used ->
          let fresh = Var.freshIn used v
           in ((site, fresh), Set.insert fresh used)

-- | Use this after checking, so generated dictionary references can refer to
-- unique local bindings without being captured by a same-named inner binder.
renameLocals :: (Ord v) => PreparedTerm v -> Term v Ann
renameLocals PreparedTerm {preparedTerm, binderRenamings} = go Map.empty preparedTerm
  where
    go names node =
      let location = ABT.annotation node
       in case ABT.out node of
            ABT.Var v -> ABT.annotatedVar location (Map.findWithDefault v v names)
            ABT.Abs v body ->
              let fresh = fromMaybe v (siteId location >>= (`Map.lookup` binderRenamings))
               in ABT.abs' location fresh (go (Map.insert v fresh names) body)
            ABT.Cycle body -> ABT.cycle' location (go names body)
            ABT.Tm functor -> ABT.tm' location (go names <$> functor)
