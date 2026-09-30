-- | Apply dictionary decisions to identified expressions, never traversal order.
module Unison.Typechecker.GivenApply
  ( Rewrite (..),
    ApplyError (..),
    apply,
    dictionaryTerm,
  )
where

import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann, SiteId, siteId)
import Unison.Prelude
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Typechecker.GivenResolver qualified as Given

-- | Parameters are outermost first; arguments are applied left to right.
data Rewrite v = Rewrite
  { parameters :: [v],
    arguments :: [Term v Ann]
  }

data ApplyError
  = MissingSite Ann
  | DuplicateSite SiteId
  | UnusedSites (Set SiteId)
  deriving stock (Eq, Show)

-- | Rewrite the numbered source tree after local binders have been freshened.
-- Newly inserted terms are not traversed again. Invalid/stale site maps fail
-- closed instead of dropping decisions or moving them to another expression.
apply :: forall v. (Ord v) => Map SiteId (Rewrite v) -> Term v Ann -> Either ApplyError (Term v Ann)
apply decisions source = do
  (result, seen) <- runStateT (go source) Set.empty
  let unused = Map.keysSet decisions `Set.difference` seen
  if Set.null unused then pure result else Left (UnusedSites unused)
  where
    go :: Term v Ann -> StateT (Set SiteId) (Either ApplyError) (Term v Ann)
    go node = do
      let location = ABT.annotation node
      site <- lift (maybe (Left (MissingSite location)) Right (siteId location))
      seen <- get
      when (Set.member site seen) (lift (Left (DuplicateSite site)))
      put (Set.insert site seen)
      body <- case ABT.out node of
        ABT.Var v -> pure (ABT.annotatedVar location v)
        ABT.Abs v body -> ABT.abs' location v <$> go body
        ABT.Cycle body -> ABT.cycle' location <$> go body
        ABT.Tm functor -> ABT.tm' location <$> traverse go functor
      pure $ case Map.lookup site decisions of
        Nothing -> body
        Just Rewrite {parameters, arguments} ->
          foldr (\v -> Term.lam location (location, v)) (foldl' (Term.app location) body arguments) parameters

-- | Resolution trees preserve both the chosen identity and recursive premises.
dictionaryTerm :: (Ord v) => Ann -> Given.ResolutionTree v loc -> Term v Ann
dictionaryTerm location Given.ResolutionTree {Given.resolvedGiven, Given.resolvedPremises} =
  foldl' (Term.app location) chosen (dictionaryTerm location <$> resolvedPremises)
  where
    chosen = case resolvedGiven of
      Given.Local v -> Term.var location v
      Given.Global ref -> Term.ref location ref
