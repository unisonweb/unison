{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Runtime where

import Data.Bifunctor (first)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import Unison.Builtin.Decls (tupleTerm, pattern TupleTerm')
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.CodeLookup as CL
import Unison.Parser (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import Unison.Type (Type)
import Unison.UnisonFile (UnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import Unison.Var (Var)
import qualified Unison.Var as Var

type Error = P.Pretty P.ColorText

type Term v = Term.Term v ()

data Runtime v = Runtime
  { terminate :: IO (),
    evaluate ::
      CL.CodeLookup v IO () ->
      PPE.PrettyPrintEnv ->
      Term v ->
      IO (Either Error (Term v)),
    mainType :: Type v Ann,
    ioTestType :: Type v Ann,
    needsContainment :: Bool
  }

type IsCacheHit = Bool

noCache :: Reference -> IO (Maybe (Term v))
noCache _ = pure Nothing

type WatchResults v a =
  ( Either
      Error
      -- Bindings:
      ( [(v, Term v)],
        -- Map watchName (loc, hash, expression, value, isHit)
        Map v (a, UF.WatchKind, Reference, Term v, Term v, IsCacheHit)
      )
  )

-- Evaluates the watch expressions in the file, returning a `Map` of their
-- results. This has to be a bit fancy to handle that the definitions in the
-- file depend on each other and evaluation must proceed in a way that respects
-- these dependencies.
--
-- Note: The definitions in the file are hashed and looked up in
-- `evaluationCache`. If that returns a result, evaluation of that definition
-- can be skipped.
evaluateWatches ::
  forall v a.
  Var v =>
  CL.CodeLookup v IO a ->
  PPE.PrettyPrintEnv ->
  (Reference -> IO (Maybe (Term v))) ->
  Runtime v ->
  UnisonFile v a ->
  IO (WatchResults v a)
evaluateWatches code ppe evaluationCache rt uf = do
  -- 1. compute hashes for everything in the file
  let m :: Map v (Reference, Term.Term v a)
      m =
        first Reference.DerivedId
          <$> Term.hashComponents (Map.fromList (UF.terms uf <> UF.allWatches uf))
      watches = Set.fromList (fst <$> UF.allWatches uf)
      watchKinds :: Map v UF.WatchKind
      watchKinds =
        Map.fromList
          [ (v, k) | (k, ws) <- Map.toList (UF.watches uf), (v, _) <- ws
          ]
      unann = Term.amap (const ())
  -- 2. use the cache to lookup things already computed
  m' <- fmap Map.fromList . for (Map.toList m) $ \(v, (r, t)) -> do
    o <- evaluationCache r
    case o of
      Nothing -> pure (v, (r, ABT.annotation t, unann t, False))
      Just t' -> pure (v, (r, ABT.annotation t, t', True))
  -- 3. create a big ol' let rec whose body is a big tuple of all watches
  let rv :: Map Reference v
      rv = Map.fromList [(r, v) | (v, (r, _)) <- Map.toList m]
      bindings :: [(v, Term v)]
      bindings = [(v, unref rv b) | (v, (_, _, b, _)) <- Map.toList m']
      watchVars = [Term.var () v | v <- toList watches]
      bigOl'LetRec = Term.letRec' True bindings (tupleTerm watchVars)
      cl = void $ CL.fromUnisonFile uf <> code
  -- 4. evaluate it and get all the results out of the tuple, then
  -- create the result Map
  out <- evaluate rt cl ppe bigOl'LetRec
  case out of
    Right out -> do
      let (bindings, results) = case out of
            TupleTerm' results -> (mempty, results)
            Term.LetRecNamed' bs (TupleTerm' results) -> (bs, results)
            _ -> error $ "Evaluation should produce a tuple, but gave: " ++ show out
      let go v eval (ref, a, uneval, isHit) =
            ( a,
              Map.findWithDefault (die v) v watchKinds,
              ref,
              uneval,
              Term.etaNormalForm eval,
              isHit
            )
          watchMap =
            Map.intersectionWithKey
              go
              (Map.fromList (toList watches `zip` results))
              m'
          die v = error $ "not sure what kind of watch this is: " <> show v
      pure $ Right (bindings, watchMap)
    Left e -> pure (Left e)
  where
    -- unref :: Map Reference v -> Term.Term v a -> Term.Term v a
    unref rv t = ABT.visitPure go t
      where
        go t@(Term.Ref' r@(Reference.DerivedId _)) = case Map.lookup r rv of
          Nothing -> Nothing
          Just v -> Just (Term.var (ABT.annotation t) v)
        go _ = Nothing

evaluateTerm ::
  (Var v, Monoid a) =>
  CL.CodeLookup v IO a ->
  PPE.PrettyPrintEnv ->
  Runtime v ->
  Term.Term v a ->
  IO (Either Error (Term v))
evaluateTerm codeLookup ppe rt tm = do
  let uf =
        UF.UnisonFileId
          mempty
          mempty
          mempty
          (Map.singleton UF.RegularWatch [(Var.nameds "result", tm)])
  runnable <-
    if needsContainment rt
      then Codebase.makeSelfContained' codeLookup uf
      else pure uf
  r <- evaluateWatches codeLookup ppe noCache rt runnable
  pure $
    r <&> \(_, map) ->
      let [(_loc, _kind, _hash, _src, value, _isHit)] = Map.elems map
       in value
