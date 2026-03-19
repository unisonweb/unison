{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Runtime where

import Control.Lens
import Control.Monad.Except
import Data.Map qualified as Map
import Data.Set.NonEmpty (NESet)
import Data.Zip qualified as Zip
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls (tupleTerm, pattern TupleTerm')
import Unison.Codebase.CodeLookup qualified as CL
import Unison.Codebase.CodeLookup.Util qualified as CL
import Unison.Codebase.Runtime.Profile
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.Util.Pretty qualified as P
import Unison.Var (Var)
import Unison.Var qualified as Var
import Unison.WatchKind (WatchKind)
import Unison.WatchKind qualified as WK

data Response e
  = DecompErrs [e]
  | Profile (P.Pretty P.ColorText)
  | EmptyResponse

instance Semigroup (Response e) where
  DecompErrs l <> DecompErrs r = DecompErrs (l <> r)
  d@(DecompErrs _) <> _ = d
  _ <> d@(DecompErrs _) = d
  p@(Profile _) <> _ = p
  _ <> p@(Profile _) = p
  EmptyResponse <> r = r

instance Monoid (Response e) where
  mempty = EmptyResponse

type Term v = Term.Term v ()

data CompileOpts = COpts
  { profile :: Bool
  }

defaultCompileOpts :: CompileOpts
defaultCompileOpts = COpts {profile = False}

data Runtime e e' v = Runtime
  { terminate :: IO (),
    evaluate ::
      CL.CodeLookup v IO () ->
      PPE.PrettyPrintEnv ->
      ProfileSpec ->
      Term v ->
      IO (Either e (Response e', Term v)),
    compileTo ::
      CompileOpts ->
      CL.CodeLookup v IO () ->
      PPE.PrettyPrintEnv ->
      Reference ->
      FilePath ->
      IO (Maybe e),
    mainType :: Type v Ann,
    ioTestTypes :: NESet (Type v Ann)
  }

type IsCacheHit = Bool

noCache :: Reference.Id -> IO (Maybe (Term v))
noCache _ = pure Nothing

type WatchResults e e' v a =
  Either
    e
    -- Bindings:
    ( [(v, Term v)],
      -- Map watchName (loc, hash, expression, value, isHit)
      Response e',
      Map v (a, WatchKind, Reference.Id, Term v, Term v, IsCacheHit)
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
  forall e e' v a.
  (Var v) =>
  CL.CodeLookup v IO a ->
  PPE.PrettyPrintEnv ->
  ProfileSpec ->
  (Reference.Id -> IO (Maybe (Term v))) ->
  Runtime e e' v ->
  TypecheckedUnisonFile v a ->
  IO (WatchResults e e' v a)
evaluateWatches code ppe prof evaluationCache rt tuf = do
  -- 1. compute hashes for everything in the file
  let m :: Map v (Reference.Id, Term.Term v a)
      m = fmap (\(_a, id, _wk, tm, _tp) -> (id, tm)) (UF.hashTermsId tuf)
      watches :: Set v = Map.keysSet watchKinds
      watchKinds :: Map v WatchKind
      watchKinds =
        Map.fromList
          [(v, k) | (k, ws) <- UF.watchComponents tuf, (v, _a, _tm, _tp) <- ws]
      unann = Term.amap (const ())
  -- 2. use the cache to lookup things already computed
  m' <- fmap Map.fromList . for (Map.toList m) $ \(v, (r, t)) -> do
    o <- evaluationCache r
    case o of
      Nothing -> pure (v, (r, ABT.annotation t, unann t, False))
      Just t' -> pure (v, (r, ABT.annotation t, t', True))
  -- 3. create a big ol' let rec whose body is a big tuple of all watches
  let rv :: Map Reference.Id v
      rv = Map.fromList [(r, v) | (v, (r, _)) <- Map.toList m]
      bindings :: [(v, (), Term v)]
      bindings = [(v, (), unref rv b) | (v, (_, _, b, _)) <- Map.toList m']
      watchVars = [Term.var () v | v <- toList watches]
      bigOl'LetRec = Term.letRec' True bindings (tupleTerm watchVars)
      cl = void (CL.fromTypecheckedUnisonFile tuf) <> void code
  -- 4. evaluate it and get all the results out of the tuple, then
  -- create the result Map
  out <- evaluate rt cl ppe prof bigOl'LetRec
  case out of
    Right (errs, out) -> do
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
      pure $ Right (bindings, errs, watchMap)
    Left e -> pure (Left e)
  where
    -- unref :: Map Reference.Id v -> Term.Term v a -> Term.Term v a
    unref rv t = ABT.visitPure go t
      where
        go t@(Term.Ref' (Reference.DerivedId r)) = case Map.lookup r rv of
          Nothing -> Nothing
          Just v -> Just (Term.var (ABT.annotation t) v)
        go _ = Nothing

evaluateTerm' ::
  (Var v, Monoid a) =>
  CL.CodeLookup v IO a ->
  (Reference.Id -> IO (Maybe (Term v))) ->
  PPE.PrettyPrintEnv ->
  ProfileSpec ->
  Runtime e e' v ->
  Term.Term v a ->
  IO (Either e (Response e', Term v))
evaluateTerm' codeLookup cache ppe prof rt tm = do
  result <- cache (Hashing.hashClosedTerm tm)
  case result of
    Just r -> pure (Right (EmptyResponse, r))
    Nothing -> do
      let tuf =
            UF.typecheckedUnisonFile
              mempty
              mempty
              mempty
              [(WK.RegularWatch, [(Var.nameds "result", mempty, tm, mempty <$> mainType rt)])]
      r <- evaluateWatches (void codeLookup) ppe prof cache rt (void tuf)
      pure $
        r <&> \(_, errs, map) ->
          case Map.elems map of
            [(_loc, _kind, _hash, _src, value, _isHit)] -> (errs, value)
            _ -> error "evaluateTerm': Pattern mismatch on watch results"

evaluateTerm ::
  (Var v, Monoid a) =>
  CL.CodeLookup v IO a ->
  PPE.PrettyPrintEnv ->
  ProfileSpec ->
  Runtime e e' v ->
  Term.Term v a ->
  IO (Either e (Response e', Term v))
evaluateTerm codeLookup = evaluateTerm' codeLookup noCache

evaluateTermBatch ::
  forall v a e e'.
  (Var v, Monoid a) =>
  CL.CodeLookup v IO a ->
  (Reference.Id -> IO (Maybe (Term v))) ->
  PPE.PrettyPrintEnv ->
  ProfileSpec ->
  Runtime e e' v ->
  Map v (Term.Term v a) ->
  IO (Either e (Map v (Response e', Term v)))
evaluateTermBatch codeLookup cache ppe prof rt tms = do
  results :: Map v (Maybe (Term v)) <- traverse (cache . Hashing.hashClosedTerm) tms
  let cacheTagged :: Map v (Either (Response e', Term v) (v, Term.Term v a))
      cacheTagged =
        Zip.zip tms results
          & Map.mapWithKey \k -> \case
            (_tm, Just cached) -> (Left (EmptyResponse, cached))
            (tm, Nothing) -> (Right (k, tm))

  cacheTagged
    & unsafePartsOf (traversed . _Right)
      %%~ ( \vs -> do
              let watches = vs <&> \(v, tm) -> (WK.RegularWatch, [(v, mempty, tm, mempty <$> mainType rt)])
              let tuf =
                    UF.typecheckedUnisonFile
                      mempty
                      mempty
                      mempty
                      watches
              r <- liftIO $ evaluateWatches (void codeLookup) ppe prof cache rt (void tuf)
              case r of
                Left e -> throwError e
                Right (_, errs, resultMap) ->
                  pure
                    ( vs <&> \(v, _tm) ->
                        let (_loc, _kind, _hash, _src, value, _isHit) = resultMap Map.! v
                         in (errs, value)
                    )
          )
    & runExceptT
    <&> (fmap . fmap) (either id id)
