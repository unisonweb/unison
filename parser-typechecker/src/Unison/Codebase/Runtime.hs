{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Runtime where

import Unison.Prelude

import qualified Data.Map as Map
import qualified Unison.ABT as ABT
import Unison.Builtin.Decls (tupleTerm, pattern TupleTerm')
import qualified Unison.Codebase.CodeLookup as CL
import qualified Unison.Codebase.CodeLookup.Util as CL
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Parser.Ann (Ann)
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import Unison.Type (Type)
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import Unison.Var (Var)
import qualified Unison.Var as Var
import Unison.WatchKind (WatchKind)
import qualified Unison.WatchKind as WK

type Error = P.Pretty P.ColorText
type Term v = Term.Term v ()

data Runtime v = Runtime
  { terminate :: IO ()
  , evaluate
      :: CL.CodeLookup v IO ()
      -> PPE.PrettyPrintEnv
      -> Term v
      -> IO (Either Error (Term v))
  , compileTo
      :: CL.CodeLookup v IO ()
      -> PPE.PrettyPrintEnv
      -> Reference
      -> FilePath
      -> IO (Maybe Error)
  , mainType :: Type v Ann
  , ioTestType :: Type v Ann
  }

type IsCacheHit = Bool

noCache :: Reference.Id -> IO (Maybe (Term v))
noCache _ = pure Nothing

type WatchResults v a = (Either Error
         -- Bindings:
       ( [(v, Term v)]
         -- Map watchName (loc, hash, expression, value, isHit)
       , Map v (a, WatchKind, Reference.Id, Term v, Term v, IsCacheHit)
       ))

-- Evaluates the watch expressions in the file, returning a `Map` of their
-- results. This has to be a bit fancy to handle that the definitions in the
-- file depend on each other and evaluation must proceed in a way that respects
-- these dependencies.
--
-- Note: The definitions in the file are hashed and looked up in
-- `evaluationCache`. If that returns a result, evaluation of that definition
-- can be skipped.
evaluateWatches
  :: forall v a
   . Var v
  => CL.CodeLookup v IO a
  -> PPE.PrettyPrintEnv
  -> (Reference.Id -> IO (Maybe (Term v)))
  -> Runtime v
  -> TypecheckedUnisonFile v a
  -> IO (WatchResults v a)
evaluateWatches code ppe evaluationCache rt tuf = do
  -- 1. compute hashes for everything in the file
  let m :: Map v (Reference.Id, Term.Term v a)
      m = fmap (\(id, _wk, tm, _tp) -> (id, tm)) (UF.hashTermsId tuf)
      watches :: Set v = Map.keysSet watchKinds
      watchKinds :: Map v WatchKind
      watchKinds =
        Map.fromList
          [(v, k) | (k, ws) <- UF.watchComponents tuf, (v, _tm, _tp) <- ws]
      unann = Term.amap (const ())
  -- 2. use the cache to lookup things already computed
  m' <- fmap Map.fromList . for (Map.toList m) $ \(v, (r, t)) -> do
    o <- evaluationCache r
    case o of
      Nothing -> pure (v, (r, ABT.annotation t, unann t, False))
      Just t' -> pure (v, (r, ABT.annotation t, t', True))
  -- 3. create a big ol' let rec whose body is a big tuple of all watches
  let rv :: Map Reference.Id v
      rv = Map.fromList [ (r, v) | (v, (r, _)) <- Map.toList m ]
      bindings :: [(v, Term v)]
      bindings     = [ (v, unref rv b) | (v, (_, _, b, _)) <- Map.toList m' ]
      watchVars    = [ Term.var () v | v <- toList watches ]
      bigOl'LetRec = Term.letRec' True bindings (tupleTerm watchVars)
      cl           = void (CL.fromTypecheckedUnisonFile tuf) <> void code
  -- 4. evaluate it and get all the results out of the tuple, then
  -- create the result Map
  out <- evaluate rt cl ppe bigOl'LetRec
  case out of
    Right out -> do
      let
        (bindings, results) = case out of
          TupleTerm' results -> (mempty, results)
          Term.LetRecNamed' bs (TupleTerm' results) -> (bs, results)
          _ -> error $ "Evaluation should produce a tuple, but gave: " ++ show out
      let go v eval (ref, a, uneval, isHit) =
            (a, Map.findWithDefault (die v) v watchKinds,
             ref, uneval, Term.etaNormalForm eval, isHit)
          watchMap = Map.intersectionWithKey go
            (Map.fromList (toList watches `zip` results)) m'
          die v = error $ "not sure what kind of watch this is: " <> show v
      pure $ Right (bindings, watchMap)
    Left e -> pure (Left e)
 where
    -- unref :: Map Reference.Id v -> Term.Term v a -> Term.Term v a
  unref rv t = ABT.visitPure go t
   where
    go t@(Term.Ref' (Reference.DerivedId r)) = case Map.lookup r rv of
      Nothing -> Nothing
      Just v  -> Just (Term.var (ABT.annotation t) v)
    go _ = Nothing

evaluateTerm'
  :: (Var v, Monoid a)
  => CL.CodeLookup v IO a
  -> (Reference.Id -> IO (Maybe (Term v)))
  -> PPE.PrettyPrintEnv
  -> Runtime v
  -> Term.Term v a
  -> IO (Either Error (Term v))
evaluateTerm' codeLookup cache ppe rt tm = do
  result <- cache (Hashing.hashClosedTerm tm)
  case result of
    Just r -> pure (Right r)
    Nothing -> do
      let
        tuf = UF.typecheckedUnisonFile mempty mempty mempty
                 [(WK.RegularWatch, [(Var.nameds "result", tm, mempty <$> mainType rt)])]
      r <- evaluateWatches (void codeLookup) ppe cache rt (void tuf)
      pure $ r <&> \(_,map) ->
        let [(_loc, _kind, _hash, _src, value, _isHit)] = Map.elems map
        in value

evaluateTerm
  :: (Var v, Monoid a)
  => CL.CodeLookup v IO a -> PPE.PrettyPrintEnv -> Runtime v -> Term.Term v a
  -> IO (Either Error (Term v))
evaluateTerm codeLookup = evaluateTerm' codeLookup noCache
