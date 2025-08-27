{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Runtime.Interface
  ( startRuntime,
    withRuntime,
    standalone,
    runStandalone,
    StoredCache
      ( -- Exported for tests
        SCache
      ),
    decodeStandalone,
    RuntimeHost (..),
    Runtime (..),

    -- * Exported for tests
    getStoredCache,
    putStoredCache,
  )
where

import Control.Concurrent.STM as STM
import Control.Exception (fromException, tryJust)
import Control.Monad
import Control.Monad.State
import Data.Binary.Get (runGetOrFail)
import Data.Bitraversable (bitraverse)
import Data.ByteString.Lazy qualified as BL
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut, runPutL)
import Data.Bytes.Serial
import Data.Foldable
import Data.IORef
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Set as Set
  ( filter,
    fromList,
    map,
    notMember,
    singleton,
    (\\),
  )
import Data.Set qualified as Set
import Data.Text as Text (unpack)
import Data.Void (absurd)
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as RF
import Unison.Codebase.CodeLookup (CodeLookup (..))
import Unison.Codebase.MainTerm (builtinIOTestTypes, builtinMain)
import Unison.Codebase.Runtime (CompileOpts (..), Error, Runtime (..))
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorReference qualified as RF
import Unison.DataDeclaration (Decl, declFields, declTypeDependencies)
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.LabeledDependency qualified as RF
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude
import Unison.PrettyPrintEnv
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference)
import Unison.Reference qualified as RF
import Unison.Referent qualified as RF (pattern Ref)
import Unison.Runtime.ANF as ANF
import Unison.Runtime.ANF.Optimize as ANF
import Unison.Runtime.ANF.Rehash as ANF (rehashGroups)
import Unison.Runtime.ANF.Serialize as ANF
  ( getGroupCurrent,
    getOptInfos,
    putGroup,
    putOptInfos,
  )
import Unison.Runtime.Builtin
import Unison.Runtime.Decompile
import Unison.Runtime.Exception
  ( die,
    listErrors,
    prettyCompileExn,
    prettyRuntimeExn,
    prettyRuntimeExnSansCtx,
    tabulateErrors,
  )
import Unison.Runtime.Foreign.Function (functionUnreplacements)
import Unison.Runtime.MCode
  ( Args (..),
    CombIx (..),
    GInstr (..),
    GSection (..),
    RCombs,
    RefNums (..),
    absurdCombs,
    combTypes,
    emitComb,
    emptyRNs,
    resolveCombs,
    sanitizeCombsOfForeignFuncs,
  )
import Unison.Runtime.MCode.Serialize
import Unison.Runtime.Machine
  ( ActiveThreads,
    CCache (..),
    Combs,
    Tracer (..),
    apply0,
    baseCCache,
    cacheAdd,
    cacheAdd0,
    eval0,
    expandSandbox,
    preEvalTopLevelConstants,
    refLookup,
    refNumTm,
    refNumsTm,
    refNumsTy,
    resolveSection,
  )
import Unison.Runtime.Pattern
import Unison.Runtime.Serialize as SER
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Syntax.TermPrinter
import Unison.Term qualified as Tm
import Unison.Type qualified as Type
import Unison.Util.EnumContainers as EC
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Pretty as P
import Unison.Util.Recursion qualified as Rec
import UnliftIO qualified
import UnliftIO.Concurrent qualified as UnliftIO

type Term v = Tm.Term v ()

type Type v = Type.Type v ()

-- Note that these annotations are suggestions at best, since in many places codebase refs, intermediate refs, and
-- floated refs are all intermingled.
type CodebaseReference = Reference

-- Note that these annotations are suggestions at best, since in many places codebase refs, intermediate refs, and
-- floated refs are all intermingled.
type IntermediateReference = Reference

-- Note that these annotations are suggestions at best, since in many places codebase refs, intermediate refs, and
-- floated refs are all intermingled.
type FloatedReference = Reference

data Remapping from to = Remap
  { remap :: Map.Map from to,
    backmap :: Map.Map to from
  }

instance (Ord from, Ord to) => Semigroup (Remapping from to) where
  Remap r1 b1 <> Remap r2 b2 = Remap (r1 <> r2) (b1 <> b2)

instance (Ord from, Ord to) => Monoid (Remapping from to) where
  mempty = Remap mempty mempty

data EvalCtx = ECtx
  { dspec :: DataSpec,
    floatRemap :: Remapping CodebaseReference FloatedReference,
    intermedRemap :: Remapping FloatedReference IntermediateReference,
    decompTm :: Map.Map Reference (Map.Map Word64 (Term Symbol)),
    ccache :: CCache
  }

uncurryDspec :: DataSpec -> Map.Map ConstructorReference Int
uncurryDspec = Map.fromList . concatMap f . Map.toList
  where
    f (r, l) = zipWith (\n c -> (ConstructorReference r n, c)) [0 ..] $ either id id l

cacheContext :: CCache -> EvalCtx
cacheContext =
  ECtx builtinDataSpec mempty mempty
    . Map.fromList
    $ Map.keys builtinTermNumbering
      <&> \r -> (r, Map.singleton 0 (Tm.ref () r))

baseContext :: Bool -> IO EvalCtx
baseContext sandboxed = cacheContext <$> baseCCache sandboxed

resolveTermRef ::
  CodeLookup Symbol IO () ->
  RF.Reference ->
  IO (Term Symbol)
resolveTermRef _ b@(RF.Builtin _) =
  die [] $ "Unknown builtin term reference: " ++ show b
resolveTermRef cl r@(RF.DerivedId i) =
  getTerm cl i >>= \case
    Nothing -> die [] $ "Unknown term reference: " ++ show r
    Just tm -> pure tm

allocType ::
  EvalCtx ->
  RF.Reference ->
  Either [Int] [Int] ->
  IO EvalCtx
allocType _ b@(RF.Builtin _) _ =
  die [] $ "Unknown builtin type reference: " ++ show b
allocType ctx r cons =
  pure $ ctx {dspec = Map.insert r cons $ dspec ctx}

recursiveDeclDeps ::
  CodeLookup Symbol IO () ->
  Decl Symbol () ->
  -- (type deps, term deps)
  StateT (Set RF.LabeledDependency) IO (Set Reference, Set Reference)
recursiveDeclDeps cl d = do
  seen0 <- get
  let seen = seen0 <> Set.map RF.typeRef deps
  put seen
  let newDeps = Set.filter (\r -> notMember (RF.typeRef r) seen0) deps
  rec <-
    (toList newDeps) & foldMapM \r -> do
      case r of
        RF.DerivedId i ->
          lift (getTypeDeclaration cl i) >>= \case
            Just d -> recursiveDeclDeps cl d
            Nothing -> pure mempty
        _ -> pure mempty
  pure $ (deps, mempty) <> rec
  where
    deps = declTypeDependencies d

categorize :: RF.LabeledDependency -> (Set Reference, Set Reference)
categorize =
  \case
    RF.TypeReference ref -> (Set.singleton ref, mempty)
    RF.ConReference (RF.ConstructorReference ref _conId) _conType -> (Set.singleton ref, mempty)
    RF.TermReference ref -> (mempty, Set.singleton ref)

recursiveTermDeps ::
  CodeLookup Symbol IO () ->
  Term Symbol ->
  -- (type deps, term deps)
  StateT (Set RF.LabeledDependency) IO (Set Reference, Set Reference)
recursiveTermDeps cl tm = do
  seen0 <- get
  let seen = seen0 <> deps
  put seen
  rec <-
    (toList (deps \\ seen0)) & foldMapM \r ->
      case r of
        RF.ConReference (RF.ConstructorReference (RF.DerivedId refId) _conId) _conType -> handleTypeReferenceId refId
        RF.TypeReference (RF.DerivedId refId) -> handleTypeReferenceId refId
        RF.TermReference r -> recursiveRefDeps cl r
        _ -> pure mempty
  pure $ foldMap categorize deps <> rec
  where
    handleTypeReferenceId :: RF.Id -> StateT (Set RF.LabeledDependency) IO (Set Reference, Set Reference)
    handleTypeReferenceId refId =
      lift (getTypeDeclaration cl refId) >>= \case
        Just d -> recursiveDeclDeps cl d
        Nothing -> pure mempty
    deps = Tm.labeledDependencies tm

recursiveRefDeps ::
  CodeLookup Symbol IO () ->
  Reference ->
  StateT (Set RF.LabeledDependency) IO (Set Reference, Set Reference)
recursiveRefDeps cl (RF.DerivedId i) =
  lift (getTerm cl i) >>= \case
    Just tm -> recursiveTermDeps cl tm
    Nothing -> pure mempty
recursiveRefDeps _ _ = pure mempty

recursiveIRefDeps ::
  Map.Map Reference (SuperGroup Reference Symbol) ->
  Set Reference ->
  [Reference] ->
  Set Reference
recursiveIRefDeps cl seen0 rfs = srfs <> foldMap f rfs
  where
    seen = seen0 <> srfs
    srfs = Set.fromList rfs
    f = foldMap (recursiveGroupDeps cl seen) . flip Map.lookup cl

recursiveGroupDeps ::
  Map.Map Reference (SuperGroup Reference Symbol) ->
  Set Reference ->
  SuperGroup Reference Symbol ->
  Set Reference
recursiveGroupDeps cl seen0 grp = deps <> recursiveIRefDeps cl seen depl
  where
    depl = Prelude.filter (`Set.notMember` seen0) $ groupTermLinks grp
    deps = Set.fromList depl
    seen = seen0 <> deps

recursiveIntermedDeps ::
  Map.Map Reference (SuperGroup Reference Symbol) ->
  [Reference] ->
  [(Reference, SuperGroup Reference Symbol)]
recursiveIntermedDeps cl rfs = mapMaybe f $ Set.toList ds
  where
    ds = recursiveIRefDeps cl mempty rfs
    f rf = fmap (rf,) (Map.lookup rf cl)

collectDeps ::
  CodeLookup Symbol IO () ->
  Term Symbol ->
  IO ([(Reference, Either [Int] [Int])], [Reference])
collectDeps cl tm = do
  (tys, tms) <- evalStateT (recursiveTermDeps cl tm) mempty
  (,toList tms) <$> (traverse getDecl (toList tys))
  where
    getDecl ty@(RF.DerivedId i) =
      (ty,) . maybe (Right []) declFields
        <$> getTypeDeclaration cl i
    getDecl r = pure (r, Right [])

collectRefDeps ::
  CodeLookup Symbol IO () ->
  Reference ->
  IO ([(Reference, Either [Int] [Int])], [Reference])
collectRefDeps cl r = do
  tm <- resolveTermRef cl r
  (tyrs, tmrs) <- collectDeps cl tm
  pure (tyrs, r : tmrs)

backrefAdd ::
  Map.Map Reference (Map.Map Word64 (Term Symbol)) ->
  EvalCtx ->
  EvalCtx
backrefAdd m ctx@ECtx {decompTm} =
  ctx {decompTm = m <> decompTm}

remapAdd :: (Ord from, Ord to) => Map.Map from to -> Remapping from to -> Remapping from to
remapAdd m Remap {remap, backmap} =
  Remap {remap = m <> remap, backmap = tm <> backmap}
  where
    tm = Map.fromList . fmap (\(x, y) -> (y, x)) $ Map.toList m

floatRemapAdd :: Map.Map Reference Reference -> EvalCtx -> EvalCtx
floatRemapAdd m ctx@ECtx {floatRemap} =
  ctx {floatRemap = remapAdd m floatRemap}

intermedRemapAdd :: Map.Map Reference Reference -> EvalCtx -> EvalCtx
intermedRemapAdd m ctx@ECtx {intermedRemap} =
  ctx {intermedRemap = remapAdd m intermedRemap}

baseToIntermed :: EvalCtx -> CodebaseReference -> Maybe IntermediateReference
baseToIntermed ctx r = do
  r <- Map.lookup r . remap $ floatRemap ctx
  Map.lookup r . remap $ intermedRemap ctx

-- Runs references through the forward maps to get intermediate
-- references. Works on both base and floated references.
toIntermed :: EvalCtx -> Reference -> IntermediateReference
toIntermed ctx r
  | r <- Map.findWithDefault r r . remap $ floatRemap ctx,
    Just r <- Map.lookup r . remap $ intermedRemap ctx =
      r
toIntermed _ r = r

floatToIntermed :: EvalCtx -> FloatedReference -> Maybe IntermediateReference
floatToIntermed ctx r =
  Map.lookup r . remap $ intermedRemap ctx

intermedToBase :: EvalCtx -> IntermediateReference -> Maybe CodebaseReference
intermedToBase ctx r = do
  r <- Map.lookup r . backmap $ intermedRemap ctx
  Map.lookup r . backmap $ floatRemap ctx

-- Runs references through the backmaps with defaults at all steps.
backmapRef :: EvalCtx -> Reference -> CodebaseReference
backmapRef ctx r0 = r2
  where
    r1 = Map.findWithDefault r0 r0 . backmap $ intermedRemap ctx
    r2 = Map.findWithDefault r1 r1 . backmap $ floatRemap ctx

-- Runs references through the backmaps with defaults at all steps.
maybeBackmapRef :: EvalCtx -> Reference -> Maybe CodebaseReference
maybeBackmapRef ctx r0 = do
  r1 <- Map.lookup r0 . backmap $ intermedRemap ctx
  Map.lookup r1 . backmap $ floatRemap ctx

performRehash ::
  Map.Map Reference (SuperGroup Reference Symbol) ->
  EvalCtx ->
  (EvalCtx, Map Reference Reference, [(Reference, SuperGroup Reference Symbol)])
performRehash rgrp0 ctx =
  (intermedRemapAdd rrefs ctx, rrefs, Map.toList rrgrp)
  where
    frs = remap $ floatRemap ctx
    irs = remap $ intermedRemap ctx
    f b r
      | not b,
        r `Map.notMember` rgrp0,
        r <- Map.findWithDefault r r frs,
        Just r <- Map.lookup r irs =
          r
      | otherwise = r

    (rrefs, rrgrp) =
      case rehashGroups $ fmap (overGroupLinks f) rgrp0 of
        Left (msg, refs) -> error $ unpack msg ++ ": " ++ show refs
        Right p -> p

loadCode ::
  CodeLookup Symbol IO () ->
  PrettyPrintEnv ->
  EvalCtx ->
  [Reference] ->
  IO (EvalCtx, [(Reference, SuperGroup Reference Symbol)])
loadCode cl ppe ctx tmrs = do
  igs <- readTVarIO (intermed $ ccache ctx)
  q <-
    refNumsTm (ccache ctx) <&> \m r -> case r of
      RF.DerivedId {}
        | Just r <- baseToIntermed ctx r -> r `Map.notMember` m
        | Just r <- floatToIntermed ctx r -> r `Map.notMember` m
        | otherwise -> True
      _ -> False
  let (new, old) = L.partition q tmrs
      odeps = recursiveIntermedDeps igs $ toIntermed ctx <$> old
  itms <-
    traverse (\r -> (RF.unsafeId r,) <$> resolveTermRef cl r) new
  let im = Tm.unhashComponent (Map.fromList itms)
      (subvs, rgrp0, rbkr) = intermediateTerms ppe ctx im
      lubvs r = case Map.lookup r subvs of
        Just r -> r
        Nothing -> error "loadCode: variable missing for float refs"
      vm = Map.mapKeys RF.DerivedId . Map.map (lubvs . fst) $ im
      int b r = if b then r else toIntermed ctx r
      (ctx', _, rgrp) =
        performRehash
          (fmap (overGroupLinks int) rgrp0)
          (floatRemapAdd vm ctx)
  return (backrefAdd rbkr ctx', rgrp ++ odeps)

loadDeps ::
  CodeLookup Symbol IO () ->
  PrettyPrintEnv ->
  EvalCtx ->
  [(Reference, Either [Int] [Int])] ->
  [Reference] ->
  IO (EvalCtx, [(Reference, Code Reference)])
loadDeps cl ppe ctx tyrs tmrs = do
  let cc = ccache ctx
  sand <- readTVarIO (sandbox cc)
  p <-
    refNumsTy cc <&> \m (r, _) -> case r of
      RF.DerivedId {} ->
        r `Map.notMember` dspec ctx
          || r `Map.notMember` m
      _ -> False
  ctx <- foldM (uncurry . allocType) ctx $ Prelude.filter p tyrs
  let tyAdd = Set.fromList $ fst <$> tyrs
  (ctx', rgrp) <- loadCode cl ppe ctx tmrs
  crgrp <- traverse (checkCacheability cl ctx') rgrp
  (ctx', crgrp) <$ cacheAdd0 tyAdd crgrp (expandSandbox sand rgrp) cc

checkCacheability ::
  CodeLookup Symbol IO () ->
  EvalCtx ->
  (IntermediateReference, SuperGroup Reference Symbol) ->
  IO (IntermediateReference, Code Reference)
checkCacheability cl ctx (r, sg) =
  getTermType mayCodebaseRef >>= \case
    -- A term's result is cacheable iff it has no arrows in its type,
    -- this is sufficient since top-level definitions can't have effects without a delay.
    Just typ
      | not (Rec.cata hasArrows typ) ->
          pure (r, CodeRep sg Cacheable)
    _ -> pure (r, CodeRep sg Uncacheable)
  where
    mayCodebaseRef :: Maybe CodebaseReference
    mayCodebaseRef = maybeBackmapRef ctx r
    getTermType :: Maybe CodebaseReference -> IO (Maybe (Type Symbol))
    getTermType = \case
      Just (RF.DerivedId i) ->
        getTypeOfTerm cl i >>= \case
          Just t -> pure $ Just t
          Nothing -> pure Nothing
      Just (RF.Builtin {}) -> pure $ Nothing
      Nothing -> pure Nothing
    hasArrows :: Type.TypeF v a Bool -> Bool
    hasArrows abt = case ABT.out' abt of
      (ABT.Tm f) -> case f of
        Type.Arrow _ _ -> True
        other -> or other
      t -> or t

decompileCtx ::
  EnumMap Word64 Reference -> EvalCtx -> Val -> DecompResult Symbol
decompileCtx crs ctx = decompile ib $ backReferenceTm crs fr ir dt
  where
    ib = intermedToBase ctx
    fr = floatRemap ctx
    ir = intermedRemap ctx
    dt = decompTm ctx

interpEval ::
  ActiveThreads ->
  IO () ->
  IORef EvalCtx ->
  CodeLookup Symbol IO () ->
  PrettyPrintEnv ->
  Term Symbol ->
  IO (Either Error ([Error], Term Symbol))
interpEval activeThreads cleanupThreads ctxVar cl ppe tm =
  catchInternalErrors $ do
    ctx <- readIORef ctxVar
    (tyrs, tmrs) <- collectDeps cl tm
    (ctx, _) <- loadDeps cl ppe ctx tyrs tmrs
    (ctx, _, init) <- prepareEvaluation ppe tm ctx
    initw <- refNumTm (ccache ctx) init
    writeIORef ctxVar ctx
    evalInContext ppe ctx activeThreads initw
      `UnliftIO.finally` cleanupThreads

interpCompile ::
  Text ->
  IORef EvalCtx ->
  CompileOpts ->
  CodeLookup Symbol IO () ->
  PrettyPrintEnv ->
  Reference ->
  FilePath ->
  IO (Maybe Error)
interpCompile version ctxVar _copts cl ppe rf path = tryM $ do
  ctx <- readIORef ctxVar
  (tyrs, tmrs) <- collectRefDeps cl rf
  (ctx, _) <- loadDeps cl ppe ctx tyrs tmrs
  let cc = ccache ctx
      lk m = flip Map.lookup m =<< baseToIntermed ctx rf
  Just w <- lk <$> readTVarIO (refTm cc)
  let combIx = CIx rf w 0
  sto <- standalone cc w
  BL.writeFile path . runPutL $ do
    serialize $ version
    serialize $ RF.showShort 8 rf
    putCombIx combIx
    putStoredCache sto

backrefLifted ::
  Reference ->
  Term Symbol ->
  [(Reference, Term Symbol)] ->
  Map.Map Reference (Map.Map Word64 (Term Symbol))
backrefLifted ref (Tm.Ann' tm _) dcmp = backrefLifted ref tm dcmp
backrefLifted ref tm dcmp =
  Map.fromList . (fmap . fmap) (Map.singleton 0) $ (ref, tm) : dcmp

intermediateTerms ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  EvalCtx ->
  Map RF.Id (Symbol, Term Symbol) ->
  ( Map.Map Symbol Reference,
    Map.Map Reference (SuperGroup Reference Symbol),
    Map.Map Reference (Map.Map Word64 (Term Symbol))
  )
intermediateTerms ppe ctx rtms =
  case normalizeGroup ctx orig (Map.elems rtms) of
    (subvs, cmbs, dcmp) ->
      (subvs, Map.mapWithKey f cmbs, Map.map (Map.singleton 0) dcmp)
      where
        f ref =
          superNormalize
            . splitPatterns (dspec ctx)
            . addDefaultCases tmName
          where
            tmName = HQ.toText . termName ppe $ RF.Ref ref
  where
    orig =
      Map.fromList
        . fmap (\(x, y) -> (y, RF.DerivedId x))
        . Map.toList
        $ Map.map fst rtms

normalizeTerm ::
  EvalCtx ->
  Term Symbol ->
  ( Reference,
    Map Reference Reference,
    Map Reference (Term Symbol),
    Map Reference (Map.Map Word64 (Term Symbol))
  )
normalizeTerm ctx tm =
  absorb
    . lamLift orig
    . saturate (uncurryDspec $ dspec ctx)
    . inlineAlias
    $ tm
  where
    orig
      | Tm.LetRecNamed' bs _ <- tm =
          fmap (RF.DerivedId . fst)
            . Hashing.hashTermComponentsWithoutTypes
            $ Map.fromList bs
      | otherwise = mempty
    absorb (ll, frem, bs, dcmp) =
      let ref = RF.DerivedId $ Hashing.hashClosedTerm ll
       in (ref, frem, Map.fromList $ (ref, ll) : bs, backrefLifted ref tm dcmp)

normalizeGroup ::
  EvalCtx ->
  Map Symbol Reference ->
  [(Symbol, Term Symbol)] ->
  ( Map Symbol Reference,
    Map Reference (Term Symbol),
    Map Reference (Term Symbol)
  )
normalizeGroup ctx orig gr0 = case lamLiftGroup orig gr of
  (subvis, cmbs, dcmp) ->
    let subvs = (fmap . fmap) RF.DerivedId subvis
        subrs = Map.fromList $ mapMaybe f subvs
     in ( Map.fromList subvs,
          Map.fromList $
            (fmap . fmap) (Tm.updateDependencies subrs mempty) cmbs,
          Map.fromList dcmp
        )
  where
    gr = fmap (saturate (uncurryDspec $ dspec ctx) . inlineAlias) <$> gr0
    f (v, r) = (,RF.Ref r) . RF.Ref <$> Map.lookup v orig

intermediateTerm ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  EvalCtx ->
  Term Symbol ->
  ( Reference,
    Map.Map Reference Reference,
    Map.Map Reference (SuperGroup Reference Symbol),
    Map.Map Reference (Map.Map Word64 (Term Symbol))
  )
intermediateTerm ppe ctx tm =
  case normalizeTerm ctx tm of
    (ref, frem, cmbs, dcmp) -> (ref, frem, fmap f cmbs, dcmp)
      where
        tmName = HQ.toText . termName ppe $ RF.Ref ref
        f =
          superNormalize
            . splitPatterns (dspec ctx)
            . addDefaultCases tmName

prepareEvaluation ::
  (HasCallStack) =>
  PrettyPrintEnv ->
  Term Symbol ->
  EvalCtx ->
  IO (EvalCtx, [(Reference, Code Reference)], Reference)
prepareEvaluation ppe tm ctx = do
  missing <- cacheAdd rcode (ccache ctx')
  when (not . null $ missing) . fail $
    reportBug "E029347" $
      "Error in prepareEvaluation, cache is missing: " <> show missing
  pure (backrefAdd rbkr ctx', rcode, rmn)
  where
    uncacheable g = CodeRep g Uncacheable
    (rmn0, frem, rgrp0, rbkr) = intermediateTerm ppe ctx tm
    int b r
      | b || Map.member r rgrp0 = r
      | otherwise = toIntermed ctx r
    (ctx', rrefs, rgrp) =
      performRehash
        ((fmap . overGroupLinks) int $ rgrp0)
        (floatRemapAdd frem ctx)
    rcode = second uncacheable <$> rgrp
    rmn = case Map.lookup rmn0 rrefs of
      Just r -> r
      Nothing -> error "prepareEvaluation: could not remap main ref"

watchHook :: IORef Val -> XStack -> IO ()
watchHook r xstk = peek (packXStack xstk) >>= writeIORef r

backReferenceTm ::
  EnumMap Word64 Reference ->
  Remapping IntermediateReference CodebaseReference ->
  Remapping FloatedReference IntermediateReference ->
  Map.Map CodebaseReference (Map.Map Word64 (Term Symbol)) ->
  Word64 ->
  Word64 ->
  Maybe (Term Symbol)
backReferenceTm ws frs irs dcm c i = do
  r <- EC.lookup c ws
  -- backmap from function replacements
  r <- pure $ Map.findWithDefault r r functionUnreplacements
  -- backmap intermediate ref to floated ref
  r <- Map.lookup r (backmap irs)
  -- backmap floated ref to original ref
  r <- pure $ Map.findWithDefault r r (backmap frs)
  -- look up original ref in decompile info
  bs <- Map.lookup r dcm
  Map.lookup i bs

evalInContext ::
  PrettyPrintEnv ->
  EvalCtx ->
  ActiveThreads ->
  Word64 ->
  IO (Either Error ([Error], Term Symbol))
evalInContext ppe ctx activeThreads w = do
  r <- newIORef (boxedVal BlackHole)
  crs <- readTVarIO (combRefs $ ccache ctx)
  let hook = watchHook r
      decom = decompileCtx crs ctx
      finish = fmap (first listErrors . decom)

      prettyError e
        | Just rte <- fromException e = Just $ prettyRuntimeExn ppe (backmapRef ctx) decom rte
        | Just (Panic msg mval) <- fromException e =
            Just . pure . P.callout panicIcon . P.linesNonEmpty $
              [ P.wrap $
                  "The program halted with a runtime panic:",
                "",
                P.string msg
              ]
                ++ maybe [] (render . decom) mval
        | otherwise = Nothing
        where
          render (errs, tm) =
            ["", P.indentN 2 $ pretty ppe tm, tabulateErrors errs]

      debugText fancy val = case decom val of
        (errs, dv)
          | null errs ->
              SimpleTrace . debugTextFormat fancy $ pretty ppe dv
          | otherwise ->
              MsgTrace
                (debugTextFormat fancy $ tabulateErrors errs)
                (show val)
                (debugTextFormat fancy $ pretty ppe dv)

  result <-
    bitraverse id (const $ readIORef r) <=< tryJust prettyError $
      apply0 (Just hook) ((ccache ctx) {tracer = debugText}) activeThreads w
  pure $ finish result

executeMainComb ::
  CombIx ->
  CCache ->
  IO (Either (Pretty ColorText) ())
executeMainComb init cc = do
  rSection <- resolveSection cc $ Ins (Pack RF.unitRef TT.unitTag ZArgs) $ Call True init init (VArg1 0)
  result <-
    UnliftIO.try . eval0 cc Nothing $ rSection
  case result of
    Left err -> Left <$> formatErr err
    Right () -> pure (Right ())
  where
    formatErr re = do
      crs <- readTVarIO (combRefs cc)
      let ctx = cacheContext cc
          decom =
            decompile
              (intermedToBase ctx)
              ( backReferenceTm
                  crs
                  (floatRemap ctx)
                  (intermedRemap ctx)
                  (decompTm ctx)
              )
      prettyRuntimeExn mempty id decom re

panicIcon :: Pretty ColorText
panicIcon = "💥🤯💥"

catchInternalErrors ::
  IO (Either Error a) ->
  IO (Either Error a)
catchInternalErrors sub = sub `UnliftIO.catch` hCE `UnliftIO.catch` hRE
  where
    hCE = fmap Left . prettyCompileExn
    hRE = fmap Left . prettyRuntimeExnSansCtx

decodeStandalone ::
  BL.ByteString ->
  Either String (Text, Text, CombIx, StoredCache)
decodeStandalone b = bimap thd thd $ runGetOrFail g b
  where
    thd (_, _, x) = x
    g =
      (,,,)
        <$> deserialize
        <*> deserialize
        <*> getCombIx
        <*> getStoredCache

-- | Whether the runtime is hosted within a persistent session or as a one-off process.
-- This affects the amount of clean-up and book-keeping the runtime does.
data RuntimeHost
  = OneOff
  | Persistent

startRuntime :: Bool -> RuntimeHost -> Text -> IO (Runtime Symbol)
startRuntime sandboxed runtimeHost version = do
  ctxVar <- newIORef =<< baseContext sandboxed
  (activeThreads, cleanupThreads) <- case runtimeHost of
    -- Don't bother tracking open threads when running standalone, they'll all be cleaned up
    -- when the process itself exits.
    OneOff -> pure (Nothing, pure ())
    -- Track all forked threads so that they can be killed when the main process returns,
    -- otherwise they'll be orphaned and left running.
    Persistent -> do
      activeThreads <- newIORef Set.empty
      let cleanupThreads = do
            threads <- readIORef activeThreads
            foldMap UnliftIO.killThread threads
      pure (Just activeThreads, cleanupThreads)
  pure $
    Runtime
      { terminate = pure (),
        evaluate = interpEval activeThreads cleanupThreads ctxVar,
        compileTo = interpCompile version ctxVar,
        mainType = builtinMain External,
        ioTestTypes = builtinIOTestTypes External
      }

withRuntime :: (MonadUnliftIO m) => Bool -> RuntimeHost -> Text -> (Runtime Symbol -> m a) -> m a
withRuntime sandboxed runtimeHost version action =
  UnliftIO.bracket (liftIO $ startRuntime sandboxed runtimeHost version) (liftIO . terminate) action

tryM :: IO () -> IO (Maybe Error)
tryM =
  flip UnliftIO.catch hRE
    . flip UnliftIO.catch hCE
    . fmap (const Nothing)
  where
    hCE = fmap Just . prettyCompileExn
    hRE = fmap Just . prettyRuntimeExnSansCtx

runStandalone :: Bool -> StoredCache -> CombIx -> IO (Either (Pretty ColorText) ())
runStandalone sandboxed sc init =
  restoreCache sandboxed sc >>= executeMainComb init

-- | A version of the Code Cache designed to be serialized to disk as
-- standalone bytecode.
data StoredCache
  = SCache
      (EnumMap Word64 Combs)
      (EnumMap Word64 Reference)
      (EnumSet Word64)
      (OptInfos Reference Symbol)
      (EnumMap Word64 Reference)
      Word64
      Word64
      (Map Reference (SuperGroup Reference Symbol))
      (Map Reference Word64)
      (Map Reference Word64)
      (Map Reference (Set Reference))
  deriving (Show, Eq)

putStoredCache :: (MonadPut m) => StoredCache -> m ()
putStoredCache (SCache cs crs cacheableCombs oinfo trs ftm fty int rtm rty sbs) = do
  putEnumMap putNat (putEnumMap putNat (putComb absurd)) cs
  putEnumMap putNat putReference crs
  putEnumSet putNat cacheableCombs
  putOptInfos oinfo
  putEnumMap putNat putReference trs
  putNat ftm
  putNat fty
  putMap putReference (putGroup mempty False) int
  putMap putReference putNat rtm
  putMap putReference putNat rty
  putMap putReference (putFoldable putReference) sbs

getStoredCache :: (MonadGet m) => m StoredCache
getStoredCache =
  SCache
    <$> getEnumMap getNat (getEnumMap getNat getComb)
    <*> getEnumMap getNat getReference
    <*> getEnumSet getNat
    <*> getOptInfos
    <*> getEnumMap getNat getReference
    <*> getNat
    <*> getNat
    <*> getMap getReference getGroupCurrent
    <*> getMap getReference getNat
    <*> getMap getReference getNat
    <*> getMap getReference (fromList <$> getList getReference)

debugTextFormat :: Bool -> Pretty ColorText -> String
debugTextFormat fancy =
  render 50
  where
    render = if fancy then toANSI else toPlain

restoreCache :: Bool -> StoredCache -> IO CCache
restoreCache sandboxed (SCache cs crs cacheableCombs opt trs ftm fty int rtm rty sbs) = do
  cc <-
    CCache sandboxed debugText
      <$> newTVarIO srcCombs
      <*> newTVarIO combs
      <*> newTVarIO (crs <> builtinTermBackref)
      <*> newTVarIO cacheableCombs
      <*> newTVarIO (opt <> builtinOptInfo)
      <*> newTVarIO (trs <> builtinTypeBackref)
      <*> newTVarIO ftm
      <*> newTVarIO fty
      <*> newTVarIO int
      <*> newTVarIO (rtm <> builtinTermNumbering)
      <*> newTVarIO (rty <> builtinTypeNumbering)
      <*> newTVarIO (sbs <> baseSandboxInfo)
  let (unresolvedCacheableCombs, unresolvedNonCacheableCombs) =
        srcCombs
          & sanitizeCombsOfForeignFuncs sandboxed sandboxedForeignFuncs
          & absurdCombs
          & EC.mapToList
          & foldMap
            ( \(k, v) ->
                if k `member` cacheableCombs
                  then (EC.mapSingleton k v, mempty)
                  else (mempty, EC.mapSingleton k v)
            )
  preEvalTopLevelConstants unresolvedCacheableCombs unresolvedNonCacheableCombs cc
  pure cc
  where
    decom =
      decompile
        (const Nothing)
        (backReferenceTm crs mempty mempty mempty)
    debugText fancy c = case decom c of
      (errs, dv)
        | null errs ->
            SimpleTrace . debugTextFormat fancy $ pretty PPE.empty dv
        | otherwise ->
            MsgTrace
              (debugTextFormat fancy $ tabulateErrors errs)
              (show c)
              (debugTextFormat fancy $ pretty PPE.empty dv)
    rns = emptyRNs {dnum = refLookup "ty" builtinTypeNumbering}
    rf k = builtinTermBackref ! k
    srcCombs :: EnumMap Word64 Combs
    srcCombs =
      let builtinCombs = mapWithKey (\k v -> emitComb @Symbol rns (rf k) k mempty (0, v)) numberedTermLookup
       in builtinCombs <> cs
    combs :: EnumMap Word64 (RCombs Val)
    combs =
      srcCombs
        & sanitizeCombsOfForeignFuncs sandboxed sandboxedForeignFuncs
        & absurdCombs
        & resolveCombs Nothing

traceNeeded ::
  Reference ->
  Map Reference (SuperGroup Reference Symbol) ->
  IO (Map Reference (SuperGroup Reference Symbol))
traceNeeded init src = go mempty init
  where
    go acc nx
      | RF.isBuiltin nx = pure acc
      | Map.member nx acc = pure acc
      | Just co <- Map.lookup nx src =
          foldlM go (Map.insert nx co acc) (groupTermLinks co)
      | otherwise =
          die [] $ "traceNeeded: unknown combinator: " ++ show nx

buildSCache ::
  EnumMap Word64 Reference ->
  EnumMap Word64 Combs ->
  EnumSet Word64 ->
  OptInfos Reference Symbol ->
  EnumMap Word64 Reference ->
  Word64 ->
  Word64 ->
  Map Reference (SuperGroup Reference Symbol) ->
  Map Reference Word64 ->
  Map Reference Word64 ->
  Map Reference (Set Reference) ->
  StoredCache
buildSCache crsrc cssrc cacheableCombs optsrc trsrc ftm fty int rtmsrc rtysrc sndbx =
  SCache
    cs
    crs
    cacheableCombs
    opt
    trs
    ftm
    fty
    int
    rtm
    (restrictTyR rtysrc)
    (restrictTmR sndbx)
  where
    termRefs = Map.keysSet int

    -- Retain just the Reference->Word mappings for needed code
    rtm :: Map Reference Word64
    rtm = restrictTmR rtmsrc

    -- Retain numbers that correspond to the above termRefs
    combKeys :: EnumSet Word64
    combKeys = foldMap setSingleton rtm

    crs = restrictTmW crsrc

    cs :: EnumMap Word64 Combs
    cs = restrictTmW cssrc

    opt = bimap restrictTmR restrictTmR optsrc

    typeKeys = setFromList $ (foldMap . foldMap) combTypes cs
    trs = restrictTyW trsrc
    typeRefs = foldMap Set.singleton trs

    restrictTmW :: EnumMap Word64 a -> EnumMap Word64 a
    restrictTmW m = restrictKeys m combKeys
    restrictTmR :: Map Reference a -> Map Reference a
    restrictTmR m = Map.restrictKeys m termRefs

    restrictTyW m = restrictKeys m typeKeys
    restrictTyR m = Map.restrictKeys m typeRefs

standalone :: CCache -> Word64 -> IO StoredCache
standalone cc init =
  readTVarIO (combRefs cc) >>= \crs ->
    case EC.lookup init crs of
      Just rinit ->
        buildSCache crs
          <$> readTVarIO (srcCombs cc)
          <*> readTVarIO (cacheableCombs cc)
          <*> readTVarIO (optInfos cc)
          <*> readTVarIO (tagRefs cc)
          <*> readTVarIO (freshTm cc)
          <*> readTVarIO (freshTy cc)
          <*> (readTVarIO (intermed cc) >>= traceNeeded rinit)
          <*> readTVarIO (refTm cc)
          <*> readTVarIO (refTy cc)
          <*> readTVarIO (sandbox cc)
      Nothing ->
        die [] $ "standalone: unknown combinator: " ++ show init
