{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Runtime.Interface
  ( startRuntime,
    standalone,
    runStandalone,
    StoredCache,
    decodeStandalone,
    RuntimeHost (..),
  )
where

import Control.Concurrent.STM as STM
import Control.Exception (catch, try)
import Control.Monad
import Data.Bifunctor (bimap, first)
import Data.Binary.Get (runGetOrFail)
-- import Data.Bits (shiftL)
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut, runPutL)
import Data.Bytes.Serial
import Data.Foldable
import Data.Functor ((<&>))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set as Set
  ( Set,
    filter,
    fromList,
    map,
    notMember,
    singleton,
    (\\),
  )
import qualified Data.Set as Set
import Data.Text (Text, isPrefixOf)
import Data.Traversable (for)
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import qualified Unison.ABT as Tm (substs)
import qualified Unison.Builtin.Decls as RF
import Unison.Codebase.CodeLookup (CodeLookup (..))
import Unison.Codebase.MainTerm (builtinMain, builtinTest)
import Unison.Codebase.Runtime (Error, Runtime (..))
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import qualified Unison.ConstructorReference as RF
import Unison.DataDeclaration (Decl, declDependencies, declFields)
import qualified Unison.HashQualified as HQ
import qualified Unison.Hashing.V2.Convert as Hashing
import qualified Unison.LabeledDependency as RF
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude (reportBug)
import Unison.PrettyPrintEnv
import Unison.Reference (Reference)
import qualified Unison.Reference as RF
import qualified Unison.Referent as RF (pattern Ref)
import Unison.Runtime.ANF
import Unison.Runtime.ANF.Serialize (getGroup, putGroup)
import Unison.Runtime.Builtin
import Unison.Runtime.Decompile
import Unison.Runtime.Exception
import Unison.Runtime.MCode
  ( Args (..),
    Combs,
    Instr (..),
    RefNums (..),
    Section (..),
    combDeps,
    combTypes,
    emitComb,
    emptyRNs,
  )
import Unison.Runtime.MCode.Serialize
import Unison.Runtime.Machine
  ( ActiveThreads,
    CCache (..),
    apply0,
    baseCCache,
    cacheAdd,
    cacheAdd0,
    eval0,
    expandSandbox,
    refLookup,
    refNumTm,
    refNumsTm,
    refNumsTy,
  )
import Unison.Runtime.Pattern
import Unison.Runtime.Serialize as SER
import Unison.Runtime.Stack
import Unison.Symbol (Symbol)
import qualified Unison.Term as Tm
import Unison.TermPrinter
import Unison.Util.EnumContainers as EC
import Unison.Util.Pretty as P
import qualified Unison.Util.Text as UT
import qualified UnliftIO
import qualified UnliftIO.Concurrent as UnliftIO

type Term v = Tm.Term v ()

data EvalCtx = ECtx
  { dspec :: DataSpec,
    decompTm :: Map.Map Reference (Map.Map Word64 (Term Symbol)),
    ccache :: CCache
  }

uncurryDspec :: DataSpec -> Map.Map ConstructorReference Int
uncurryDspec = Map.fromList . concatMap f . Map.toList
  where
    f (r, l) = zipWith (\n c -> (ConstructorReference r n, c)) [0 ..] $ either id id l

cacheContext :: CCache -> EvalCtx
cacheContext =
  ECtx builtinDataSpec
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
  die $ "Unknown builtin term reference: " ++ show b
resolveTermRef cl r@(RF.DerivedId i) =
  getTerm cl i >>= \case
    Nothing -> die $ "Unknown term reference: " ++ show r
    Just tm -> pure tm

allocType ::
  EvalCtx ->
  RF.Reference ->
  Either [Int] [Int] ->
  IO EvalCtx
allocType _ b@(RF.Builtin _) _ =
  die $ "Unknown builtin type reference: " ++ show b
allocType ctx r cons =
  pure $ ctx {dspec = Map.insert r cons $ dspec ctx}

recursiveDeclDeps ::
  Set RF.LabeledDependency ->
  CodeLookup Symbol IO () ->
  Decl Symbol () ->
  IO (Set Reference, Set Reference)
recursiveDeclDeps seen0 cl d = do
  rec <- for (toList newDeps) $ \case
    RF.DerivedId i ->
      getTypeDeclaration cl i >>= \case
        Just d -> recursiveDeclDeps seen cl d
        Nothing -> pure mempty
    _ -> pure mempty
  pure $ (deps, mempty) <> fold rec
  where
    deps = declDependencies d
    newDeps = Set.filter (\r -> notMember (RF.typeRef r) seen0) deps
    seen = seen0 <> Set.map RF.typeRef deps

categorize :: RF.LabeledDependency -> (Set Reference, Set Reference)
categorize =
  \case
    RF.TypeReference ref -> (Set.singleton ref, mempty)
    RF.ConReference (RF.ConstructorReference ref _conId) _conType -> (Set.singleton ref, mempty)
    RF.TermReference ref -> (mempty, Set.singleton ref)

recursiveTermDeps ::
  Set RF.LabeledDependency ->
  CodeLookup Symbol IO () ->
  Term Symbol ->
  IO (Set Reference, Set Reference)
recursiveTermDeps seen0 cl tm = do
  rec <- for (toList (deps \\ seen0)) $ \case
    RF.ConReference (RF.ConstructorReference (RF.DerivedId refId) _conId) _conType -> handleTypeReferenceId refId
    RF.TypeReference (RF.DerivedId refId) -> handleTypeReferenceId refId
    RF.TermReference r -> recursiveRefDeps seen cl r
    _ -> pure mempty
  pure $ foldMap categorize deps <> fold rec
  where
    handleTypeReferenceId :: RF.Id -> IO (Set Reference, Set Reference)
    handleTypeReferenceId refId =
      getTypeDeclaration cl refId >>= \case
        Just d -> recursiveDeclDeps seen cl d
        Nothing -> pure mempty
    deps = Tm.labeledDependencies tm
    seen = seen0 <> deps

recursiveRefDeps ::
  Set RF.LabeledDependency ->
  CodeLookup Symbol IO () ->
  Reference ->
  IO (Set Reference, Set Reference)
recursiveRefDeps seen cl (RF.DerivedId i) =
  getTerm cl i >>= \case
    Just tm -> recursiveTermDeps seen cl tm
    Nothing -> pure mempty
recursiveRefDeps _ _ _ = pure mempty

collectDeps ::
  CodeLookup Symbol IO () ->
  Term Symbol ->
  IO ([(Reference, Either [Int] [Int])], [Reference])
collectDeps cl tm = do
  (tys, tms) <- recursiveTermDeps mempty cl tm
  (,toList tms) <$> traverse getDecl (toList tys)
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

loadDeps ::
  CodeLookup Symbol IO () ->
  PrettyPrintEnv ->
  EvalCtx ->
  [(Reference, Either [Int] [Int])] ->
  [Reference] ->
  IO EvalCtx
loadDeps cl ppe ctx tyrs tmrs = do
  let cc = ccache ctx
  sand <- readTVarIO (sandbox cc)
  p <-
    refNumsTy cc <&> \m (r, _) -> case r of
      RF.DerivedId {} ->
        r `Map.notMember` dspec ctx
          || r `Map.notMember` m
      _ -> False
  q <-
    refNumsTm (ccache ctx) <&> \m r -> case r of
      RF.DerivedId {} -> r `Map.notMember` m
      _ -> False
  ctx <- foldM (uncurry . allocType) ctx $ Prelude.filter p tyrs
  rtms <-
    traverse (\r -> (,) r <$> resolveTermRef cl r) $
      Prelude.filter q tmrs
  let (rgrp0, rbkr) = intermediateTerms ppe ctx rtms
      rgrp = Map.toList rgrp0
      tyAdd = Set.fromList $ fst <$> tyrs
  backrefAdd rbkr ctx
    <$ cacheAdd0 tyAdd rgrp (expandSandbox sand rgrp) cc

backrefLifted ::
  Reference ->
  Term Symbol ->
  [(Reference, Term Symbol)] ->
  Map.Map Reference (Map.Map Word64 (Term Symbol))
backrefLifted ref (Tm.Ann' tm _) dcmp = backrefLifted ref tm dcmp
backrefLifted ref tm dcmp =
  Map.fromList . (fmap . fmap) (Map.singleton 0) $ (ref, tm) : dcmp

intermediateTerms ::
  HasCallStack =>
  PrettyPrintEnv ->
  EvalCtx ->
  [(Reference, Term Symbol)] ->
  ( Map.Map Reference (SuperGroup Symbol),
    Map.Map Reference (Map.Map Word64 (Term Symbol))
  )
intermediateTerms ppe ctx rtms =
  foldMap (\(ref, tm) -> intermediateTerm ppe ref ctx tm) rtms

intermediateTerm ::
  HasCallStack =>
  PrettyPrintEnv ->
  Reference ->
  EvalCtx ->
  Term Symbol ->
  ( Map.Map Reference (SuperGroup Symbol),
    Map.Map Reference (Map.Map Word64 (Term Symbol))
  )
intermediateTerm ppe ref ctx tm =
  (first . fmap)
    ( superNormalize
        . splitPatterns (dspec ctx)
        . addDefaultCases tmName
    )
    . memorize
    . lamLift
    . saturate (uncurryDspec $ dspec ctx)
    . inlineAlias
    $ tm
  where
    memorize (ll, ctx, dcmp) =
      ( Map.fromList $ (ref, ll) : ctx,
        backrefLifted ref tm dcmp
      )
    tmName = HQ.toString . termName ppe $ RF.Ref ref

prepareEvaluation ::
  HasCallStack =>
  PrettyPrintEnv ->
  Term Symbol ->
  EvalCtx ->
  IO (EvalCtx, Word64)
prepareEvaluation ppe tm ctx = do
  missing <- cacheAdd rgrp (ccache ctx)
  when (not . null $ missing) . fail $
    reportBug "E029347" $ "Error in prepareEvaluation, cache is missing: " <> show missing
  (,) (backrefAdd rbkr ctx) <$> refNumTm (ccache ctx) rmn
  where
    (rmn, rtms)
      | Tm.LetRecNamed' bs mn0 <- tm,
        hcs <-
          fmap (first RF.DerivedId)
            . Hashing.hashTermComponentsWithoutTypes
            $ Map.fromList bs,
        mn <- Tm.substs (Map.toList $ Tm.ref () . fst <$> hcs) mn0,
        rmn <- RF.DerivedId $ Hashing.hashClosedTerm mn =
          (rmn, (rmn, mn) : Map.elems hcs)
      | rmn <- RF.DerivedId $ Hashing.hashClosedTerm tm =
          (rmn, [(rmn, tm)])

    (rgrp0, rbkr) = intermediateTerms ppe ctx rtms
    rgrp = Map.toList rgrp0

watchHook :: IORef Closure -> Stack 'UN -> Stack 'BX -> IO ()
watchHook r _ bstk = peek bstk >>= writeIORef r

backReferenceTm ::
  EnumMap Word64 Reference ->
  Map.Map Reference (Map.Map Word64 (Term Symbol)) ->
  Word64 ->
  Word64 ->
  Maybe (Term Symbol)
backReferenceTm ws rs c i = do
  r <- EC.lookup c ws
  bs <- Map.lookup r rs
  Map.lookup i bs

evalInContext ::
  PrettyPrintEnv ->
  EvalCtx ->
  ActiveThreads ->
  Word64 ->
  IO (Either Error (Term Symbol))
evalInContext ppe ctx activeThreads w = do
  r <- newIORef BlackHole
  crs <- readTVarIO (combRefs $ ccache ctx)
  let hook = watchHook r
      decom = decompile (backReferenceTm crs (decompTm ctx))

      prettyError (PE _ p) = p
      prettyError (BU nm c) = either id (bugMsg ppe nm) $ decom c

      tr tx c = case decom c of
        Right dv -> do
          putStrLn $ "trace: " ++ UT.unpack tx
          putStrLn . toANSI 50 $ pretty ppe dv
        Left _ -> do
          putStrLn $ "trace: " ++ UT.unpack tx
          putStrLn "Couldn't decompile value."
          print c

  result <-
    traverse (const $ readIORef r)
      . first prettyError
      <=< try
      $ apply0 (Just hook) ((ccache ctx) {tracer = tr}) activeThreads w
  pure $ decom =<< result

executeMainComb ::
  Word64 ->
  CCache ->
  IO ()
executeMainComb init cc =
  flip catch (putStrLn . toANSI 50 <=< handler)
    . eval0 cc Nothing
    . Ins (Pack RF.unitRef 0 ZArgs)
    $ Call True init (BArg1 0)
  where
    handler (PE _ msg) = pure msg
    handler (BU nm c) = do
      crs <- readTVarIO (combRefs cc)
      let decom = decompile (backReferenceTm crs (decompTm $ cacheContext cc))
      pure . either id (bugMsg mempty nm) $ decom c

bugMsg :: PrettyPrintEnv -> Text -> Term Symbol -> Pretty ColorText
bugMsg ppe name tm
  | name == "blank expression" =
      P.callout icon . P.lines $
        [ P.wrap
            ( "I encountered a" <> P.red (P.text name)
                <> "with the following name/message:"
            ),
          "",
          P.indentN 2 $ pretty ppe tm,
          "",
          sorryMsg
        ]
  | "pattern match failure" `isPrefixOf` name =
      P.callout icon . P.lines $
        [ P.wrap
            ( "I've encountered a" <> P.red (P.text name)
                <> "while scrutinizing:"
            ),
          "",
          P.indentN 2 $ pretty ppe tm,
          "",
          "This happens when calling a function that doesn't handle all \
          \possible inputs",
          sorryMsg
        ]
  | name == "builtin.raise" =
      P.callout icon . P.lines $
        [ P.wrap ("The program halted with an unhandled exception:"),
          "",
          P.indentN 2 $ pretty ppe tm
        ]
  | name == "builtin.bug",
    RF.TupleTerm' [Tm.Text' msg, x] <- tm,
    "pattern match failure" `isPrefixOf` msg =
      P.callout icon . P.lines $
        [ P.wrap
            ( "I've encountered a" <> P.red (P.text msg)
                <> "while scrutinizing:"
            ),
          "",
          P.indentN 2 $ pretty ppe x,
          "",
          "This happens when calling a function that doesn't handle all \
          \possible inputs",
          sorryMsg
        ]
bugMsg ppe name tm =
  P.callout icon . P.lines $
    [ P.wrap
        ( "I've encountered a call to" <> P.red (P.text name)
            <> "with the following value:"
        ),
      "",
      P.indentN 2 $ pretty ppe tm,
      "",
      sorryMsg
    ]
  where

icon, sorryMsg :: Pretty ColorText
icon = "💔💥"
sorryMsg =
  P.wrap $
    "I'm sorry this message doesn't have more detail about"
      <> "the location of the failure."
      <> "My makers plan to fix this in a future release. 😢"

catchInternalErrors ::
  IO (Either Error a) ->
  IO (Either Error a)
catchInternalErrors sub = sub `catch` \(CE _ e) -> pure $ Left e

decodeStandalone ::
  BL.ByteString ->
  Either String (Text, Text, Word64, StoredCache)
decodeStandalone b = bimap thd thd $ runGetOrFail g b
  where
    thd (_, _, x) = x
    g =
      (,,,)
        <$> deserialize
        <*> deserialize
        <*> getNat
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
        evaluate = \cl ppe tm -> catchInternalErrors $ do
          ctx <- readIORef ctxVar
          (tyrs, tmrs) <- collectDeps cl tm
          ctx <- loadDeps cl ppe ctx tyrs tmrs
          (ctx, init) <- prepareEvaluation ppe tm ctx
          writeIORef ctxVar ctx
          evalInContext ppe ctx activeThreads init `UnliftIO.finally` cleanupThreads,
        compileTo = \cl ppe rf path -> tryM $ do
          ctx <- readIORef ctxVar
          (tyrs, tmrs) <- collectRefDeps cl rf
          ctx <- loadDeps cl ppe ctx tyrs tmrs
          let cc = ccache ctx
          Just w <- Map.lookup rf <$> readTVarIO (refTm cc)
          sto <- standalone cc w
          BL.writeFile path . runPutL $ do
            serialize $ version
            serialize $ RF.showShort 8 rf
            putNat w
            putStoredCache sto,
        mainType = builtinMain External,
        ioTestType = builtinTest External
      }

tryM :: IO () -> IO (Maybe Error)
tryM = fmap (either (Just . extract) (const Nothing)) . try
  where
    extract (PE _ e) = e
    extract (BU _ _) = "impossible"

runStandalone :: StoredCache -> Word64 -> IO ()
runStandalone sc init =
  restoreCache sc >>= executeMainComb init

data StoredCache
  = SCache
      (EnumMap Word64 Combs)
      (EnumMap Word64 Reference)
      (EnumMap Word64 Reference)
      Word64
      Word64
      (Map Reference (SuperGroup Symbol))
      (Map Reference Word64)
      (Map Reference Word64)
      (Map Reference (Set Reference))
  deriving (Show)

putStoredCache :: MonadPut m => StoredCache -> m ()
putStoredCache (SCache cs crs trs ftm fty int rtm rty sbs) = do
  putEnumMap putNat (putEnumMap putNat putComb) cs
  putEnumMap putNat putReference crs
  putEnumMap putNat putReference trs
  putNat ftm
  putNat fty
  putMap putReference (putGroup mempty) int
  putMap putReference putNat rtm
  putMap putReference putNat rty
  putMap putReference (putFoldable putReference) sbs

getStoredCache :: MonadGet m => m StoredCache
getStoredCache =
  SCache
    <$> getEnumMap getNat (getEnumMap getNat getComb)
    <*> getEnumMap getNat getReference
    <*> getEnumMap getNat getReference
    <*> getNat
    <*> getNat
    <*> getMap getReference getGroup
    <*> getMap getReference getNat
    <*> getMap getReference getNat
    <*> getMap getReference (fromList <$> getList getReference)

restoreCache :: StoredCache -> IO CCache
restoreCache (SCache cs crs trs ftm fty int rtm rty sbs) =
  CCache builtinForeigns False uglyTrace
    <$> newTVarIO (cs <> combs)
    <*> newTVarIO (crs <> builtinTermBackref)
    <*> newTVarIO (trs <> builtinTypeBackref)
    <*> newTVarIO ftm
    <*> newTVarIO fty
    <*> newTVarIO int
    <*> newTVarIO (rtm <> builtinTermNumbering)
    <*> newTVarIO (rty <> builtinTypeNumbering)
    <*> newTVarIO (sbs <> baseSandboxInfo)
  where
    uglyTrace tx c = do
      putStrLn $ "trace: " ++ UT.unpack tx
      print c
    rns = emptyRNs {dnum = refLookup "ty" builtinTypeNumbering}
    combs =
      mapWithKey
        (\k v -> emitComb @Symbol rns k mempty (0, v))
        numberedTermLookup

traceNeeded ::
  Word64 ->
  EnumMap Word64 Combs ->
  IO (EnumMap Word64 Combs)
traceNeeded init src = fmap (`withoutKeys` ks) $ go mempty init
  where
    ks = keysSet numberedTermLookup
    go acc w
      | hasKey w acc = pure acc
      | Just co <- EC.lookup w src =
          foldlM go (mapInsert w co acc) (foldMap combDeps co)
      | otherwise = die $ "traceNeeded: unknown combinator: " ++ show w

buildSCache ::
  EnumMap Word64 Combs ->
  EnumMap Word64 Reference ->
  EnumMap Word64 Reference ->
  Word64 ->
  Word64 ->
  Map Reference (SuperGroup Symbol) ->
  Map Reference Word64 ->
  Map Reference Word64 ->
  Map Reference (Set Reference) ->
  StoredCache
buildSCache cs crsrc trsrc ftm fty intsrc rtmsrc rtysrc sndbx =
  SCache
    cs
    crs
    trs
    ftm
    fty
    (restrictTmR intsrc)
    (restrictTmR rtmsrc)
    (restrictTyR rtysrc)
    (restrictTmR sndbx)
  where
    combKeys = keysSet cs
    crs = restrictTmW crsrc
    termRefs = foldMap Set.singleton crs

    typeKeys = setFromList $ (foldMap . foldMap) combTypes cs
    trs = restrictTyW trsrc
    typeRefs = foldMap Set.singleton trs

    restrictTmW m = restrictKeys m combKeys
    restrictTmR :: Map Reference a -> Map Reference a
    restrictTmR m = Map.restrictKeys m termRefs

    restrictTyW m = restrictKeys m typeKeys
    restrictTyR m = Map.restrictKeys m typeRefs

standalone :: CCache -> Word64 -> IO StoredCache
standalone cc init =
  buildSCache
    <$> (readTVarIO (combs cc) >>= traceNeeded init)
    <*> readTVarIO (combRefs cc)
    <*> readTVarIO (tagRefs cc)
    <*> readTVarIO (freshTm cc)
    <*> readTVarIO (freshTy cc)
    <*> readTVarIO (intermed cc)
    <*> readTVarIO (refTm cc)
    <*> readTVarIO (refTy cc)
    <*> readTVarIO (sandbox cc)
