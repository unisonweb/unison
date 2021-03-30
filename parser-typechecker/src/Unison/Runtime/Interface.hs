{-# language DataKinds #-}
{-# language PatternGuards #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Unison.Runtime.Interface
  ( startRuntime
  ) where

import GHC.Stack (HasCallStack)

import Unison.Prelude (reportBug)
import Control.Concurrent.STM as STM
import Control.Exception (try)
import Control.Monad

import Data.Bifunctor (first,second)
import Data.Functor ((<&>))
import Data.IORef
import Data.Foldable
import Data.Set as Set
  (Set, (\\), singleton, map, notMember, filter, fromList)
import Data.Traversable (for)
import Data.Text (Text)
import Data.Word (Word64)

import qualified Data.Map.Strict as Map

import qualified Unison.ABT as Tm (substs)
import qualified Unison.Term as Tm

import Unison.DataDeclaration (declFields, declDependencies, Decl)
import qualified Unison.LabeledDependency as RF
import Unison.Reference (Reference)
import qualified Unison.Reference as RF

import Unison.Util.EnumContainers as EC

import Unison.Codebase.CodeLookup (CodeLookup(..))
import Unison.Codebase.Runtime (Runtime(..), Error)
import Unison.Codebase.MainTerm (builtinMain, builtinTest)

import Unison.Parser (Ann(External))
import Unison.PrettyPrintEnv
import Unison.Util.Pretty as P
import Unison.Symbol (Symbol)
import Unison.TermPrinter

import Unison.Runtime.ANF
import Unison.Runtime.Builtin
import Unison.Runtime.Decompile
import Unison.Runtime.Exception
import Unison.Runtime.Machine
  ( apply0
  , CCache(..), cacheAdd, cacheAdd0, baseCCache
  , refNumTm, refNumsTm, refNumsTy
  )
import Unison.Runtime.Pattern
import Unison.Runtime.Stack

type Term v = Tm.Term v ()

data EvalCtx
  = ECtx
  { dspec :: DataSpec
  , decompTm :: Map.Map Reference (Term Symbol)
  , ccache :: CCache
  }

uncurryDspec :: DataSpec -> Map.Map (Reference,Int) Int
uncurryDspec = Map.fromList . concatMap f . Map.toList
  where
  f (r,l) = zipWith (\n c -> ((r,n),c)) [0..] $ either id id l

baseContext :: IO EvalCtx
baseContext = baseCCache <&> \cc
    -> ECtx
     { dspec = builtinDataSpec
     , decompTm = Map.fromList $
         Map.keys builtinTermNumbering
           <&> \r -> (r, Tm.ref () r)
     , ccache = cc
     }

resolveTermRef
  :: CodeLookup Symbol IO ()
  -> RF.Reference
  -> IO (Term Symbol)
resolveTermRef _  b@(RF.Builtin _)
  = die $ "Unknown builtin term reference: " ++ show b
resolveTermRef cl r@(RF.DerivedId i)
  = getTerm cl i >>= \case
      Nothing -> die $ "Unknown term reference: " ++ show r
      Just tm -> pure tm

allocType
  :: EvalCtx
  -> RF.Reference
  -> Either [Int] [Int]
  -> IO EvalCtx
allocType _ b@(RF.Builtin _) _
  = die $ "Unknown builtin type reference: " ++ show b
allocType ctx r cons
  = pure $ ctx { dspec = Map.insert r cons $ dspec ctx }

recursiveDeclDeps
  :: Set RF.LabeledDependency
  -> CodeLookup Symbol IO ()
  -> Decl Symbol ()
  -> IO (Set Reference, Set Reference)
recursiveDeclDeps seen0 cl d = do
    rec <- for (toList newDeps) $ \case
      RF.DerivedId i -> getTypeDeclaration cl i >>= \case
        Just d -> recursiveDeclDeps seen cl d
        Nothing -> pure mempty
      _ -> pure mempty
    pure $ (deps, mempty) <> fold rec
  where
  deps = declDependencies d
  newDeps = Set.filter (\r -> notMember (RF.typeRef r) seen0) deps
  seen = seen0 <> Set.map RF.typeRef deps

categorize :: RF.LabeledDependency -> (Set Reference, Set Reference)
categorize
  = either ((,mempty) . singleton) ((mempty,) . singleton)
  . RF.toReference

recursiveTermDeps
  :: Set RF.LabeledDependency
  -> CodeLookup Symbol IO ()
  -> Term Symbol
  -> IO (Set Reference, Set Reference)
recursiveTermDeps seen0 cl tm = do
    rec <- for (RF.toReference <$> toList (deps \\ seen0)) $ \case
      Left (RF.DerivedId i) -> getTypeDeclaration cl i >>= \case
        Just d -> recursiveDeclDeps seen cl d
        Nothing -> pure mempty
      Right (RF.DerivedId i) -> getTerm cl i >>= \case
        Just tm -> recursiveTermDeps seen cl tm
        Nothing -> pure mempty
      _ -> pure mempty
    pure $ foldMap categorize deps <> fold rec
  where
  deps = Tm.labeledDependencies tm
  seen = seen0 <> deps

collectDeps
  :: CodeLookup Symbol IO ()
  -> Term Symbol
  -> IO ([(Reference, Either [Int] [Int])], [Reference])
collectDeps cl tm = do
  (tys, tms) <- recursiveTermDeps mempty cl tm
  (, toList tms) <$> traverse getDecl (toList tys)
  where
  getDecl ty@(RF.DerivedId i) =
    (ty,) . maybe (Right []) declFields
      <$> getTypeDeclaration cl i
  getDecl r = pure (r,Right [])

loadDeps
  :: CodeLookup Symbol IO ()
  -> EvalCtx
  -> Term Symbol
  -> IO EvalCtx
loadDeps cl ctx tm = do
  (tyrs, tmrs) <- collectDeps cl tm
  p <- refNumsTy (ccache ctx) <&> \m (r,_) -> case r of
    RF.DerivedId{} -> r `Map.notMember` dspec ctx
                   || r `Map.notMember` m
    _ -> False
  q <- refNumsTm (ccache ctx) <&> \m r -> case r of
    RF.DerivedId{} -> r `Map.notMember` m
    _ -> False
  ctx <- foldM (uncurry . allocType) ctx $ Prelude.filter p tyrs
  rtms <- traverse (\r -> (,) r <$> resolveTermRef cl r)
            $ Prelude.filter q tmrs
  ctx <- pure $ ctx { decompTm = Map.fromList rtms <> decompTm ctx }
  let rint = second (intermediateTerm ctx) <$> rtms
      tyAdd = Set.fromList $ fst <$> tyrs
  cacheAdd0 tyAdd rint (ccache ctx)
  pure ctx

intermediateTerm
  :: HasCallStack => EvalCtx -> Term Symbol -> SuperGroup Symbol
intermediateTerm ctx tm
  = superNormalize
  . lamLift
  . splitPatterns (dspec ctx)
  . saturate (uncurryDspec $ dspec ctx)
  $ tm

prepareEvaluation
  :: HasCallStack => Term Symbol -> EvalCtx -> IO (EvalCtx, Word64)
prepareEvaluation tm ctx = do
  ctx <- pure $ ctx { decompTm = Map.fromList rtms <> decompTm ctx }
  missing <- cacheAdd rint (ccache ctx)
  when (not . null $ missing) . fail $
    reportBug "E029347" $ "Error in prepareEvaluation, cache is missing: " <> show missing
  (,) ctx <$> refNumTm (ccache ctx) rmn
  where
  (rmn, rtms)
    | Tm.LetRecNamed' bs mn0 <- tm
    , hcs <- fmap (first RF.DerivedId)
           . Tm.hashComponents $ Map.fromList bs
    , mn <- Tm.substs (Map.toList $ Tm.ref () . fst <$> hcs) mn0
    , rmn <- RF.DerivedId $ Tm.hashClosedTerm mn
    = (rmn , (rmn, mn) : Map.elems hcs)

    | rmn <- RF.DerivedId $ Tm.hashClosedTerm tm
    = (rmn, [(rmn, tm)])

  rint = second (intermediateTerm ctx) <$> rtms

watchHook :: IORef Closure -> Stack 'UN -> Stack 'BX -> IO ()
watchHook r _ bstk = peek bstk >>= writeIORef r

backReferenceTm
  :: EnumMap Word64 Reference
  -> Map.Map Reference (Term Symbol)
  -> Word64 -> Maybe (Term Symbol)
backReferenceTm ws rs = (`Map.lookup` rs) <=< (`EC.lookup` ws)

evalInContext
  :: PrettyPrintEnv
  -> EvalCtx
  -> Word64
  -> IO (Either Error (Term Symbol))
evalInContext ppe ctx w = do
  r <- newIORef BlackHole
  crs <- readTVarIO (combRefs $ ccache ctx)
  let hook = watchHook r
      decom = decompile (backReferenceTm crs (decompTm ctx))
      prettyError (PE p) = p
      prettyError (BU nm c) = either id (bugMsg ppe nm) $ decom c
  result <- traverse (const $ readIORef r)
          . first prettyError
        <=< try $ apply0 (Just hook) (ccache ctx) w
  pure $ decom =<< result

bugMsg :: PrettyPrintEnv -> Text -> Term Symbol -> Pretty ColorText
bugMsg ppe name tm = P.callout icon . P.lines $
  [ P.wrap ("I've encountered a call to" <> P.red (P.text name)
      <> "with the following value:")
  , ""
  , P.indentN 2 $ pretty ppe tm
  , ""
  , sorryMsg
  ]
  where
icon, sorryMsg  :: Pretty ColorText
icon = "💔💥"
sorryMsg
  = P.wrap
  $ "I'm sorry this message doesn't have more detail about"
  <> "the location of the failure."
  <> "My makers plan to fix this in a future release. 😢"

startRuntime :: IO (Runtime Symbol)
startRuntime = do
  ctxVar <- newIORef =<< baseContext
  pure $ Runtime
       { terminate = pure ()
       , evaluate = \cl ppe tm -> do
           ctx <- readIORef ctxVar
           ctx <- loadDeps cl ctx tm
           writeIORef ctxVar ctx
           (ctx, init) <- prepareEvaluation tm ctx
           evalInContext ppe ctx init
       , mainType = builtinMain External
       , ioTestType = builtinTest External
       , needsContainment = False
       }
