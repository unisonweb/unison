{-# language DataKinds #-}
{-# language PatternGuards #-}
{-# language ScopedTypeVariables #-}

module Unison.Runtime.Interface
  ( startRuntime
  ) where

import Control.Exception (try)
import Control.Monad (foldM, (<=<))

import Data.Bifunctor (first,second)
import Data.Foldable
import Data.IORef
import Data.Word (Word64)

import qualified Data.Map.Strict as Map

import qualified Unison.Term as Tm
import Unison.Var (Var)

import Unison.DataDeclaration (declFields)
import qualified Unison.LabeledDependency as RF
import Unison.Reference (Reference)
import qualified Unison.Reference as RF

import Unison.Util.EnumContainers as EC

import Unison.Codebase.CodeLookup (CodeLookup(..))
import Unison.Codebase.Runtime (Runtime(..), Error)

import Unison.Runtime.ANF
import Unison.Runtime.Builtin
import Unison.Runtime.Decompile
import Unison.Runtime.Machine
import Unison.Runtime.MCode
import Unison.Runtime.Pattern
import Unison.Runtime.Stack

type Term v = Tm.Term v ()

data EvalCtx v
  = ECtx
  { freshTy :: Int
  , freshTm :: Word64
  , refTy :: Map.Map RF.Reference RTag
  , refTm :: Map.Map RF.Reference Word64
  , combs :: EnumMap Word64 Comb
  , dspec :: DataSpec
  , backrefTy :: EnumMap RTag RF.Reference
  , backrefTm :: EnumMap Word64 (Term v)
  , backrefComb :: EnumMap Word64 RF.Reference
  }

uncurryDspec :: DataSpec -> Map.Map (Reference,Int) Int
uncurryDspec = Map.fromList . concatMap f . Map.toList
  where
  f (r,l) = zipWith (\n c -> ((r,n),c)) [0..] $ either id id l

numberLetRec :: Word64 -> Term v -> EnumMap Word64 (Term v)
numberLetRec frsh (Tm.LetRecNamed' bs e)
  = mapFromList . zip [frsh..] $ e : map snd bs
numberLetRec _ _ = error "impossible"

baseContext :: forall v. Var v => EvalCtx v
baseContext
  = ECtx
  { freshTy = fty
  , freshTm = ftm
  , refTy = builtinTypeNumbering
  , refTm = builtinTermNumbering
  , combs = emitComb @v mempty <$> numberedTermLookup
  , dspec = builtinDataSpec
  , backrefTy = builtinTypeBackref
  , backrefTm = Tm.ref () <$> builtinTermBackref
  , backrefComb = builtinTermBackref
  }
  where
  ftm = 1 + maximum builtinTermNumbering
  fty = (1+) . fromEnum $ maximum builtinTypeNumbering

-- allocTerm
--   :: Var v
--   => CodeLookup v m ()
--   -> EvalCtx v
--   -> RF.Reference
--   -> IO (EvalCtx v)
-- allocTerm _  _   b@(RF.Builtin _)
--   = die $ "Unknown builtin term reference: " ++ show b
-- allocTerm _  _   (RF.DerivedId _)
--   = die $ "TODO: allocTerm: hash reference"

allocType
  :: EvalCtx v
  -> RF.Reference
  -> Either [Int] [Int]
  -> IO (EvalCtx v)
allocType _ b@(RF.Builtin _) _
  = die $ "Unknown builtin type reference: " ++ show b
allocType ctx r cons
  = pure $ ctx
         { refTy = Map.insert r frsh $ refTy ctx
         , backrefTy = mapInsert frsh r $ backrefTy ctx
         , dspec = Map.insert r cons $ dspec ctx
         , freshTy = freshTy ctx + 1
         }
  where
  frsh = toEnum $ freshTy ctx

collectDeps
  :: Var v
  => CodeLookup v IO ()
  -> Term v
  -> IO ([(Reference, Either [Int] [Int])], [Reference])
collectDeps cl tm
  = (,tms) <$> traverse getDecl tys
  where
  chld = toList $ Tm.labeledDependencies tm
  categorize = either (first . (:)) (second . (:)) . RF.toReference
  (tys, tms) = foldr categorize ([],[]) chld
  getDecl ty@(RF.DerivedId i) =
    (ty,) . maybe (Right []) declFields
      <$> getTypeDeclaration cl i
  getDecl r = pure (r,Right [])

loadDeps
  :: Var v
  => CodeLookup v IO ()
  -> EvalCtx v
  -> Term v
  -> IO (EvalCtx v)
loadDeps cl ctx tm = do
  (tys, _  ) <- collectDeps cl tm
  -- TODO: terms
  foldM (uncurry . allocType) ctx
    $ filter (\(r,_) -> r `Map.notMember` refTy ctx) tys

addCombs :: EnumMap Word64 Comb -> EvalCtx v -> EvalCtx v
addCombs m ctx = ctx { combs = m <> combs ctx }

addTermBackrefs :: EnumMap Word64 (Term v) -> EvalCtx v -> EvalCtx v
addTermBackrefs refs ctx = ctx { backrefTm = refs <> backrefTm ctx }

refresh :: Word64 -> EvalCtx v -> EvalCtx v
refresh w ctx = ctx { freshTm = w }

ref :: Ord k => Show k => Map.Map k v -> k -> v
ref m k
  | Just x <- Map.lookup k m = x
  | otherwise = error $ "unknown reference: " ++ show k

compileTerm
  :: Var v => Word64 -> Term v -> EvalCtx v -> EvalCtx v
compileTerm w tm ctx
  = finish
  . fmap
      ( emitCombs frsh
      . superNormalize (ref $ refTm ctx) (ref $ refTy ctx))
  . bkrf
  . lamLift
  . splitPatterns (dspec ctx)
  . saturate (uncurryDspec $ dspec ctx)
  $ tm
  where
  frsh = freshTm ctx
  bkrf tm = (numberLetRec frsh tm, tm)
  finish (recs, (main, aux, frsh'))
    = refresh frsh'
    . addTermBackrefs recs
    . addCombs (mapInsert w main aux)
    $ ctx

watchHook :: IORef Closure -> Stack 'UN -> Stack 'BX -> IO ()
watchHook r _ bstk = peek bstk >>= writeIORef r

combEnv :: EvalCtx v -> Word64 -> Comb
combEnv ctx w
  | Just c <- EC.lookup w (combs ctx) = c
  | otherwise = error $ "bad combinator reference: " ++ show w

evalInContext
  :: Var v
  => PrettyPrintEnv
  -> EvalCtx v
  -> Word64
  -> IO (Either Error (Term v))
evalInContext ppe ctx w = do
  r <- newIORef BlackHole
  let hook = watchHook r
      renv = Refs (backrefTy ctx) (backrefComb ctx)
  result <- traverse (const $ readIORef r)
          . first prettyError
        <=< try $ apply0 (Just hook) renv (combEnv ctx) w
  pure $ decom =<< result
  where
  decom = decompile (`EC.lookup`backrefTy ctx) (`EC.lookup`backrefTm ctx)
  prettyError (PE p) = p
  prettyError (BU c) = either id (pretty ppe) $ decom c

startRuntime :: Var v => IO (Runtime v)
startRuntime = do
  ctxVar <- newIORef baseContext
  pure $ Runtime
       { terminate = pure ()
       , evaluate = \cl ppe tm -> do
           ctx <- readIORef ctxVar
           ctx <- loadDeps cl ctx tm
           writeIORef ctxVar ctx
           let init = freshTm ctx
           ctx <- pure $ refresh (init+1) ctx
           ctx <- pure $ compileTerm init tm ctx
           evalInContext ppe ctx init
       }
