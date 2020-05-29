{-# language ScopedTypeVariables #-}

module Unison.Runtime.Interface
  ( startRuntime
  ) where

import Control.Exception (try)
import Control.Monad (foldM)

import Data.Bifunctor (first,second)
import Data.Foldable
import Data.IORef
import Data.Word (Word64)

import qualified Data.Map.Strict as Map

import qualified Unison.Term as Tm
import Unison.Var (Var)

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

type Term v = Tm.Term v ()

data EvalCtx v
  = ECtx
  { freshTy :: Int
  , freshTm :: Word64
  , refTy :: Map.Map RF.Reference RTag
  , refTm :: Map.Map RF.Reference Word64
  , combs :: EnumMap Word64 Comb
  , backrefTy :: EnumMap RTag RF.Reference
  , backrefTm :: EnumMap Word64 (Term v)
  }

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
  , backrefTy = builtinTypeBackref
  , backrefTm = Tm.ref () <$> builtinTermBackref
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
  -> IO (EvalCtx v)
allocType _ b@(RF.Builtin _)
  = die $ "Unknown builtin type reference: " ++ show b
allocType ctx r
  = pure $ ctx
         { refTy = Map.insert r frsh $ refTy ctx
         , backrefTy = mapInsert frsh r $ backrefTy ctx
         , freshTy = freshTy ctx + 1
         }
  where
  frsh = toEnum $ freshTy ctx

collectDeps
  :: Var v
  => CodeLookup v IO ()
  -> Term v
  -> IO ([Reference], [Reference])
collectDeps _  tm
  = pure $ foldr categorize ([],[]) chld
  where
  chld = toList $ Tm.labeledDependencies tm
  categorize = either (first . (:)) (second . (:)) . RF.toReference

loadDeps
  :: Var v
  => CodeLookup v IO ()
  -> EvalCtx v
  -> Term v
  -> IO (EvalCtx v)
loadDeps cl ctx tm = do
  (tys, _  ) <- collectDeps cl tm
  -- TODO: terms
  foldM allocType ctx $ filter (`Map.notMember`refTy ctx) tys

addCombs :: EnumMap Word64 Comb -> EvalCtx v -> EvalCtx v
addCombs m ctx = ctx { combs = m <> combs ctx }

addTermBackrefs :: EnumMap Word64 (Term v) -> EvalCtx v -> EvalCtx v
addTermBackrefs refs ctx = ctx { backrefTm = refs <> backrefTm ctx }

refresh :: Word64 -> EvalCtx v -> EvalCtx v
refresh w ctx = ctx { freshTm = w }

compileTerm
  :: Var v => Word64 -> Term v -> EvalCtx v -> EvalCtx v
compileTerm w tm ctx
  = finish
  . emitCombs frsh
  . superNormalize (refTm ctx Map.!) (refTy ctx Map.!)
  . lamLift
  . splitPatterns
  $ tm
  where
  frsh = freshTm ctx
  recs = numberLetRec frsh tm
  finish (main, aux, frsh')
    = refresh frsh'
    . addTermBackrefs recs
    . addCombs (mapInsert w main aux)
    $ ctx

evalInContext
  :: Var v => EvalCtx v -> Word64 -> IO (Either Error (Term v))
evalInContext ctx w = do
  result <- fmap (first prettyError) . try $ apply0 (combs ctx !) w
  pure $ decompile (`EC.lookup` backrefTy ctx)
                   (`EC.lookup` backrefTm ctx)
           =<< result

startRuntime :: Var v => IO (Runtime v)
startRuntime = do
  ctxVar <- newIORef baseContext
  pure $ Runtime
       { terminate = pure ()
       , evaluate = \cl _ tm -> do
           ctx <- readIORef ctxVar
           ctx <- loadDeps cl ctx tm
           writeIORef ctxVar ctx
           let init = freshTm ctx
           ctx <- pure $ refresh (init+1) ctx
           ctx <- pure $ compileTerm init tm ctx
           evalInContext ctx init
       }
