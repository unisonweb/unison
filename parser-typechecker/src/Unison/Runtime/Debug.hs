
module Unison.Runtime.Debug
  ( traceComb
  , traceCombs
  , tracePretty
  , tracePrettyGroup
  ) where

import Data.Word

import qualified Unison.Term as Tm
import Unison.Var (Var)
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.TermPrinter (pretty)
import Unison.Util.Pretty (toANSI)
import Unison.Util.EnumContainers

import Unison.Runtime.ANF
import Unison.Runtime.MCode

import Debug.Trace

type Term v = Tm.Term v ()

traceComb :: Bool -> Word64 -> Comb -> Bool
traceComb False _ _ = True
traceComb True  w c = trace (prettyComb w c "\n") True

traceCombs
  :: Bool
  -> (Comb, EnumMap Word64 Comb, Word64)
  -> (Comb, EnumMap Word64 Comb, Word64)
traceCombs False c = c
traceCombs True  c = trace (prettyCombs c "") c

tracePretty
  :: Var v
  => PrettyPrintEnv
  -> Bool
  -> Term v
  -> Term v
tracePretty _ False tm = tm
tracePretty ppe True tm = trace (toANSI 50 $ pretty ppe tm) tm

tracePrettyGroup
  :: Var v
  => Bool
  -> SuperGroup v
  -> SuperGroup v
tracePrettyGroup False g = g
tracePrettyGroup True g = trace (prettyGroup g "") g

