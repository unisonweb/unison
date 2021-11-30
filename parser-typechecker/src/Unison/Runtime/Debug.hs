{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted

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
traceComb True  w c = trace (prettyComb w 0 c "\n") True

traceCombs
  :: Word64
  -> Bool
  -> EnumMap Word64 Comb
  -> EnumMap Word64 Comb
traceCombs _ False c = c
traceCombs w True  c = trace (prettyCombs w c "") c

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
  => Word64
  -> Bool
  -> SuperGroup v
  -> SuperGroup v
tracePrettyGroup _ False g = g
tracePrettyGroup w True g = trace (prettyGroup (show w) g "") g

