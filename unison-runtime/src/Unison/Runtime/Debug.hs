module Unison.Runtime.Debug
  ( traceComb,
    traceCombs,
    tracePretty,
    tracePrettyCodes,
    tracePrettyGroup,
    tracePrettyNormal,
    module Debug.Trace,
  )
where

import Data.Word
import Debug.Trace
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Reference (Reference, toShortHash)
import Unison.Runtime.ANF
import Unison.Runtime.MCode
import Unison.ShortHash (shortenTo)
import Unison.Syntax.NamePrinter (prettyShortHash)
import Unison.Syntax.TermPrinter (pretty)
import Unison.Term qualified as Tm
import Unison.Util.EnumContainers
import Unison.Util.Pretty (toANSI, toAnsiUnbroken)
import Unison.Var (Var)

type Term v = Tm.Term v ()

traceComb :: (Show clos, Show comb) => Bool -> Word64 -> GComb clos comb -> Bool
traceComb False _ _ = True
traceComb True w c = trace (prettyComb w 0 c "\n") True

traceCombs ::
  Word64 ->
  Bool ->
  EnumMap Word64 Comb ->
  EnumMap Word64 Comb
traceCombs _ False c = c
traceCombs w True c = trace (prettyCombs w c "") c

tracePretty ::
  (Var v) =>
  PrettyPrintEnv ->
  Bool ->
  Term v ->
  Term v
tracePretty _ False tm = tm
tracePretty ppe True tm = trace (toANSI 50 $ pretty ppe tm) tm

tracePrettyNormal ::
  (Var v) =>
  Bool ->
  ANormal v ->
  ANormal v
tracePrettyNormal False tm = tm
tracePrettyNormal True tm = trace (prettyANF False 0 tm "") tm

tracePrettyGroup ::
  (Var v) =>
  String ->
  Bool ->
  SuperGroup v ->
  SuperGroup v
tracePrettyGroup _ False g = g
tracePrettyGroup w True g = trace (prettyGroup w g "") g

prettyRef :: Reference -> String
prettyRef = toAnsiUnbroken . prettyShortHash . shortenTo 10 . toShortHash

tracePrettyCodes :: Bool -> [(Reference, Code)] -> [(Reference, Code)]
tracePrettyCodes False = id
tracePrettyCodes True = map f
  where
    f p@(r, c) = trace (prettyGroup (prettyRef r) (codeGroup c) "") p

