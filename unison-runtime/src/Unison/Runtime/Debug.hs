module Unison.Runtime.Debug
  ( traceComb,
    traceCombs,
    tracePretty,
    tracePrettyDefs,
    tracePrettyCodes,
    tracePrettyGroup,
    tracePrettyRGroup,
    tracePrettyRPGroup,
    tracePrettyGroups,
    tracePrettyNormal,
    module Debug.Trace,
  )
where

import Data.Map qualified as Map
import Data.Monoid (Endo (..))
import Data.Text qualified as Text
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
import Unison.Util.Pretty (ColorText, Pretty, toANSI)
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
tracePretty ppe True tm = trace (Text.unpack . toANSI 50 $ pretty ppe tm) tm

tracePrettyDefs ::
  (Var v) =>
  PrettyPrintEnv ->
  Bool ->
  [(Reference, Term v)] ->
  [(Reference, Term v)]
tracePrettyDefs _ False tms = tms
tracePrettyDefs ppe True tms = map f tms
  where
    f p@(r, tm) =
      trace (Text.unpack . toANSI 50 $ prettyRef r <> " := " <> pretty ppe tm) p

tracePrettyNormal ::
  (Var v) =>
  Bool ->
  ANormal Reference v ->
  ANormal Reference v
tracePrettyNormal False tm = tm
tracePrettyNormal True tm = trace (prettyANF False 0 tm "") tm

tracePrettyGroup ::
  (Var v) =>
  String ->
  Bool ->
  SuperGroup Reference v ->
  SuperGroup Reference v
tracePrettyGroup _ False g = g
tracePrettyGroup w True g = trace (prettyGroup w g "") g

tracePrettyRGroup ::
  (Var v) =>
  Reference ->
  Bool ->
  SuperGroup Reference v ->
  SuperGroup Reference v
tracePrettyRGroup = tracePrettyGroup . prettyRefStr

tracePrettyRPGroup ::
  (Var v) =>
  Reference ->
  (SuperGroup Reference v -> Bool) ->
  SuperGroup Reference v ->
  SuperGroup Reference v
tracePrettyRPGroup r p g = tracePrettyRGroup r (p g) g

tracePrettyGroups ::
  (Var v) =>
  Bool ->
  Map.Map Reference (SuperGroup Reference v) ->
  Map.Map Reference (SuperGroup Reference v)
tracePrettyGroups False gs = gs
tracePrettyGroups True gs =
  trace (appEndo (foldMap f (Map.toList gs)) "") gs
  where
    f (r, g) = Endo $ prettyGroup (prettyRefStr r) g . showString "\n\n"

prettyRef :: Reference -> Pretty ColorText
prettyRef = prettyShortHash . shortenTo 10 . toShortHash

prettyRefStr :: Reference -> String
prettyRefStr = Text.unpack . toANSI 0 . prettyRef

tracePrettyCodes ::
  Bool -> [(Reference, Code Reference)] -> [(Reference, Code Reference)]
tracePrettyCodes False co = co
tracePrettyCodes True co = trace (foldMap f co "") co
  where
    f (r, c) =
      prettyGroup (prettyRefStr r) (codeGroup c) <> showString "\n\n"
