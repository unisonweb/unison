module Unison.PatternMatchCoverage.Pretty where

import Data.Char
import Unison.ConstructorReference (ConstructorReference)
import Unison.PrettyPrintEnv
import Unison.Symbol
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term qualified as Term
import Unison.Util.Pretty
import Unison.Util.Pretty qualified as P
import Unison.Var

prettyVar :: (Var v) => v -> Pretty ColorText
prettyVar v =
  let go x =
        let (d, m) = divMod x 26
            c = chr (ord 'a' + fromIntegral m)
         in c : case d of
              0 -> ""
              _ -> go d
   in P.bold $ string (go (freshId v))

prettyConstructorReference :: PrettyPrintEnv -> ConstructorReference -> Pretty ColorText
prettyConstructorReference ppe cr =
  let con :: Term.Term Symbol ()
      con = Term.constructor () cr
   in TermPrinter.pretty ppe con
