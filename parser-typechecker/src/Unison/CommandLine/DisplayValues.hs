{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module Unison.CommandLine.DisplayValues where

import Data.Map (Map)
import Unison.Reference (Reference)
import Unison.Term (AnnotatedTerm)
import qualified Data.Map as Map
import qualified Unison.DataDeclaration as DD
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Referent as Referent
import qualified Unison.Runtime.IOSource as B
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import qualified Unison.DeclPrinter as DP
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)

type Pretty = P.Pretty P.ColorText

displayDoc :: Var v
           => PPE.PrettyPrintEnv 
           -> Map Reference (AnnotatedTerm v a) 
           -> Map Reference (AnnotatedTerm v a) 
           -> Map Reference (DD.Decl v a)
           -> AnnotatedTerm v a -> Pretty
displayDoc ppe terms evaluated types t = case t of
  B.Doc segs -> foldMap prettySeg segs 
  tm -> TP.pretty ppe tm
  where
  prettySeg (B.SegmentBlob txt) = P.paragraphyText txt
  prettySeg (B.SegmentLink (B.LinkTerm (Term.TermLink' r))) = termName r 
  prettySeg (B.SegmentLink (B.LinkType (Term.TypeLink' r))) = typeName r 
  prettySeg (B.SegmentSource (B.LinkTerm (Term.TermLink' r))) = prettyTerm terms r 
  prettySeg (B.SegmentSource (B.LinkType (Term.TypeLink' r))) = prettyType r
  prettySeg (B.SegmentEvaluate (Term.TermLink' r)) = 
    P.lines [ prettyTerm terms r, "🔽", prettyTerm evaluated r ]
  prettySeg tm = TP.pretty ppe tm
  prettyTerm terms r = case r of
    Referent.Ref ref -> case Map.lookup ref terms of
      Nothing -> "😶  Missing term source for: " <> termName r
      Just tm -> P.syntaxToColor $ TP.prettyBinding ppe (PPE.termName ppe r) tm
    Referent.Con r _ _ -> prettyType r 
  prettyType r = case Map.lookup r types of
    Nothing -> "😶  Missing type source for: " <> typeName r
    Just ty -> P.syntaxToColor $ DP.prettyDecl ppe r (PPE.typeName ppe r) ty
  termName r = P.syntaxToColor $ 
    NP.styleHashQualified'' (NP.fmt S.Reference) name
    where name = PPE.termName ppe r
  typeName r = P.syntaxToColor $ 
    NP.styleHashQualified'' (NP.fmt S.Reference) name
    where name = PPE.typeName ppe r

{-

Hello, this is some documentation. 

`foo` is a link. 

``this is not a link``, just some backticked stuff

```unison
{{ Source @termLink foo }}
```

@ docs.List.map
@ pchiusano
@ licenses.MIT
List.map f x = 
  ...

-}

