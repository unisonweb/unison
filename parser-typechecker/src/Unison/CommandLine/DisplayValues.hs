{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module Unison.CommandLine.DisplayValues where

import Unison.Reference (Reference)
import Unison.Term (AnnotatedTerm)
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

displayDoc :: (Var v, Monad m)
           => PPE.PrettyPrintEnv 
           -> (Reference -> m (Maybe (AnnotatedTerm v a)))
           -> (Reference -> m (Maybe (AnnotatedTerm v a)))
           -> (Reference -> m (Maybe (DD.Decl v a)))
           -> AnnotatedTerm v a 
           -> m Pretty
displayDoc ppe terms evaluated types t = go t
  where
  go (B.DocJoin docs) = foldMap id <$> traverse go docs
  go (B.DocBlob txt) = pure $ P.paragraphyText txt
  go (B.DocLink (B.LinkTerm (Term.TermLink' r))) = pure $ termName r 
  go (B.DocLink (B.LinkType (Term.TypeLink' r))) = pure $ typeName r 
  go (B.DocSource (B.LinkTerm (Term.TermLink' r))) = prettyTerm terms r 
  go (B.DocSource (B.LinkType (Term.TypeLink' r))) = prettyType r
  go (B.DocEvaluate sep (Term.TermLink' r)) =
    foldMap id <$> sequence [ prettyTerm terms r, go sep, prettyTerm evaluated r ]
  go tm = pure $ TP.pretty ppe tm
  prettyTerm terms r = case r of
    Referent.Ref ref -> terms ref >>= \case
      Nothing -> pure $ "😶  Missing term source for: " <> termName r
      Just tm -> pure . P.syntaxToColor $ TP.prettyBinding ppe (PPE.termName ppe r) tm
    Referent.Con r _ _ -> prettyType r 
  prettyType r = types r >>= \case
    Nothing -> pure $ "😶  Missing type source for: " <> typeName r
    Just ty -> pure . P.syntaxToColor $ DP.prettyDecl ppe r (PPE.typeName ppe r) ty
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

