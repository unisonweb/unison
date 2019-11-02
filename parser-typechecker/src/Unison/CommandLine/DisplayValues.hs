{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# Language OverloadedStrings #-}

module Unison.CommandLine.DisplayValues where

import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Term (AnnotatedTerm)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DP
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S

type Pretty = P.Pretty P.ColorText

displayTerm :: (Var v, Monad m)
           => PPE.PrettyPrintEnv 
           -> (Reference -> m (Maybe (AnnotatedTerm v a)))
           -> (Referent -> m (Maybe (Type v a)))
           -> (Reference -> m (Maybe (AnnotatedTerm v a)))
           -> (Reference -> m (Maybe (DD.Decl v a)))
           -> AnnotatedTerm v a 
           -> m Pretty
displayTerm ppe terms typeOf eval types tm = case tm of
  -- todo: can dispatch on other things with special rendering
  Term.Ref' r -> eval r >>= \case
    Nothing -> pure $ termName ppe (Referent.Ref r) 
    Just tm -> displayDoc ppe terms typeOf eval types tm
  _ -> displayDoc ppe terms typeOf eval types tm

displayDoc :: (Var v, Monad m)
           => PPE.PrettyPrintEnv 
           -> (Reference -> m (Maybe (AnnotatedTerm v a)))
           -> (Referent  -> m (Maybe (Type v a)))
           -> (Reference -> m (Maybe (AnnotatedTerm v a)))
           -> (Reference -> m (Maybe (DD.Decl v a)))
           -> AnnotatedTerm v a 
           -> m Pretty
displayDoc ppe terms typeOf evaluated types t = go t
  where
  go (DD.DocJoin docs) = foldMap id <$> traverse go docs
  go (DD.DocBlob txt) = pure $ P.paragraphyText txt
  go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) = pure $ termName ppe r 
  go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) = pure $ typeName ppe r 
  go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = prettyTerm terms r 
  go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = prettyType r
  go (DD.DocSignature (Term.TermLink' r)) = prettySignature r
  go (DD.DocEvaluate (Term.TermLink' r)) = prettyTerm evaluated r
  go tm = pure $ TP.pretty ppe tm
  prettySignature r = typeOf r >>= \case
    Nothing -> pure $ termName ppe r 
    Just typ -> pure $ P.group $ TypePrinter.prettySignatures ppe [(PPE.termName ppe r, typ)]
  prettyTerm terms r = case r of
    Referent.Ref (Reference.Builtin _) -> prettySignature r 
    Referent.Ref ref -> terms ref >>= \case
      Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
      Just tm -> pure . P.syntaxToColor $ P.group $ TP.prettyBinding ppe (PPE.termName ppe r) tm
    Referent.Con r _ _ -> prettyType r 
  prettyType r = types r >>= \case
    Nothing -> pure $ "😶  Missing type source for: " <> typeName ppe r
    Just ty -> pure . P.syntaxToColor $ P.group $ DP.prettyDecl ppe r (PPE.typeName ppe r) ty

termName :: PPE.PrettyPrintEnv -> Referent -> Pretty
termName ppe r = P.syntaxToColor $ 
  NP.styleHashQualified'' (NP.fmt S.Reference) name
  where name = PPE.termName ppe r

typeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
typeName ppe r = P.syntaxToColor $ 
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

