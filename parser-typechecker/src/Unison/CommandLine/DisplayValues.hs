{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.CommandLine.DisplayValues where

import Data.Foldable (fold)
import qualified Unison.Builtin.Decls as DD
import qualified Unison.DataDeclaration as DD
import qualified Unison.DeclPrinter as DP
import qualified Unison.NamePrinter as NP
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TP
import Unison.Type (Type)
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)

type Pretty = P.Pretty P.ColorText

displayTerm ::
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term v a))) ->
  (Referent -> m (Maybe (Type v a))) ->
  (Reference -> m (Maybe (Term v a))) ->
  (Reference -> m (Maybe (DD.Decl v a))) ->
  Term v a ->
  m Pretty
displayTerm pped terms typeOf eval types tm = case tm of
  -- todo: can dispatch on other things with special rendering
  Term.Ref' r ->
    eval r >>= \case
      Nothing -> pure $ termName (PPE.suffixifiedPPE pped) (Referent.Ref r)
      Just tm -> displayDoc pped terms typeOf eval types tm
  _ -> displayDoc pped terms typeOf eval types tm

displayDoc ::
  forall v m a.
  (Var v, Monad m) =>
  PPE.PrettyPrintEnvDecl ->
  (Reference -> m (Maybe (Term v a))) ->
  (Referent -> m (Maybe (Type v a))) ->
  (Reference -> m (Maybe (Term v a))) ->
  (Reference -> m (Maybe (DD.Decl v a))) ->
  Term v a ->
  m Pretty
displayDoc pped terms typeOf evaluated types = go
  where
    go (DD.DocJoin docs) = fold <$> traverse go docs
    go (DD.DocBlob txt) = pure $ P.paragraphyText txt
    go (DD.DocLink (DD.LinkTerm (Term.TermLink' r))) =
      pure $ P.underline (termName (PPE.suffixifiedPPE pped) r)
    go (DD.DocLink (DD.LinkType (Term.TypeLink' r))) =
      pure $ P.underline (typeName (PPE.suffixifiedPPE pped) r)
    go (DD.DocSource (DD.LinkTerm (Term.TermLink' r))) = prettyTerm terms r
    go (DD.DocSource (DD.LinkType (Term.TypeLink' r))) = prettyType r
    go (DD.DocSignature (Term.TermLink' r)) = prettySignature r
    go (DD.DocEvaluate (Term.TermLink' r)) = prettyEval evaluated r
    go tm = pure $ TP.pretty (PPE.suffixifiedPPE pped) tm
    prettySignature r =
      typeOf r >>= \case
        Nothing -> pure $ termName (PPE.unsuffixifiedPPE pped) r
        Just typ ->
          pure . P.group $
            TypePrinter.prettySignatures
              (PPE.suffixifiedPPE pped)
              [(PPE.termName (PPE.unsuffixifiedPPE pped) r, typ)]
    prettyEval terms r = case r of
      Referent.Ref (Reference.Builtin n) -> pure . P.syntaxToColor $ P.text n
      Referent.Ref ref ->
        let ppe = PPE.declarationPPE pped ref
         in terms ref >>= \case
              Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
              Just tm -> pure $ TP.pretty ppe tm
      Referent.Con r _ _ -> pure $ typeName (PPE.declarationPPE pped r) r
    prettyTerm terms r = case r of
      Referent.Ref (Reference.Builtin _) -> prettySignature r
      Referent.Ref ref ->
        let ppe = PPE.declarationPPE pped ref
         in terms ref >>= \case
              Nothing -> pure $ "😶  Missing term source for: " <> termName ppe r
              Just tm -> pure . P.syntaxToColor $ P.group $ TP.prettyBinding ppe (PPE.termName ppe r) tm
      Referent.Con r _ _ -> prettyType r
    prettyType r =
      let ppe = PPE.declarationPPE pped r
       in types r >>= \case
            Nothing -> pure $ "😶  Missing type source for: " <> typeName ppe r
            Just ty -> pure . P.syntaxToColor $ P.group $ DP.prettyDecl ppe r (PPE.typeName ppe r) ty

termName :: PPE.PrettyPrintEnv -> Referent -> Pretty
termName ppe r =
  P.syntaxToColor $
    NP.styleHashQualified'' (NP.fmt $ S.Referent r) name
  where
    name = PPE.termName ppe r

typeName :: PPE.PrettyPrintEnv -> Reference -> Pretty
typeName ppe r =
  P.syntaxToColor $
    NP.styleHashQualified'' (NP.fmt $ S.Reference r) name
  where
    name = PPE.typeName ppe r
