{-# LANGUAGE DataKinds #-}

module Unison.LSP.Formatting where

import Control.Lens hiding (List)
import Data.List.NonEmpty.Extra qualified as NEL
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as Decl
import Unison.Debug qualified as Debug
import Unison.HashQualified qualified as HQ
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis, ppedForFile)
import Unison.LSP.Types
import Unison.Name qualified as Name
import Unison.Parser.Ann qualified as Ann
import Unison.Prelude
import Unison.PrettyPrintEnv.Util qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference qualified as Reference
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term qualified as Term
import Unison.Util.Pretty qualified as Pretty

formatDocRequest :: Msg.TRequestMessage 'Msg.Method_TextDocumentFormatting -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentFormatting) -> Lsp ()) -> Lsp ()
formatDocRequest m respond = do
  edits <- formatDefs (m ^. params . textDocument . uri)
  respond . Right . InL $ edits

-- | Format all definitions in a file.
formatDefs :: Uri -> Lsp [TextEdit]
formatDefs fileUri =
  fromMaybe []
    <$> runMaybeT do
      cwd <- lift getCurrentPath
      FileAnalysis {fileSummary = mayFileSummary} <- getFileAnalysis fileUri
      fileSummary <- hoistMaybe mayFileSummary
      -- let FileSummary{dataDeclsBySymbol, effectDeclsBySymbol, termsBySymbol} = fileSummary
      -- (datas, effects, termsAndWatches) <-
      --   (Just (UF.TypecheckedUnisonFileId {dataDeclarationsId', effectDeclarationsId', hashTermsId}), _) -> do
      --     let termsWithWatchKind =
      --           Map.toList hashTermsId
      --             <&> \(sym, (tldAnn, refId, wk, tm, _typ)) -> (sym, tldAnn, Just refId, tm, wk)
      --     Debug.debugM Debug.Temp "term ranges" $ termsWithWatchKind
      --     Debug.debugM Debug.Temp "decl ranges" $ dataDeclarationsId'
      --     pure (dataDeclarationsId', effectDeclarationsId', termsWithWatchKind)
      --   (_, Just (UF.UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches})) -> do
      --     let termsWithKind = terms <&> \(sym, tldAnn, trm) -> (sym, tldAnn, Nothing, trm, Nothing)
      --     let _watchesWithKind = watches & ifoldMap \wk exprs -> exprs <&> \(sym, tldAnn, trm) -> (sym, tldAnn, Nothing, trm, Just wk)
      --     pure (dataDeclarationsId, effectDeclarationsId, termsWithKind {- <> watchesWithKind -})
      --   (Nothing, Nothing) -> empty
      filePPED <- lift $ ppedForFile fileUri
      -- let termsWithoutWatches =
      --       termsAndWatches & filter \case
      --         (_sym, _tldAnn, _mayRefId, _trm, wk) -> wk == Nothing
      -- let decls = Map.toList (fmap Right <$> datas) <> Map.toList (fmap Left <$> effects)
      formattedDecls <- ifor (allTypeDecls fileSummary) \sym (ref, decl) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let declNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let declName = Name.fromSegments declNameSegments
        let hqName = HQ.fromName symName
        let biasedPPED = PPED.biasTo [declName] filePPED
        pure $
          (either (Decl.annotation . Decl.toDataDecl) Decl.annotation decl, DeclPrinter.prettyDecl biasedPPED (Reference.DerivedId ref) hqName decl)
            & over _2 Pretty.syntaxToColor
      formattedTerms <- ifor (termsBySymbol fileSummary) \sym (tldAnn, mayRefId, trm, _typ) -> do
        symName <- hoistMaybe (Name.fromVar sym)
        let defNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
        let defName = Name.fromSegments defNameSegments
        let hqName = HQ.NameOnly symName
        let biasedPPED = PPED.biasTo [defName] filePPED
        let definitionPPE = case mayRefId of
              Just refId -> PPE.declarationPPE biasedPPED (Reference.DerivedId refId)
              Nothing -> PPED.suffixifiedPPE biasedPPED
        let formattedTerm = Pretty.syntaxToColor $ TermPrinter.prettyBindingWithoutTypeSignature definitionPPE hqName (stripTypeAnnotation trm)
        -- let formattedWatches =
        --       allWatches fileSummary & map \(_tldAnn, maySym, _mayRef, trm, _mayType, mayWatchKind) -> do
        --         case (mayWatchKind, maySym) of
        --           (Just wk, Just (Symbol.Symbol _ (Var.User {}))) ->
        --             -- Watch with binding
        --             Pretty.syntaxToColor $ Pretty.string wk <> "> " <> TermPrinter.prettyBindingWithoutTypeSignature definitionPPE hqName (stripTypeAnnotation trm)
        --           (Just wk, _) -> Pretty.string wk <> "> " <> TermPrinter.prettyBlock False definitionPPE (stripTypeAnnotation trm)
        --           (Nothing, _) -> "> " <> TermPrinter.prettyBlock False definitionPPE (stripTypeAnnotation trm)

        -- let formattedTm = case sym of
        --       Symbol.Symbol _ (Var.User {}) -> Pretty.syntaxToColor $ TermPrinter.prettyBindingWithoutTypeSignature biasedPPE hqName trm
        --       _ -> TermPrinter.pretty biasedPPE (stripTypeAnnotation trm)
        -- let formatted = case wk of
        --       Nothing -> Pretty.syntaxToColor $ TermPrinter.prettyBinding biasedPPE hqName trm
        --       Just wk -> Pretty.string wk <> "> " <> formattedTm
        pure (tldAnn, formattedTerm)

      -- Only keep definitions which are _actually_ in the file, skipping generated accessors
      -- and such.
      let filteredDefs =
            (formattedTerms <> formattedDecls)
              & mapMaybe
                ( \case
                    (Ann.Ann {start, end}, txt) -> Just (Debug.debugLog Debug.Temp "start,end" $ (start, end), txt)
                    _ -> Nothing
                )
      -- when (null filteredDefs) empty {- Don't format if we have no definitions or it wipes out the fold! -}
      Config {formattingWidth} <- lift getConfig
      let textEdits =
            filteredDefs & foldMap \((start, end), txt) -> do
              range <- maybeToList $ annToRange (Ann.Ann start end)
              pure $ TextEdit range (Text.pack $ Pretty.toPlain (Pretty.Width formattingWidth) txt)
      pure textEdits
  where
    stripTypeAnnotation ::
      (Term.Term v a) -> (Term.Term v a)
    stripTypeAnnotation = \case
      Term.Ann' tm _annotation -> tm
      x -> x
