{-# LANGUAGE DataKinds #-}

module Unison.LSP.Formatting where

import Control.Lens hiding (List)
import Data.IntervalMap.Interval qualified as Interval
import Data.List.NonEmpty.Extra qualified as NEL
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as Decl
import Unison.HashQualified qualified as HQ
import Unison.LSP.Conversions (annToInterval, annToRange, rangeToInterval)
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
  edits <- formatDefs (m ^. params . textDocument . uri) Nothing
  respond . Right . InL $ edits

formatRangeRequest :: Msg.TRequestMessage 'Msg.Method_TextDocumentRangeFormatting -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentRangeFormatting) -> Lsp ()) -> Lsp ()
formatRangeRequest m respond = do
  let p = m ^. params
  edits <- formatDefs (p ^. textDocument . uri) (Just . Set.singleton $ p ^. range)
  respond . Right . InL $ edits

-- | Format all definitions in a file.
formatDefs :: Uri -> Maybe (Set Range {- the ranges to format, if Nothing then format the whole file. -}) -> Lsp [TextEdit]
formatDefs fileUri mayRangesToFormat =
  fromMaybe []
    <$> runMaybeT do
      cwd <- lift getCurrentPath
      FileAnalysis {fileSummary = mayFileSummary} <- getFileAnalysis fileUri
      fileSummary <- hoistMaybe mayFileSummary
      filePPED <- lift $ ppedForFile fileUri
      formattedDecls <-
        (allTypeDecls fileSummary)
          & fmap
            ( \(ref, decl) ->
                let tldAnn = either (Decl.annotation . Decl.toDataDecl) (Decl.annotation) decl
                 in (tldAnn, ref, decl)
            )
          & Map.filter (\(tldAnn, _, _) -> shouldFormatTLD tldAnn)
          & itraverse \sym (tldAnn, ref, decl) -> do
            symName <- hoistMaybe (Name.fromVar sym)
            let declNameSegments = NEL.appendr (Path.toList (Path.unabsolute cwd)) (Name.segments symName)
            let declName = Name.fromSegments declNameSegments
            let hqName = HQ.fromName symName
            let biasedPPED = PPED.biasTo [declName] filePPED
            pure $
              (tldAnn, DeclPrinter.prettyDecl biasedPPED (Reference.DerivedId ref) hqName decl)
                & over _2 Pretty.syntaxToColor
      formattedTerms <-
        (termsBySymbol fileSummary)
          & Map.filter (\(tldAnn, _, _, _) -> shouldFormatTLD tldAnn)
          & itraverse \sym (tldAnn, mayRefId, trm, _typ) -> do
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
      let nonGeneratedDefs =
            (formattedTerms <> formattedDecls)
              & mapMaybe
                ( \case
                    (Ann.Ann {start, end}, txt) -> Just ((start, end), txt)
                    _ -> Nothing
                )
      -- when (null filteredDefs) empty {- Don't format if we have no definitions or it wipes out the fold! -}
      Config {formattingWidth} <- lift getConfig
      let textEdits =
            nonGeneratedDefs & foldMap \((start, end), txt) -> do
              range <- maybeToList $ annToRange (Ann.Ann start end)
              pure $ TextEdit range (Text.pack $ Pretty.toPlain (Pretty.Width formattingWidth) txt)
      pure textEdits
  where
    shouldFormatTLD :: Ann.Ann -> Bool
    shouldFormatTLD ann =
      case mayRangesToFormat of
        Nothing -> True
        Just rangesToFormat -> any (annRangeOverlap ann) rangesToFormat
    -- Does the given range overlap with the given annotation?
    annRangeOverlap :: Ann.Ann -> Range -> Bool
    annRangeOverlap a r =
      annToInterval a & \case
        Nothing -> False
        Just annI -> rangeToInterval r `Interval.overlaps` annI
    stripTypeAnnotation ::
      (Term.Term v a) -> (Term.Term v a)
    stripTypeAnnotation = \case
      Term.Ann' tm _annotation -> tm
      x -> x
