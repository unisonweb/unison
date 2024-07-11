{-# LANGUAGE DataKinds #-}

module Unison.LSP.Formatting where

import Control.Lens hiding (List)
import Data.Set qualified as Set
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.Codebase.Editor.HandleInput.FormatFile qualified as Formatting
import Unison.Codebase.ProjectPath qualified as PP
import Unison.LSP.Conversions (lspToURange, uToLspRange)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.FileAnalysis qualified as FileAnalysis
import Unison.LSP.Types
import Unison.Prelude

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
  fromMaybe [] <$> runMaybeT do
    FileAnalysis {parsedFile = mayParsedFile, typecheckedFile = mayTypecheckedFile} <- getFileAnalysis fileUri
    pp <- lift getCurrentProjectPath
    Config {formattingWidth} <- lift getConfig
    MaybeT $
      Formatting.formatFile (\uf tf -> FileAnalysis.ppedForFileHelper uf tf) formattingWidth (pp ^. PP.absPath_) mayParsedFile mayTypecheckedFile (Set.map lspToURange <$> mayRangesToFormat)
        <&> (fmap . fmap) uTextReplacementToLSP
  where
    uTextReplacementToLSP :: Formatting.TextReplacement -> TextEdit
    uTextReplacementToLSP (Formatting.TextReplacement newText range) = TextEdit (uToLspRange range) newText
