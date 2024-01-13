{-# LANGUAGE DataKinds #-}

module Unison.LSP.FoldingRange where

import Control.Lens hiding (List)
import Language.LSP.Protocol.Lens hiding (id, to)
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.ABT qualified as ABT
import Unison.DataDeclaration qualified as DD
import Unison.Debug qualified as Debug
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Prelude
import Unison.UnisonFile (UnisonFile (..))

foldingRangeRequest :: Msg.TRequestMessage 'Msg.Method_TextDocumentFoldingRange -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentFoldingRange) -> Lsp ()) -> Lsp ()
foldingRangeRequest m respond = do
  foldRanges <- foldingRangesForFile (m ^. params . textDocument . uri)
  Debug.debugM Debug.LSP "Folding Ranges" foldRanges
  respond . Right . InL $ foldRanges

-- | Return a folding range for each top-level definition
foldingRangesForFile :: Uri -> Lsp [FoldingRange]
foldingRangesForFile fileUri =
  fromMaybe []
    <$> runMaybeT do
      FileAnalysis {parsedFile} <- getFileAnalysis fileUri
      UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms} <- MaybeT $ pure parsedFile
      let dataFolds = dataDeclarationsId ^.. folded . _2 . to dataDeclSpan
      let abilityFolds = effectDeclarationsId ^.. folded . _2 . to DD.toDataDecl . to dataDeclSpan
      let termFolds = terms ^.. folded . _3 . to ABT.annotation
      let folds = dataFolds <> abilityFolds <> termFolds
      let ranges = mapMaybe annToRange folds
      pure $
        ranges <&> \r ->
          FoldingRange
            { _startLine = r ^. start . line,
              _startCharacter = Just (r ^. start . character),
              _endLine = r ^. end . line,
              _endCharacter = Just (r ^. end . character),
              _kind = Just FoldingRangeKind_Region,
              _collapsedText = Nothing
            }
  where
    dataDeclSpan dd =
      -- We don't have a proper Annotation for data decls so we take the span of all the
      -- constructors using their monoid instance.
      DD.annotation dd <> DD.constructors' dd ^. folded . to (\(a, _v, typ) -> a <> ABT.annotation typ)
