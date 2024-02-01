{-# LANGUAGE DataKinds #-}

module Unison.LSP.FoldingRange where

import Control.Lens hiding (List)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Language.LSP.Protocol.Lens hiding (id, to)
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Unison.DataDeclaration qualified as DD
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Prelude
import Unison.UnisonFile (UnisonFile (..))
import Unison.Var qualified as Var

foldingRangeRequest :: Msg.TRequestMessage 'Msg.Method_TextDocumentFoldingRange -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentFoldingRange) -> Lsp ()) -> Lsp ()
foldingRangeRequest m respond = do
  foldRanges <- foldingRangesForFile (m ^. params . textDocument . uri)
  respond . Right . InL $ foldRanges

-- | Return a folding range for each top-level definition
foldingRangesForFile :: Uri -> Lsp [FoldingRange]
foldingRangesForFile fileUri =
  fromMaybe []
    <$> runMaybeT do
      FileAnalysis {parsedFile} <- getFileAnalysis fileUri
      UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches} <- MaybeT $ pure parsedFile
      let dataFolds =
            dataDeclarationsId
              & Map.toList
              & map \(sym, (_typ, decl)) -> (Just sym, DD.annotation decl)
      let abilityFolds =
            effectDeclarationsId
              & Map.toList
              & map \(sym, (_typ, decl)) -> (Just sym, DD.annotation . DD.toDataDecl $ decl)
      let termFolds = terms & fmap \(sym, ann, _trm) -> (Just sym, ann)
      let watchFolds =
            watches
              & fold
              & fmap
                ( \(_sym, ann, _trm) ->
                    -- We don't use the symbol here because watch symbols are often auto-generated
                    -- and ugly.
                    (Nothing, ann)
                )
      let folds =
            dataFolds <> abilityFolds <> termFolds <> watchFolds
      let ranges =
            folds
              & mapMaybe \(sym, range) ->
                (Text.pack . Var.nameStr <$> sym,) <$> annToRange range
      pure $
        ranges <&> \(maySym, r) ->
          FoldingRange
            { _startLine = r ^. start . line,
              _startCharacter = Just (r ^. start . character),
              _endLine = r ^. end . line,
              _endCharacter = Just (r ^. end . character),
              _kind = Just FoldingRangeKind_Region,
              _collapsedText = maySym
            }
