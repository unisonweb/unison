{-# LANGUAGE DataKinds #-}

module Unison.LSP.FoldingRange
  ( foldingRangeRequest,
    foldingRangesForFile,
  )
where

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
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)
import Unison.UnisonFile (UnisonFile (..))
import Unison.UnisonFile qualified as UF
import Unison.Var qualified as Var

foldingRangeRequest :: Msg.TRequestMessage 'Msg.Method_TextDocumentFoldingRange -> (Either Msg.ResponseError (Msg.MessageResult 'Msg.Method_TextDocumentFoldingRange) -> Lsp ()) -> Lsp ()
foldingRangeRequest m respond = do
  let fileUri = m ^. params . textDocument . uri
  foldRanges <-
    fromMaybe [] <$> runMaybeT do
      FileAnalysis {parsedFile = mayParsedFile} <- getFileAnalysis fileUri
      parsedFile <- hoistMaybe mayParsedFile
      pure $ foldingRangesForFile parsedFile
  respond . Right . InL $ foldRanges

-- | Return a folding range for each top-level definition
foldingRangesForFile :: UF.UnisonFile Symbol Ann -> [FoldingRange]
foldingRangesForFile UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms, watches} =
  let dataFolds =
        dataDeclarationsId
          & Map.toList
          & map \(sym, (_typ, decl)) -> (Just sym, DD.annotation decl)
      abilityFolds =
        effectDeclarationsId
          & Map.toList
          & map \(sym, (_typ, decl)) -> (Just sym, DD.annotation . DD.toDataDecl $ decl)
      termFolds = terms & fmap \(sym, ann, _trm) -> (Just sym, ann)
      watchFolds =
        watches
          & fold
          & fmap
            ( \(_sym, ann, _trm) ->
                -- We don't use the symbol here because watch symbols are often auto-generated
                -- and ugly.
                (Nothing, ann)
            )
      folds =
        dataFolds <> abilityFolds <> termFolds <> watchFolds
      ranges =
        folds
          & mapMaybe \(sym, range) ->
            (Text.pack . Var.nameStr <$> sym,) <$> annToRange range
   in ranges <&> \(maySym, r) ->
        FoldingRange
          { _startLine = r ^. start . line,
            _startCharacter = Just (r ^. start . character),
            _endLine = r ^. end . line,
            _endCharacter = Just (r ^. end . character),
            _kind = Just FoldingRangeKind_Region,
            _collapsedText = maySym
          }
