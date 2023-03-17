{-# LANGUAGE DataKinds #-}

module Unison.LSP.FoldingRange where

import Control.Lens hiding (List)
import Language.LSP.Types hiding (line)
import Language.LSP.Types.Lens hiding (id, to)
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as DD
import qualified Unison.Debug as Debug
import Unison.LSP.Conversions (annToRange)
import Unison.LSP.FileAnalysis (getCurrentFileAnalysis)
import Unison.LSP.Types
import Unison.Prelude
import Unison.UnisonFile (UnisonFile (..))

foldingRangeRequest :: RequestMessage 'TextDocumentFoldingRange -> (Either ResponseError (ResponseResult 'TextDocumentFoldingRange) -> Lsp ()) -> Lsp ()
foldingRangeRequest m respond = do
  foldRanges <- foldingRangesForFile (m ^. params . textDocument . uri)
  Debug.debugM Debug.LSP "Folding Ranges" foldRanges
  respond . Right . List $ foldRanges

-- | Return a folding range for each top-level definition
foldingRangesForFile :: Uri -> Lsp [FoldingRange]
foldingRangesForFile fileUri =
  fromMaybe []
    <$> runMaybeT do
      FileAnalysis {parsedFile} <- MaybeT $ getCurrentFileAnalysis fileUri
      UnisonFileId {dataDeclarationsId, effectDeclarationsId, terms} <- MaybeT $ pure parsedFile
      let dataFolds = dataDeclarationsId ^.. folded . _2 . to dataDeclSpan
      let abilityFolds = effectDeclarationsId ^.. folded . _2 . to DD.toDataDecl . to dataDeclSpan
      let termFolds = terms ^.. folded . _2 . to ABT.annotation
      let folds = dataFolds <> abilityFolds <> termFolds
      let ranges = mapMaybe annToRange folds
      pure $ ranges <&> \r -> FoldingRange {_startLine = r ^. start . line, _startCharacter = Just (r ^. start . character), _endLine = r ^. end . line, _endCharacter = Just (r ^. end . character), _kind = Just FoldingRangeRegion}
  where
    dataDeclSpan dd =
      -- We don't have a proper Annotation for data decls so we take the span of all the
      -- constructors using their monoid instance.
      DD.annotation dd <> DD.constructors' dd ^. folded . to (\(a, _v, typ) -> a <> ABT.annotation typ)
