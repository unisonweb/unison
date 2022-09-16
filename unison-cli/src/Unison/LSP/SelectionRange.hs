{-# LANGUAGE DataKinds #-}

module Unison.LSP.SelectionRange where

import Control.Lens hiding (List)
import qualified Data.Foldable as Foldable
import qualified Data.IntervalMap.Lazy as IM
import Data.List (sortBy)
import Language.LSP.Types hiding (Range (..))
import qualified Language.LSP.Types as LSP
import Language.LSP.Types.Lens
import Unison.LSP.Conversions (annToURange, rangeToInterval, uToLspRange)
import Unison.LSP.FileAnalysis (getFileAnalysis)
import Unison.LSP.Types
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Util.Range (Range (..))
import qualified Unison.Util.Range as URange

selectionRangeHandler :: RequestMessage 'TextDocumentSelectionRange -> (Either ResponseError (ResponseResult 'TextDocumentSelectionRange) -> Lsp ()) -> Lsp ()
selectionRangeHandler m respond =
  respond . Right . fromMaybe mempty =<< runMaybeT do
    ranges <- allRangesForFile (m ^. params . textDocument . uri)
    pure $
      (m ^. params . positions) <&> \position ->
        let intersects = IM.containing ranges position
         in fromMaybe (idRange position) $ groupByParentage (sortBy compareRanges $ IM.elems intersects)
  where
    idRange :: LSP.Position -> LSP.SelectionRange
    idRange p = SelectionRange {_range = LSP.Range p p, _parent = Nothing}
    groupByParentage :: [URange.Range] -> Maybe SelectionRange
    groupByParentage (r : next : rest)
      | next `URange.contains` r =
          Just
            SelectionRange
              { _range = uToLspRange next,
                _parent = groupByParentage (next : rest)
              }
      | otherwise = groupByParentage (r : rest)
    groupByParentage [r] = Just $ SelectionRange {_range = uToLspRange r, _parent = Nothing}
    groupByParentage [] = Nothing

    compareRanges :: URange.Range -> URange.Range -> Ordering
    compareRanges r1@(Range start1 end1) r2@(Range start2 end2)
      | r1 `URange.contains` r2 = LT
      | r2 `URange.contains` r1 = GT
      -- Otherwise neither range fits within the other, so sort arbitrarily
      | otherwise = (start1, end1) `compare` (start2, end2)

allRangesForFile :: Uri -> MaybeT Lsp (IM.IntervalMap Position URange.Range)
allRangesForFile uri = do
  FileAnalysis {parsedFile} <- MaybeT (getFileAnalysis uri)
  pf <- MaybeT $ pure parsedFile
  let anns = Foldable.toList pf
  pure $
    anns
      & foldMap aToR
      & fmap (\r -> (rangeToInterval (uToLspRange r), r))
      & IM.fromList
  where
    aToR :: Ann -> [URange.Range]
    aToR = maybeToList . annToURange
