module Unison.LSP.Conversions where

import qualified Data.IntervalMap.Interval as Interval
import Language.LSP.Types
import Unison.LSP.Orphans ()
import qualified Unison.Lexer as Lex
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import qualified Unison.Util.Range as Range

rangeToInterval :: Range -> Interval.Interval Position
rangeToInterval (Range start end) =
  -- TODO: Double check whether LSP ranges are closed or partially closed
  Interval.IntervalCO start end

uToLspPos :: Lex.Pos -> Position
uToLspPos uPos =
  Position
    { _line = fromIntegral $ Lex.line uPos - 1, -- 1 indexed vs 0 indexed
      _character = fromIntegral $ Lex.column uPos - 1 -- 1 indexed vs 0 indexed
    }

uToLspRange :: Range.Range -> Range
uToLspRange (Range.Range start end) = Range (uToLspPos start) (uToLspPos end)

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.Ann start end -> Just $ Range (uToLspPos start) (uToLspPos end)
