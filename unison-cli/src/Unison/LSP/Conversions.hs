module Unison.LSP.Conversions where

import qualified Data.IntervalMap.Interval as Interval
import Language.LSP.Types
import Unison.LSP.Orphans ()
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import qualified Unison.Syntax.Lexer as Lex
import qualified Unison.Util.Range as Range

rangeToInterval :: Range -> Interval.Interval Position
rangeToInterval (Range start end)
  -- Selections are are open on the right-side
  | start == end = Interval.ClosedInterval start end
  -- Ranges of a single 'point' need to be closed for some interval map operations to work as
  -- intended (E.g. intersecting).
  | otherwise = Interval.IntervalCO start end

uToLspPos :: Lex.Pos -> Position
uToLspPos uPos =
  Position
    { _line = fromIntegral $ max 0 (Lex.line uPos - 1), -- 1 indexed vs 0 indexed
      _character = fromIntegral $ max 0 (Lex.column uPos - 1)
    }

uToLspRange :: Range.Range -> Range
uToLspRange (Range.Range start end) = Range (uToLspPos start) (uToLspPos end)

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.GeneratedFrom a -> annToRange a
  Ann.Ann start end -> Just $ Range (uToLspPos start) (uToLspPos end)
