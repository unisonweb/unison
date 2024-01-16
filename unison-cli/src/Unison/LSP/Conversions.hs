module Unison.LSP.Conversions where

import Control.Lens
import Data.IntervalMap.Interval qualified as Interval
import Language.LSP.Protocol.Types
import Unison.LSP.Orphans ()
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Ann
import Unison.Syntax.Lexer qualified as Lex
import Unison.Util.Range qualified as Range

rangeToInterval :: Range -> Interval.Interval Position
rangeToInterval (Range start end) =
  Interval.ClosedInterval start end

annToInterval :: Ann -> Maybe (Interval.Interval Position)
annToInterval ann = annToRange ann <&> rangeToInterval

uToLspPos :: Lex.Pos -> Position
uToLspPos uPos =
  Position
    { _line = fromIntegral $ max 0 (Lex.line uPos - 1), -- 1 indexed vs 0 indexed
      _character = fromIntegral $ max 0 (Lex.column uPos - 1)
    }

lspToUPos :: Position -> Lex.Pos
lspToUPos Position {_line = line, _character = char} =
  Lex.Pos
    (fromIntegral $ line + 1) -- 1 indexed vs 0 indexed
    (fromIntegral $ char + 1)

uToLspRange :: Range.Range -> Range
uToLspRange (Range.Range start end) = Range (uToLspPos start) (uToLspPos end)

lspToURange :: Range -> Range.Range
lspToURange (Range start end) = Range.Range (lspToUPos start) (lspToUPos end)

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.GeneratedFrom a -> annToRange a
  Ann.Ann start end -> Just $ Range (uToLspPos start) (uToLspPos end)
