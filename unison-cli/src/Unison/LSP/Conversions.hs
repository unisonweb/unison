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

-- | Convert a Unison file-position where the first char is 1 and line is 1, to an LSP `Position`
-- where the first char is 0 and line is 0.
uToLspPos :: Lex.Pos -> Position
uToLspPos uPos =
  Position
    { _line = fromIntegral $ max 0 (Lex.line uPos - 1),
      _character = fromIntegral $ max 0 (Lex.column uPos - 1)
    }

-- | Convert an LSP `Position` where the first char is 0 and line is 0, to a Unison file-position
-- where the first char is 1 and line is 1.
lspToUPos :: Position -> Lex.Pos
lspToUPos Position {_line = line, _character = char} =
  Lex.Pos
    (fromIntegral $ line + 1)
    (fromIntegral $ char + 1)

-- | Convert a Unison `Range` where the first char is 1 and line is 1, to an LSP `Range`
-- where the first char is 0 and line is 0.
uToLspRange :: Range.Range -> Range
uToLspRange (Range.Range start end) = Range (uToLspPos start) (uToLspPos end)

-- | Convert an LSP `Range` where the first char is 0 and line is 0, to a Unison `Range`
-- where the first char is 1 and line is 1.
lspToURange :: Range -> Range.Range
lspToURange (Range start end) = Range.Range (lspToUPos start) (lspToUPos end)

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.GeneratedFrom a -> annToRange a
  Ann.Ann start end -> Just $ Range (uToLspPos start) (uToLspPos end)

annToURange :: Ann.Ann -> Maybe Range.Range
annToURange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.GeneratedFrom a -> annToURange a
  Ann.Ann start end -> Just $ Range.Range start end
