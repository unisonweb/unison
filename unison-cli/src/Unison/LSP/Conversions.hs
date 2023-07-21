module Unison.LSP.Conversions where

import Control.Lens
import Data.IntervalMap.Interval qualified as Interval
import Language.LSP.Types
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
    { _line = fromIntegral $ Lex.line uPos - 1, -- 1 indexed vs 0 indexed
      _character = fromIntegral $ Lex.column uPos - 1
    }

lspToUPos :: Position -> Lex.Pos
lspToUPos lspPos =
  Lex.Pos
    (fromIntegral $ _line lspPos + 1) -- 1 indexed vs 0 indexed
    (fromIntegral $ _character lspPos + 1)

uToLspRange :: Range.Range -> Range
uToLspRange (Range.Range start end) = Range (uToLspPos start) (uToLspPos end)

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.Ann start end -> Just $ Range (uToLspPos start) (uToLspPos end)
