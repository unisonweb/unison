module Unison.LSP.FileInfo where

import Language.LSP.Types (Position (..), Range (Range))
import qualified Unison.Lexer.Pos as UPos
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann

annToRange :: Ann -> Maybe Range
annToRange = \case
  Ann.Intrinsic -> Nothing
  Ann.External -> Nothing
  Ann.Ann start end -> Just $ Range (toLspPos start) (toLspPos end)
  where
    toLspPos :: UPos.Pos -> Position
    toLspPos uPos =
      Position
        { _line = fromIntegral $ UPos.line uPos - 1, -- 1 indexed vs 0 indexed
          _character = fromIntegral $ UPos.column uPos - 1 -- 1 indexed vs 0 indexed
        }
