module U.Util.Text where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe.Foldable (minimumMay)

-- | remove however many spaces prefix all of the lines of the input
-- e.g.
-- stripMargin [here|
--         def foo:
--           blah blah
--     |] == [here|
-- def foo:
--   blah blah
-- |]T
stripMargin :: Text -> Text
stripMargin str =
  let stripLen =
        fromMaybe 0 . minimumMay
          . map (Text.length . fst . Text.span (== ' '))
          $ Text.lines str
   in Text.unlines . map (Text.drop stripLen) $ Text.lines str