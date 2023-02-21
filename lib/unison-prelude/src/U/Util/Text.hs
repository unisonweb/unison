module U.Util.Text
  ( stripMargin,
  )
where

import qualified Data.Char as Char
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
        Data.Maybe.fromMaybe 0
          . minimumMay
          . map (Text.length . fst . Text.span (== ' '))
          . filter (not . Text.all (Char.isSpace))
          $ Text.lines str
      dropFirstIf f = \case
        h : t | f h -> t
        x -> x
      dropLastIf f = reverse . dropFirstIf f . reverse
   in Text.unlines
        . dropLastIf Text.null
        . dropFirstIf Text.null
        . map (Text.drop stripLen)
        $ Text.lines str
