module U.Util.Text
  ( stripMargin,
    unsafeToInt,
  )
where

import Data.Char qualified as Char
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text
import GHC.Stack (HasCallStack)
import Safe.Foldable (minimumMay)
import Unison.Prelude (reportBug)

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

-- |
-- >>> unsafeToInt "123"
-- 123
unsafeToInt :: (HasCallStack) => Text -> Int
unsafeToInt text =
  case Text.decimal text of
    Right (n, "") -> n
    _ -> error (reportBug "E573530" ("not an int: " ++ Text.unpack text))
