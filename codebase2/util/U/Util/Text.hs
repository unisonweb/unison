{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module U.Util.Text where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe.Foldable (minimumMay)
import Debug.Trace (trace, traceShowId)

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
        Data.Maybe.fromMaybe 0 . minimumMay
          . map (Text.length . fst)
          . filter (not . Text.null . snd)
          . map (Text.span (== ' '))
          . filter (not . Text.null)
          $ Text.lines str
  in Text.unlines . traceShowId. map (Text.drop $ traceShowId stripLen) $ traceShowId $ Text.lines str

test :: Bool
test =
  stripMargin x
     == y

x' :: Text
x' = stripMargin x
x :: Text
x = "          def foo:" <> "\n" <>
    "            blah blah" <> "\n" <>
    "      "

y :: Text
y = "def foo:" <> "\n" <>
    "  blah blah" <> "\n"