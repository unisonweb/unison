module Unison.Util.Find
  ( simpleFuzzyScore,
  )
where

import Data.Text qualified as Text
import Unison.Prelude

simpleFuzzyScore :: Text -> Text -> Maybe Int
simpleFuzzyScore query s
  | query `Text.isPrefixOf` s = Just (bonus s 2)
  | query `Text.isSuffixOf` s = Just (bonus s 1)
  | query `Text.isInfixOf` s = Just (bonus s 3)
  | lowerquery `Text.isInfixOf` lowers = Just (bonus s 4)
  | otherwise = Nothing
  where
    -- prefer relative names
    bonus s n = if Text.take 1 s == "." then n * 10 else n
    lowerquery = Text.toLower query
    lowers = Text.toLower s
