module U.Util.String where

import Data.Maybe (fromMaybe)
import Safe.Foldable (minimumMay)

stripMargin :: String -> String
stripMargin str =
  let stripLen =
        fromMaybe 0 . minimumMay
          . map (length . fst . span (== ' '))
          $ lines str
   in unlines . map (drop stripLen) $ lines str