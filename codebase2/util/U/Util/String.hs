module U.Util.String where

import qualified Data.Text as Text
import qualified U.Util.Text as Text

stripMargin :: String -> String
stripMargin = Text.unpack . Text.stripMargin . Text.pack