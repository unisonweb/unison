module Unison.Runtime.Html where

import Data.Maybe (listToMaybe, catMaybes, mapMaybe)
import Data.Text (Text, toLower, pack)
import Text.HTML.TagSoup
import qualified Data.Text as Text

data Link = Link { ref :: Text, description :: Text } deriving (Show)

justAnchorSections :: [Tag Text] -> [[Tag Text]]
justAnchorSections [] = []
justAnchorSections l =
  let (newSection, remaining) =
        span (\t -> t ~/= "</a>" && t ~/= "</A>")
        $ dropWhile (\t -> t ~/= "<a>" && t ~/= "<A>") l
  in newSection : justAnchorSections remaining

sectionToLink :: [Tag Text] -> Maybe Link
sectionToLink (TagOpen _ attrList : otherTags) =
  let href = listToMaybe $ filter (\(a, _) -> toLower a == pack "href") attrList
  in href >>= (\(_, v) ->
                 Just . Link v . Text.concat . catMaybes $ map maybeTagText otherTags)
sectionToLink _ = Nothing

getLinks :: Text -> [Link]
getLinks s = mapMaybe sectionToLink . justAnchorSections $ parseTags s
