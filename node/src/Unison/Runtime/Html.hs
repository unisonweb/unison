module Unison.Runtime.Html where

import Data.Maybe (listToMaybe, catMaybes, mapMaybe)
import Data.Text (Text, toLower, pack)
import Text.HTML.TagSoup (Tag(..), (~/=), maybeTagText, parseTags, innerText, isTagOpenName, isTagComment, isTagCloseName)
import qualified Text.HTML.TagSoup.Tree as HT
import qualified Data.Char as Char
import qualified Data.Text as Text

data Link = Link { ref :: Text, description :: Text } deriving (Show)

justAnchorSections :: [Tag Text] -> [[Tag Text]]
justAnchorSections [] = []
justAnchorSections l =
  let (newSection, remaining) =
        span (\t -> t ~/= "</a>" && t ~/= "</A>")
        $ dropWhile (\t -> t ~/= "<a>" && t ~/= "<A>") l
  in newSection : justAnchorSections remaining

tagSubtrees :: Text -> [Tag Text] -> [Tag Text]
tagSubtrees tag ts =
  HT.flattenTree [x | x@(HT.TagBranch tag' _ _) <- HT.universeTree (HT.tagTree ts), tag == tag' ]

sectionToLink :: [Tag Text] -> Maybe Link
sectionToLink (TagOpen _ attrList : otherTags) =
  let href = listToMaybe $ filter (\(a, _) -> toLower a == pack "href") attrList
  in href >>= (\(_, v) ->
                 Just . Link v . Text.concat . catMaybes $ map maybeTagText otherTags)
sectionToLink _ = Nothing

getLinks :: Text -> [Link]
getLinks s = mapMaybe sectionToLink . justAnchorSections $ parseTags s

toPlainText :: Text -> Text
toPlainText s =
  Text.concat [headings s, Text.pack "\n", collapseSpaces . innerText . ignores $ parseTags s]

collapseSpaces :: Text -> Text
collapseSpaces t =
  Text.intercalate (Text.pack " ") . filter (/= Text.empty) . Text.split Char.isSpace $ t

headings :: Text -> Text
headings s = collapseSpaces . innerText . tagSubtrees (Text.pack "h1") . parseTags $ s

ignores :: [Tag Text] -> [Tag Text]
ignores = go where
  script = Text.pack "script"
  style = Text.pack "style"
  go [] = []
  go (hd:tl) = case hd of
    _ | isTagOpenName script hd -> go (dropWhile (not . isTagCloseName script) tl)
      | isTagOpenName style hd -> go (dropWhile (not . isTagCloseName style) tl)
      | isTagComment hd -> go tl
      | otherwise -> hd : go tl
