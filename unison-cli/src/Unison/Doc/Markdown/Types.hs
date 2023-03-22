module Unison.Doc.Markdown.Types where

import Control.Lens (ifoldMap)
import qualified Data.Char as Char
import qualified Data.Text as Text
import Unison.Prelude

-- | Custom type for converting Docs into Markdown.
-- I tried using an existing library for this, but we have too many edge-cases
-- to make it worth it.
data Markdown
  = -- | E.g. '---'
    ThematicBreak
  | Paragraph [Markdown]
  | BlockQuote Markdown
  | -- lang, contents
    CodeBlock Text Text
  | Heading Int Markdown
  | OrderedList Int [Markdown]
  | UnorderedList [Markdown]
  | Txt Text
  | Linebreak
  | InlineCode Text
  | Italics Markdown
  | Strong Markdown
  | Strikethrough Markdown
  | -- label, uri
    Link Markdown Text
  | -- label, uri
    Image Markdown Text
  | -- Header, cells
    Table (Maybe [Markdown]) [[Markdown]]
  deriving (Show)

instance Semigroup Markdown where
  a <> b = case (a, b) of
    (Paragraph xs, Paragraph ys) -> Paragraph (xs <> ys)
    (Paragraph xs, y) -> Paragraph (xs <> [y])
    (x, Paragraph ys) -> Paragraph (x : ys)
    (x, y) -> Paragraph [x, y]

instance Monoid Markdown where
  mempty = Paragraph []

-- TODO: HTML Escaping
toLines :: Markdown -> [Text]
toLines = \case
  ThematicBreak -> ["---"]
  Paragraph m ->
    let go acc next = case (Text.unsnoc acc, Text.uncons next) of
          (Nothing, _) -> next
          (_, Nothing) -> acc
          (Just (_, lastChar), Just (firstChar, _))
            | Char.isSpace lastChar || Char.isSpace firstChar -> acc <> next
            | otherwise -> Text.unwords [acc, next]
     in [foldl' go mempty (foldMap toLines m)]
  BlockQuote m -> ("> " <>) <$> toLines m
  CodeBlock lang contents ->
    [ "```" <> lang
    ]
      <> Text.lines contents
      <> ["```"]
  Heading n contents ->
    case toLines contents of
      [] -> [Text.replicate n "#"]
      (x : xs) -> (Text.replicate n "#" <> " " <> x) : xs
  -- TODO: Nested lists
  OrderedList startNum items ->
    items & ifoldMap \n item ->
      case toLines item of
        [] -> [tShow (n + startNum) <> "."]
        (x : xs) -> tShow (n + startNum) <> ". " <> x : (("  " <>) <$> xs)
  UnorderedList items ->
    items & ifoldMap \_ item ->
      case toLines item of
        [] -> ["-"]
        (x : xs) -> "- " <> x : (("  " <>) <$> xs)
  Txt txt -> Text.lines txt
  Linebreak -> [""]
  InlineCode txt -> [txt]
  Italics md -> toLines md
  Strong md -> toLines md
  Strikethrough md -> toLines md
  -- label, uri
  Link label uri -> ["[" <> Text.unwords (toLines label) <> "](" <> uri <> ")"]
  Image label uri -> ["![" <> Text.unwords (toLines label) <> "](" <> uri <> ")"]
  Table _headers _rows -> [] -- TODO
