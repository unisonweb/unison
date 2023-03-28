module Unison.Server.Doc.Markdown.Types where

import Control.Lens (imap)
import qualified Data.Char as Char
import qualified Data.Char as Text
import qualified Data.Text as Text
import qualified Unison.Debug as Debug
import Unison.Prelude

-- | Custom type for converting Docs into Markdown.
-- I tried using the existing cmark-gfm library for this, but we have too many edge-cases
-- for it to work well.
data Markdown
  = -- | E.g. '---'
    ThematicBreak
  | Paragraph [Markdown]
  | BlockQuote [Markdown]
  | -- lang, contents
    CodeBlock Text Text
  | Heading Int [Markdown]
  | OrderedList Int [[Markdown]]
  | UnorderedList [[Markdown]]
  | Txt Text
  | Linebreak
  | InlineCode Text
  | Italics [Markdown]
  | Strong [Markdown]
  | Strikethrough [Markdown]
  | -- label, uri
    Link [Markdown] Text
  | -- label, uri
    Image [Markdown] Text
  | -- Header, cells
    Table (Maybe [[Markdown]]) [[[Markdown]]]
  deriving (Show)

-- | Render the markdown datatype to markdown text
toText :: [Markdown] -> Text
toText = toText' . Paragraph
  where
    toText' :: Markdown -> Text
    toText' =
      Debug.debug Debug.Temp "Markdown" >>> \case
        ThematicBreak -> "\n---"
        Paragraph m -> flattenParagraph m
        BlockQuote m -> "> " <> flattenParagraph m
        CodeBlock lang contents ->
          "```"
            <> lang
            <> "\n"
            <> contents
            <> "\n```\n\n"
        Heading n contents ->
          (Text.replicate n "#" <> " " <> (flattenInline contents)) <> "\n\n"
        -- TODO: Nested lists
        OrderedList startNum items ->
          items
            & imap
              ( \n item ->
                  tShow (n + startNum) <> ". " <> flattenInline item
              )
            & Text.unlines
            & (<> "\n")
        UnorderedList items ->
          items
            & fmap
              ( \item ->
                  "- " <> flattenInline item
              )
            & Text.unlines
            & (<> "\n")
        Txt txt -> txt
        Linebreak -> "\n\n"
        InlineCode txt -> "`" <> txt <> "`"
        Italics md -> "_" <> flattenInline md <> "_"
        Strong md -> "**" <> flattenInline md <> "**"
        Strikethrough md -> "~~" <> flattenInline md <> "~~"
        -- label, uri
        Link label uri ->
          "[" <> (flattenInline label) <> "](" <> uri <> ")"
        Image label uri -> "![" <> flattenInline label <> "](" <> uri <> ")"
        Table _headers _rows -> mempty -- TODO
      where
        flattenInline :: [Markdown] -> Text
        flattenInline m =
          (toText' <$> m)
            & filter (Text.any (not . Text.isSpace))
            & Text.unwords
        flattenParagraph :: [Markdown] -> Text
        flattenParagraph m =
          let go :: Maybe Text -> Text -> Maybe Text
              go Nothing next = Just next
              go (Just acc) next = case (Text.unsnoc acc, Text.uncons next) of
                (Nothing, _) -> Just $ "\n" <> next
                (_, Nothing) -> Just $ acc <> "\n"
                (Just (_, lastChar), Just (firstChar, _))
                  | Char.isSpace lastChar || Char.isSpace firstChar -> Just $ acc <> next
                  | otherwise -> Just $ Text.unwords [acc, next]
           in case foldl' go Nothing (toText' <$> m) of
                Nothing -> ""
                Just x -> x <> "\n"
