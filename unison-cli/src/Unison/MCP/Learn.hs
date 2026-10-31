{-# LANGUAGE TemplateHaskell #-}

-- | The @learn@ tool and the server instructions. Both come from the Markdown files in @docs/mcp/learn@, which are
-- embedded in the executable when it is compiled and parsed when the server starts, so nothing here reads a file or
-- runs a tool at run time. See @docs/mcp/learn/README.md@ for how they are organized.
module Unison.MCP.Learn
  ( Guide,
    parseGuide,
    instructions,
    learn,

    -- * The embedded guide
    embeddedGuide,
  )
where

import Data.Char (isSpace)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Text.Megaparsec qualified as Megaparsec
import Unison.Prelude
import Unison.Util.FileEmbed (embedProjectTextDir)

-- | The topics, and the server instructions.
data Guide = Guide
  { -- | The text of the server instructions.
    guideInstructions :: !Text,
    -- | The topics, by path (the names of the headings above it, joined with @/@; the root is the empty path).
    guideTopics :: !(Map Text Topic),
    -- | The children of each topic (by path), in the order they appear in the file.
    guideChildren :: !(Map Text [Text])
  }

data Topic = Topic
  { -- | The first paragraph: what the topic is, in a sentence or two.
    blurb :: !Text,
    -- | The blurb and what follows it, up to the next heading.
    summary :: !Text,
    -- | The @Detail@ section, if there is one.
    detail :: !(Maybe Text)
  }

-- | The guide from the files in @docs/mcp/learn@ at the time this module was compiled.
embeddedGuide :: Guide
embeddedGuide =
  parseGuide $(embedProjectTextDir "../docs/mcp/learn")

-- | The server instructions.
instructions :: Guide -> Text
instructions = guideInstructions

------------------------------------------------------------------------------------------------------------------------
-- Parsing

-- | Parse the guide, given the files (their paths relative to the directory, and their contents).
--
--   * @instructions.md@ is the server instructions.
--   * Every other @.md@ file but @README.md@ is a topic, whose path is the file's path without @.md@. @index.md@ is the
--     root.
--   * Under a heading called @Detail@ (outside of code fences) is the topic's detail; what comes before is its summary.
--   * HTML comments on lines of their own are dropped.
parseGuide :: [(FilePath, Text)] -> Guide
parseGuide files =
  Guide
    { guideInstructions =
        maybe "" (trimBlankLines . Text.unlines . stripComments) (List.lookup "instructions.md" files'),
      guideTopics,
      guideChildren
    }
  where
    files' = [(Text.unpack (Text.replace "\\" "/" (Text.pack file)), text) | (file, text) <- files]

    guideTopics :: Map Text Topic
    guideTopics =
      Map.fromList [(path, parseTopic text) | (file, text) <- files', Just path <- [topicPath file]]

    -- The paths come out in order, so the children of each topic are in alphabetical order.
    guideChildren :: Map Text [Text]
    guideChildren =
      Map.fromListWith (flip (++)) [(parentOf path, [path]) | path <- Map.keys guideTopics, not (Text.null path)]

    parentOf :: Text -> Text
    parentOf = Text.intercalate "/" . List.init . Text.splitOn "/"

-- | The path of the topic a file is for (none if it isn't for one).
topicPath :: FilePath -> Maybe Text
topicPath file = do
  path <- Text.stripSuffix ".md" (Text.pack file)
  guard (path `notElem` ["README", "instructions"])
  pure if path == "index" then "" else path

parseTopic :: Text -> Topic
parseTopic text =
  Topic
    { blurb = firstParagraph summary,
      summary,
      detail
    }
  where
    (summaryLines, detailLines) = splitAtDetail (lineItems text)
    summary = trimBlankLines (Text.unlines summaryLines)
    detail = trimBlankLines . Text.unlines <$> detailLines

-- | Split at the @Detail@ heading (the lines before it, and the lines after it if there is one). A @Detail@ in a code
-- fence is not a heading, and HTML comments are dropped.
splitAtDetail :: [Item] -> ([Text], Maybe [Text])
splitAtDetail = \case
  [] -> ([], Nothing)
  Comment : items -> splitAtDetail items
  Heading _ "Detail" _ : items -> ([], Just (concatMap itemLines items))
  item : items ->
    let (before, after) = splitAtDetail items
     in (itemLines item ++ before, after)

itemLines :: Item -> [Text]
itemLines = \case
  Line line -> [line]
  Heading _ _ line -> [line]
  Fenced fenceLines -> fenceLines
  Comment -> []

-- | The lines of a Markdown file: what each is, as far as the guide needs to know.
data Item
  = -- | A line of text.
    Line !Text
  | -- | A heading, with its level (the number of @#@), its text, and the whole line.
    Heading !Int !Text !Text
  | -- | A code fence, with all of its lines (the opening and closing lines too).
    Fenced [Text]
  | -- | An HTML comment, which starts on a line of its own and goes on to the line that closes it.
    Comment

type Parser = Megaparsec.Parsec Void Text

-- | The text split into items. Anything is a line of text, so this can't fail.
lineItems :: Text -> [Item]
lineItems text =
  case Megaparsec.parse (Megaparsec.many item <* Megaparsec.eof) "" text of
    Left _ -> map Line (Text.lines text)
    Right items -> items
  where
    item :: Parser Item
    item = Megaparsec.choice [fenced, comment, headingItem, Line <$> anyLine]

    -- A line, without its end (the last line may not have one).
    anyLine :: Parser Text
    anyLine = do
      Megaparsec.notFollowedBy Megaparsec.eof
      l <- Megaparsec.takeWhileP Nothing (/= '\n')
      _ <- Megaparsec.optional (Megaparsec.single '\n')
      pure l

    -- A fence opens with at least three backticks or tildes (after at most three spaces of indentation), and closes
    -- with a line of at least as many of the same character and nothing else. The info string after a backtick fence
    -- can't contain a backtick.
    fenced :: Parser Item
    fenced = do
      opening <- Megaparsec.lookAhead anyLine
      Fence c n <- maybe empty pure (opensFence opening)
      _ <- anyLine
      rest <- closing (Fence c n)
      pure (Fenced (opening : rest))

    closing :: Fence -> Parser [Text]
    closing fence =
      Megaparsec.option [] do
        l <- anyLine
        if closesFence fence l then pure [l] else (l :) <$> closing fence

    -- An HTML comment on a line of its own: dropped, with the lines up to the one that closes it.
    comment :: Parser Item
    comment = do
      l <- Megaparsec.lookAhead anyLine
      guard ("<!--" `Text.isPrefixOf` Text.stripStart l)
      let skip = do
            l' <- anyLine
            unless ("-->" `Text.isInfixOf` l') (Megaparsec.option () skip)
      Comment <$ skip

    headingItem :: Parser Item
    headingItem = do
      l <- Megaparsec.lookAhead anyLine
      (level, title) <- maybe empty pure (heading l)
      _ <- anyLine
      pure (Heading level title l)

-- | The lines of the text with the HTML comments (that start on a line of their own, outside code fences) dropped.
stripComments :: Text -> [Text]
stripComments = concatMap itemLines . lineItems

heading :: Text -> Maybe (Int, Text)
heading l = do
  let hashes = Text.takeWhile (== '#') l
  let level = Text.length hashes
  guard (level >= 1 && level <= 6)
  rest <- Text.stripPrefix " " (Text.drop level l)
  pure (level, Text.strip rest)

-- | A code fence: its character, and how long its opening was.
data Fence = Fence !Char !Int

opensFence :: Text -> Maybe Fence
opensFence l = do
  let (indent, stripped) = Text.span (== ' ') l
  guard (Text.length indent <= 3)
  (c, _) <- Text.uncons stripped
  guard (c == '`' || c == '~')
  let run = Text.takeWhile (== c) stripped
  let n = Text.length run
  guard (n >= 3)
  guard (c /= '`' || not ('`' `Text.elem` Text.drop n stripped))
  pure (Fence c n)

closesFence :: Fence -> Text -> Bool
closesFence (Fence c n) l =
  Text.length indent <= 3 && Text.length run >= n && Text.all isSpace (Text.drop (Text.length run) stripped)
  where
    (indent, stripped) = Text.span (== ' ') l
    run = Text.takeWhile (== c) stripped

trimBlankLines :: Text -> Text
trimBlankLines =
  Text.intercalate "\n" . reverse . dropWhile blank . reverse . dropWhile blank . Text.lines
  where
    blank = Text.all isSpace

firstParagraph :: Text -> Text
firstParagraph = Text.unwords . map Text.strip . takeWhile (not . Text.all isSpace) . Text.lines

------------------------------------------------------------------------------------------------------------------------
-- Answering

-- | Look up a topic by path. @Left@ is a message to show when there is no such topic.
--
-- The path is names separated by @/@, like @ucm/merge@. An empty path is the overview. Adding @/detail@ gives a topic's
-- detail. A leading @learn:@ (from a link) is ignored.
learn :: Guide -> Text -> Either Text Text
learn guide request =
  case Map.lookup key (guideTopics guide) of
    Just topic -> Right (renderTopic guide key topic)
    Nothing
      | Just (topicSegments, "detail") <- List.unsnoc segments,
        let topicKey = Text.intercalate "/" topicSegments,
        Just topic <- Map.lookup topicKey (guideTopics guide) ->
          Right (renderDetail topicKey topic)
    Nothing -> Left (notFound guide segments)
  where
    segments = pathSegments request
    key = Text.intercalate "/" segments

pathSegments :: Text -> [Text]
pathSegments request =
  Text.strip request
    & (\t -> fromMaybe t (Text.stripPrefix "learn:" t))
    & Text.toLower
    & Text.splitOn "/"
    & map Text.strip
    & filter (not . Text.null)

renderTopic :: Guide -> Text -> Topic -> Text
renderTopic guide path topic =
  Text.intercalate "\n" . concat $
    [ ["# " <> if Text.null path then "unison" else path, ""],
      [summary topic],
      case detail topic of
        Just _ -> ["", "For more detail on this topic, call `learn` with `" <> path <> "/detail`."]
        Nothing -> [],
      index guide path
    ]

renderDetail :: Text -> Topic -> Text
renderDetail path topic =
  case detail topic of
    Just text -> "# " <> path <> "/detail\n\n" <> text
    Nothing ->
      "The topic `" <> path <> "` has no detail section; its summary is all there is.\n\n" <> summary topic

-- | The subtopics of a topic: a blank line, then a bulleted list of their paths and blurbs (nothing if there are none).
index :: Guide -> Text -> [Text]
index guide path =
  case subtopics guide path of
    [] -> []
    items -> "" : (if Text.null path then "## Topics" else "## Subtopics") : "" : items

-- | The bullets for the subtopics of a topic.
subtopics :: Guide -> Text -> [Text]
subtopics guide path =
  [ " * " <> child <> " - " <> maybe "" blurb (Map.lookup child (guideTopics guide))
  | child <- Map.findWithDefault [] path (guideChildren guide)
  ]

notFound :: Guide -> [Text] -> Text
notFound guide segments =
  Text.intercalate "\n" . concat $
    [ ["There is no topic `" <> Text.intercalate "/" segments <> "`."],
      case subtopics guide nearest of
        [] -> []
        items ->
          ["", (if Text.null nearest then "These are the topics:" else "The closest topic is `" <> nearest <> "`. Its subtopics:")]
            ++ items
    ]
  where
    -- The longest prefix of the path that is a topic (the root if none is).
    nearest =
      Text.intercalate "/" . fromMaybe [] . listToMaybe . reverse $
        filter (\prefix -> Map.member (Text.intercalate "/" prefix) (guideTopics guide)) (List.inits segments)
