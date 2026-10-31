module Unison.Test.MCP.Learn (test) where

import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text qualified as Text
import EasyTest
import Unison.MCP.Learn

test :: Test ()
test =
  scope "mcp.learn" . tests $
    [ parsingTests,
      embeddedGuideTests
    ]

sample :: [(FilePath, Text)]
sample =
  [ ( "instructions.md",
      Text.unlines
        [ "<!-- A comment about the file,",
          "     over two lines. -->",
          "",
          "Read [the topics](learn:ucm)."
        ]
    ),
    ("README.md", "# This is not a topic\n\nIt documents the files.\n"),
    ( "index.md",
      Text.unlines
        [ "The root.",
          "",
          "Second paragraph of the root."
        ]
    ),
    ( "ucm.md",
      Text.unlines
        [ "The command line tool. It has two lines",
          "in its blurb.",
          "",
          "- A bullet",
          "",
          "````",
          "# not a heading, inside a fence",
          "## Detail",
          "````",
          "",
          "## Detail",
          "",
          "Details of ucm."
        ]
    ),
    ( "ucm/update.md",
      Text.unlines
        [ "Updating."
        ]
    ),
    ( "ucm/merge.md",
      Text.unlines
        [ "Merging.",
          "",
          "<!-- not shown -->",
          "",
          "## Detail",
          "",
          "Details of merge."
        ]
    ),
    ("testing.md", "Testing.\n")
  ]

parsingTests :: Test ()
parsingTests =
  scope "parsing" . tests $
    [ scope "the instructions are instructions.md without its comments" do
        expect' ("Read [the topics](learn:ucm)." `Text.isInfixOf` instructions guide)
        expect' (not ("A comment" `Text.isInfixOf` instructions guide)),
      scope "the root lists the top-level topics, and README.md is not a topic" do
        text <- expectTopic (learn guide "")
        expect' (" * ucm - " `Text.isInfixOf` text)
        expect' (" * testing - " `Text.isInfixOf` text)
        expect' (not ("README" `Text.isInfixOf` text))
        expect' (isLeft (learn guide "readme")),
      scope "a topic has its summary, a pointer to its detail and its subtopics, but not its detail" do
        text <- expectTopic (learn guide "ucm")
        expect' ("The command line tool." `Text.isInfixOf` text)
        expect' ("ucm/detail" `Text.isInfixOf` text)
        expect' (" * ucm/merge - " `Text.isInfixOf` text)
        expect' (not ("Details of ucm." `Text.isInfixOf` text)),
      scope "a Detail heading inside a code fence does not start the detail" do
        text <- expectTopic (learn guide "ucm")
        expect' ("inside a fence" `Text.isInfixOf` text),
      scope "comments are dropped" do
        text <- expectTopic (learn guide "ucm/merge")
        expect' (not ("not shown" `Text.isInfixOf` text)),
      scope "a topic's detail is at its path and /detail" do
        text <- expectTopic (learn guide "ucm/merge/detail")
        expect' ("Details of merge." `Text.isInfixOf` text),
      scope "paths ignore case, slashes at the ends, and a learn: prefix" $
        expectEqual (learn guide "learn:/UCM/Merge/") (learn guide "ucm/merge"),
      scope "an unknown topic is an error that lists the subtopics of the closest topic" $
        case learn guide "ucm/mege" of
          Left err -> do
            expect' ("ucm/merge" `Text.isInfixOf` err)
            expect' ("ucm/update" `Text.isInfixOf` err)
          Right _ -> crash "expected an error"
    ]
  where
    guide = parseGuide sample

expectTopic :: Either Text Text -> Test Text
expectTopic = \case
  Right actual -> pure actual
  Left err -> crash ("expected a topic, but got the error: " <> Text.unpack err)

-- | The real guide, which is embedded from the files in @docs/mcp/learn@.
embeddedGuideTests :: Test ()
embeddedGuideTests =
  scope "embedded guide" . tests $
    [ scope "has instructions" $
        expect (not (Text.null (instructions embeddedGuide))),
      scope "has an overview" $
        expect (isRight (learn embeddedGuide "")),
      scope "every link in the instructions and the topics goes to a topic" do
        let missing =
              [ link
                | link <- linksIn (instructions embeddedGuide) ++ concatMap linksIn (everyText embeddedGuide),
                  isLeft (learn embeddedGuide link)
              ]
        expectEqual [] missing
    ]

-- | The paths of the @learn:@ links in some Markdown.
linksIn :: Text -> [Text]
linksIn text =
  [ Text.takeWhile (/= ')') rest
    | piece <- drop 1 (Text.splitOn "](learn:" text),
      let rest = piece
  ]

-- | The text of every topic and detail, found by following the index from the root.
everyText :: Guide -> [Text]
everyText guide = go [""] []
  where
    go [] seen = seen
    go (path : paths) seen =
      case learn guide path of
        Left _ -> go paths seen
        Right text ->
          let subtopics = [Text.takeWhile (/= ' ') (Text.drop 3 l) | l <- Text.lines text, " * " `Text.isPrefixOf` l]
              detailPath = path <> (if Text.null path then "" else "/") <> "detail"
              detailText = either (const []) (: []) (learn guide detailPath)
           in go (paths ++ subtopics) (text : detailText ++ seen)
