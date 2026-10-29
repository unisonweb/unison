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
    [ scope "instructions are the file instructions.md, without comments" $
        expectEqual (instructions guide) "Read [the topics](learn:ucm).",
      scope "the root lists the top-level topics, with their blurbs, in alphabetical order" $
        expectTopic
          "# unison\n\nThe root.\n\nSecond paragraph of the root.\n\n## Topics\n\n * testing - Testing.\n * ucm - The command line tool. It has two lines in its blurb."
          (learn guide ""),
      scope "a topic has its summary, a pointer to its detail, and its subtopics" $
        expectTopic
          ( Text.intercalate
              "\n"
              [ "# ucm",
                "",
                "The command line tool. It has two lines",
                "in its blurb.",
                "",
                "- A bullet",
                "",
                "````",
                "# not a heading, inside a fence",
                "## Detail",
                "````",
                "",
                "For more detail on this topic, call `learn` with `ucm/detail`.",
                "",
                "## Subtopics",
                "",
                " * ucm/merge - Merging.",
                " * ucm/update - Updating."
              ]
          )
          (learn guide "ucm"),
      scope "a topic without subtopics has no index" $
        expectTopic "# ucm/update\n\nUpdating." (learn guide "ucm/update"),
      scope "detail" $
        expectTopic "# ucm/merge/detail\n\nDetails of merge." (learn guide "ucm/merge/detail"),
      scope "paths ignore case, slashes at the ends, and a learn: prefix" $
        expectEqual (learn guide "learn:/UCM/Merge/") (learn guide "ucm/merge"),
      scope "a topic with no detail says so, and gives its summary" $
        expectTopic
          "The topic `ucm/update` has no detail section; its summary is all there is.\n\nUpdating."
          (learn guide "ucm/update/detail"),
      scope "README.md is not a topic" $
        expect (isLeft (learn guide "readme")),
      scope "an unknown topic is an error that lists the subtopics of the closest topic" $
        expectMissing
          "There is no topic `ucm/mege`.\n\nThe closest topic is `ucm`. Its subtopics:\n * ucm/merge - Merging.\n * ucm/update - Updating."
          (learn guide "ucm/mege"),
      scope "an unknown top-level topic lists the topics" $
        expectMissing
          "There is no topic `nope`.\n\nThese are the topics:\n * testing - Testing.\n * ucm - The command line tool. It has two lines in its blurb."
          (learn guide "nope")
    ]
  where
    guide = parseGuide sample

expectTopic :: Text -> Either Text Text -> Test ()
expectTopic expected = \case
  Right actual -> expectEqual expected actual
  Left err -> crash ("expected a topic, but got the error: " <> Text.unpack err)

expectMissing :: Text -> Either Text Text -> Test ()
expectMissing expected = \case
  Left actual -> expectEqual expected actual
  Right text -> crash ("expected an error, but got: " <> Text.unpack text)

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
