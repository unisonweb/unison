module Unison.Test.MCP (test) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import EasyTest
import Unison.Cli.Share.Ref
import Unison.Codebase.Transcript (Transcript)
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.MCP.Allowlist
import Unison.MCP.LineNumbers
import Unison.MCP.Output

test :: Test ()
test =
  scope "mcp" . tests $
    [ allowlistTests,
      shareRefTests,
      searchWordsTests,
      lineNumberTests,
      outputTests
    ]

parse :: Text -> Test Transcript
parse source =
  case Transcript.parse "test.md" (Text.encodeUtf8 source) of
    Left err -> crash (show err)
    Right transcript -> pure transcript

ucm :: [Text] -> Text
ucm commands = "``` ucm\n" <> Text.unlines commands <> "```\n"

allowlistTests :: Test ()
allowlistTests =
  scope "allowlist" . tests $
    [ scope "every listed command exists" $
        tests [scope name (expect (Map.member name canonicalNames)) | name <- Map.keys commandTiers],
      allows ReadOnly "view and find" (ucm ["p/b> view foo", "p/b> find bar"]),
      allows ReadOnly "an alias of a read-only command" (ucm ["p/b> ls"]),
      allows ReadOnly "share commands" (ucm ["p/b> share.source @a/b:c.d"]),
      rejects ReadOnly "update" (ucm ["p/b> update"]) [("code block 1, line 1", Local)],
      allows Local "update" (ucm ["p/b> update"]),
      rejects ReadOnly "an alias (add) of update" (ucm ["p/b> add"]) [("code block 1, line 1", Local)],
      allows Local "an alias (add) of update" (ucm ["p/b> add"]),
      allows Local "deleting a term" (ucm ["p/b> delete.term foo"]),
      rejects Local "deleting a project" (ucm ["p/b> delete.project foo"]) [("code block 1, line 1", All)],
      rejects Local "run" (ucm ["p/b> run main"]) [("code block 1, line 1", IO)],
      allows IO "run" (ucm ["p/b> run main"]),
      rejects IO "push" (ucm ["p/b> push"]) [("code block 1, line 1", All)],
      allows All "push" (ucm ["p/b> push"]),
      rejects Local "an unknown command" (ucm ["p/b> nonsense"]) [("code block 1, line 1", All)],
      allows ReadOnly "unison blocks" "``` unison\nx = 1\n```\n",
      allows ReadOnly "api GET" "``` api\nGET /api/projects\n```\n",
      rejects Local "api POST" "``` api\nPOST /api/foo\nBODY:\n  {}\n```\n" [("code block 1", All)],
      rejects ReadOnly "autoupdate" "---\nautoupdate: true\n---\n\n``` unison\nx = 1\n```\n" [("frontmatter", Local)],
      allows Local "autoupdate" "---\nautoupdate: true\n---\n\n``` unison\nx = 1\n```\n",
      scope "reports every offender, with where it is" do
        transcript <- parse (ucm ["p/b> view a", "p/b> push"] <> "\n" <> ucm ["p/b> run main"])
        expectEqual
          [("code block 1, line 2", All), ("code block 2, line 1", IO)]
          [(location, requires) | Violation {location, requires} <- validate Local transcript]
    ]
  where
    allows tier name source =
      scope (Text.unpack (toolName tier <> " allows " <> name)) do
        transcript <- parse source
        expectEqual [] (validate tier transcript)
    rejects tier name source expected =
      scope (Text.unpack (toolName tier <> " rejects " <> name)) do
        transcript <- parse source
        expectEqual expected [(location, requires) | Violation {location, requires} <- validate tier transcript]

shareRefTests :: Test ()
shareRefTests =
  scope "share refs" . tests $
    [ parsesTo "@unison/base" (ShareRef "unison" "base" ShareLatest Nothing),
      parsesTo "@unison/base:List.map" (ShareRef "unison" "base" ShareLatest (Just "List.map")),
      parsesTo "@unison/base/releases/2.1.2:Optional.map" (ShareRef "unison" "base" (ShareRelease "2.1.2") (Just "Optional.map")),
      parsesTo "@unison/base/main:x" (ShareRef "unison" "base" (ShareBranchNamed "main") (Just "x")),
      fails "unison/base",
      fails "@unison",
      fails "@unison/base:",
      fails "@unison/base/releases",
      fails "@unison/base/a/b/c"
    ]
  where
    parsesTo input expected = scope (Text.unpack input) (expectEqual (Right expected) (parseShareRef input))
    fails input = scope (Text.unpack input) (expect (either (const True) (const False) (parseShareRef input)))

outputTests :: Test ()
outputTests =
  scope "output" . tests $
    [ scope "25 lines are returned as they are" do
        out <- io (withOutputDir \dir -> deliver dir (Text.unlines (replicate maxInlineLines "line")))
        expectEqual (Text.unlines (replicate maxInlineLines "line")) out,
      scope "26 lines go to a file, and only the path is returned" do
        let long = Text.unlines (replicate (maxInlineLines + 1) "line")
        (message, contents) <-
          io $ withOutputDir \dir -> do
            message <- deliver dir long
            let path = Text.unpack . fst . Text.breakOn ". Read or grep" . snd $ Text.breakOn "/" message
            contents <- Text.readFile path
            pure (message, contents)
        expectEqual long contents
        expect (not ("line\nline" `Text.isInfixOf` message))
    ]

searchWordsTests :: Test ()
searchWordsTests =
  scope "search words" . tests $
    [ parsesTo ["List.map"] ("List.map", Nothing),
      parsesTo ["[a]", "->", "Nat"] ("[a] -> Nat", Nothing),
      parsesTo ["map", "in", "@unison"] ("map", Just (FilterUser "@unison")),
      parsesTo ["map", "in", "@unison/base"] ("map", Just (FilterProject "@unison/base")),
      -- `in` is only a filter when it is followed by a user or a project
      parsesTo ["fold", "in", "order"] ("fold in order", Nothing),
      fails ["map", "in", "@"],
      fails ["map", "in", "@a/b/c"],
      fails ["in", "@unison"]
    ]
  where
    parsesTo input expected = scope (Text.unpack (Text.unwords input)) (expectEqual (Right expected) (parseSearchWords input))
    fails input = scope (Text.unpack (Text.unwords input)) (expect (either (const True) (const False) (parseSearchWords input)))

lineNumberTests :: Test ()
lineNumberTests =
  scope "line numbers" . tests $
    [ scope "messages about unison blocks count from the start of the transcript" do
        output <- absolutized source "your transcript" outputOfRun
        expect ("      5 | y = bad" `Text.isInfixOf` output)
        -- two digits: the `|`s stay lined up
        expect ("     10 | w = worse" `Text.isInfixOf` output)
        expect ("Line numbers in the messages above are lines of your transcript." `Text.isInfixOf` output),
      scope "the note says what the numbers are lines of" do
        output <- absolutized source "transcript.md" outputOfRun
        expect ("lines of transcript.md." `Text.isInfixOf` output),
      scope "messages about a file that was loaded are left alone" do
        output <- absolutized source "your transcript" outputOfRun
        expect ("      2 | foo = bad" `Text.isInfixOf` output),
      scope "the error of a failed unison block, in the failure trailer, is rewritten too" do
        output <- absolutized "``` unison\nx = 1\ny = bad\n```\n" "your transcript" failedRun
        expect ("    3 | y = bad" `Text.isInfixOf` output),
      scope "nothing changes when there are no messages to rewrite" do
        let untouched = "``` unison\nx = 1\n```\n\n``` ucm :added-by-ucm\n  + x : Nat\n```\n"
        output <- absolutized "``` unison\nx = 1\n```\n" "your transcript" untouched
        expect (not ("Line numbers in the messages" `Text.isInfixOf` output)),
      scope "numbers that are not lines of the block are left alone" do
        let out = "``` unison\nx = 1\n```\n\n``` ucm :added-by-ucm\n    9 | not a line of the block\n```\n"
        output <- absolutized "``` unison\nx = 1\n```\n" "your transcript" out
        expect ("    9 | not a line of the block" `Text.isInfixOf` output)
    ]
  where
    -- The code of the first unison block starts on line 4, and that of the second on line 9.
    source =
      Text.unlines
        [ "# T",
          "",
          "``` unison",
          "x = 1",
          "y = bad",
          "```",
          "",
          "``` unison",
          "z = 2",
          "w = worse",
          "```"
        ]
    outputOfRun =
      Text.unlines
        [ "# T",
          "",
          "``` unison",
          "x = 1",
          "y = bad",
          "```",
          "",
          "``` ucm :added-by-ucm",
          "  I couldn't figure out what bad refers to here:",
          "",
          "      2 | y = bad",
          "```",
          "",
          "``` unison",
          "z = 2",
          "w = worse",
          "```",
          "",
          "``` ucm :added-by-ucm",
          "  I couldn't figure out what worse refers to here:",
          "",
          "      2 | w = worse",
          "```",
          "",
          "``` ucm",
          "scratch/main> load other.u",
          "",
          "  I couldn't figure out what bad refers to here:",
          "",
          "      2 | foo = bad",
          "```"
        ]
    failedRun =
      Text.unlines
        [ "``` unison",
          "x = 1",
          "y = bad",
          "```",
          "",
          "🛑",
          "",
          "The transcript failed due to an error in the stanza above. The error is:",
          "",
          "```",
          "I couldn't figure out what bad refers to here:",
          "",
          "    2 | y = bad",
          "```"
        ]
    absolutized sourceText name outputText = do
      output <- parse outputText
      pure (Transcript.format (absolutizeLineNumbers name sourceText output))
