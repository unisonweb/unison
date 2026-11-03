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
import Unison.MCP.Echo
import Unison.MCP.LineNumbers
import Unison.MCP.Output

test :: Test ()
test =
  scope "mcp" . tests $
    [ allowlistTests,
      shareRefTests,
      searchWordsTests,
      lineNumberTests,
      gutterAlignmentTests,
      echoTests,
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
      scope "says when the command doesn't exist, instead of naming a tool" do
        transcript <- parse (ucm ["p/b> project.current", "p/b> push"])
        expectEqual [True, False] [unknownCommand | Violation {unknownCommand} <- validate Local transcript]
        expectEqual
          "Nothing was run: `transcript` doesn't allow everything in this transcript.\n\n  code block 1, line 1: `project.current` isn't a UCM command (`help` lists them)\n  code block 1, line 2: `push` needs `transcript-all`\n"
          (renderViolations Local (validate Local transcript)),
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
    [ scope "the longest reply is returned as it is" do
        out <- io (withOutputDir \dir -> deliver dir (Text.unlines (replicate maxInlineLines "line")))
        expectEqual (Text.unlines (replicate maxInlineLines "line")) out,
      scope "blank lines don't count towards the limit" do
        let spaced = Text.unlines (replicate maxInlineLines "line\n   ")
        out <- io (withOutputDir \dir -> deliver dir spaced)
        expectEqual spaced out,
      scope "one more line goes to a file, and only the path is returned" do
        let long = Text.unlines (replicate (maxInlineLines + 1) "line")
        (message, contents) <-
          io $ withOutputDir \dir -> do
            message <- deliver dir long
            let path = Text.unpack . fst . Text.breakOn ". Read or grep" . Text.drop (Text.length "written to ") . snd $ Text.breakOn "written to " message
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

-- | A `unison` block of 12 lines.
twelveLines :: [Text]
twelveLines = [Text.pack ("x" <> show i <> " = 1") | i <- [1 .. 12 :: Int]]

-- | A source whose only `unison` block has its code start on the given line.
sourceWithBlockAt :: Int -> Text
sourceWithBlockAt line =
  Text.unlines (replicate (line - 2) "text" ++ ["``` unison"] ++ twelveLines ++ ["```"])

-- | The output of running a source that has one `unison` block, with the given lines in the message about it.
outputWithMessage :: [Text] -> Text
outputWithMessage messageLines =
  Text.unlines (["``` unison"] ++ twelveLines ++ ["```", "", "``` ucm :added-by-ucm"] ++ (("  " <>) <$> messageLines) ++ ["```"])

gutterAlignmentTests :: Test ()
gutterAlignmentTests =
  scope "gutter alignment" . tests $
    [ rewrites "more digits that fit before the |" 1000 ["      2 | x = bad"] "   1001 | x = bad",
      rewrites "more digits than fit before the |, which then move right" 1000 ["2 | x = bad"] "1001 | x = bad",
      rewrites
        "lines that were lined up stay lined up when the digits grow"
        100
        [" 9 | a", "10 | b"]
        "108 | a\n  109 | b",
      rewrites
        "lines that were lined up stay lined up when only some of the numbers grow"
        8
        ["      2 | a", "      3 | b"]
        "      9 | a\n       10 | b",
      rewrites "the line just past the end of the code, where UCM points when the code ends too soon" 10 ["     13 | "] "     22 | ",
      scope "a number further past the end is not a line of the block" do
        output <- absolutized (sourceWithBlockAt 10) "your transcript" (outputWithMessage ["     14 | not a line"])
        expect ("     14 | not a line" `Text.isInfixOf` output)
    ]
  where
    rewrites name start messageLines expected = scope name do
      output <- absolutized (sourceWithBlockAt start) "your transcript" (outputWithMessage messageLines)
      expect (expected `Text.isInfixOf` output)
    absolutized sourceText name outputText = do
      out <- parse outputText
      pure (Transcript.format (absolutizeLineNumbers name sourceText out))

echoTests :: Test ()
echoTests =
  scope "echo" . tests $
    [ scope "a long unison block is replaced by a note of where it is" do
        output <- elided source run
        expect ("-- code block 2: your 12 lines, lines 6-17 of your transcript" `Text.isInfixOf` output)
        expect (not ("x5 = 5" `Text.isInfixOf` output)),
      scope "a short unison block, and the commands, are kept" do
        output <- elided source run
        expect ("short = 1" `Text.isInfixOf` output)
        expect ("scratch/main> projects" `Text.isInfixOf` output),
      scope "what UCM said about the block is kept" do
        output <- elided source run
        expect ("  + x1 : Nat" `Text.isInfixOf` output),
      scope "a note says how to get the blocks back" do
        output <- elided source run
        expect ("Set `echo` to true to include them." `Text.isInfixOf` output)
        expect (not ("\\`" `Text.isInfixOf` output)),
      scope "the tags of the block are kept" do
        output <- elided source (Text.replace "``` unison\nx1" "``` unison :hide\nx1" run)
        expect ("``` unison :hide\n-- code block 2" `Text.isInfixOf` output),
      scope "no note when nothing was left out" do
        output <- elided "``` unison\nshort = 1\n```\n" "``` unison\nshort = 1\n```\n"
        expect (not ("Set `echo`" `Text.isInfixOf` output)),
      scope "exactly the limit is kept" do
        let ten = Text.unlines (["``` unison"] ++ [Text.pack ("x" <> show i <> " = 1") | i <- [1 .. maxEchoedLines]] ++ ["```"])
        output <- elided ten ten
        expect ("x1 = 1" `Text.isInfixOf` output),
      scope "a unison block that UCM wrote (from edit) is never left out" do
        let generatedBlock = Text.unlines (["``` unison :added-by-ucm"] ++ [Text.pack ("y" <> show i <> " = 1") | i <- [1 .. 20 :: Int]] ++ ["```"])
        output <- elided source (run <> "\n" <> generatedBlock)
        expect ("y15 = 1" `Text.isInfixOf` output)
    ]
  where
    -- 12 lines of code, starting on line 6, then 1 line.
    source =
      Text.unlines $
        ["``` ucm", "scratch/main> projects", "```", "", "``` unison"]
          ++ [Text.pack ("x" <> show i <> " = " <> show i) | i <- [1 .. 12 :: Int]]
          ++ ["```", "", "``` unison", "short = 1", "```"]
    run =
      Text.unlines $
        ["``` ucm", "scratch/main> projects", "", "  1. scratch", "```", "", "``` unison"]
          ++ [Text.pack ("x" <> show i <> " = " <> show i) | i <- [1 .. 12 :: Int]]
          ++ ["```", "", "``` ucm :added-by-ucm", "  + x1 : Nat", "```", "", "``` unison", "short = 1", "```", "", "``` ucm :added-by-ucm", "  + short : Nat", "```"]
    elided sourceText outputText = do
      out <- parse outputText
      pure (Transcript.format (elideLongBlocks maxEchoedLines "your transcript" sourceText out))
