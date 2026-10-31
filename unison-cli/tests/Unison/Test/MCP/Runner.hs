-- | How transcripts behave when the MCP server runs them ('Transcript.mcpConfig'), compared with a transcript test.
module Unison.Test.MCP.Runner (test) where

import Control.Monad.IO.Class (liftIO)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import EasyTest
import System.IO.Temp qualified as Temp
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.Merge2 (lineMergedNames, lineMergedNote)
import Unison.Merge.ThreeWay qualified as Merge
import Unison.Syntax.Name qualified as Name
import Unison.Codebase.Init qualified as Codebase.Init
import Unison.Codebase.SqliteCodebase qualified as SC
import Unison.Codebase.Transcript (Transcript)
import Unison.Codebase.Transcript.Runner qualified as Transcript
import Unison.Codebase.Verbosity qualified as Verbosity
import Unison.Runtime.JIT qualified as JIT
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Control.Exception (finally)
import System.Directory (listDirectory, removeDirectoryRecursive, removeFile)

test :: Test ()
test =
  scope "mcp.runner" . tests $
    [ scope "an empty search is not a failure for the MCP server" do
        result <- runTranscriptWith mcp (ucm ["scratch/main> find nothing_is_called_this"])
        expect (not (failed result)),
      scope "an empty search stops a transcript test" do
        result <- runTranscriptWith testConfig (ucm ["scratch/main> find nothing_is_called_this"])
        expect (failed result),
      scope "listing an empty namespace is not a failure for the MCP server" do
        result <- runTranscriptWith mcp (ucm ["scratch/main> ls nothing_is_called_this"])
        expect (not (failed result)),
      scope "docs on a definition with no documentation is not a failure for the MCP server" do
        result <- runTranscriptWith mcp (undocumented <> ucm ["scratch/main> docs foo", "scratch/main> ls"])
        expect (not (failed result))
        either (crash . show) (expectTextContaining ["has no documentation"] . Text.pack . renderOutput) result,
      scope "docs on a definition with no documentation stops a transcript test" do
        result <- runTranscriptWith testConfig (undocumented <> ucm ["scratch/main> docs foo"])
        expect (failed result),
      scope "docs on a name that doesn't exist is still a failure for the MCP server" do
        result <- runTranscriptWith mcp (ucm ["scratch/main> docs nothing_is_called_this"])
        expect (failed result),
      scope "a project that doesn't exist is an error, with the projects that do" do
        result <- runTranscriptWith mcp (ucm ["myproject/main> ls"])
        expectFailureMentioning ["There is no project named `myproject`", "scratch"] result,
      scope "a project that doesn't exist is created for a transcript test" do
        result <- runTranscriptWith testConfig (ucm ["myproject/main> branches"])
        expect (not (failed result)),
      scope "a branch that doesn't exist is an error, with the branches that do" do
        result <- runTranscriptWith mcp (ucm ["scratch/nope> ls"])
        expectFailureMentioning ["has no branch named `nope`", "main"] result,
      scope "a branch created by an earlier command can be named" do
        result <- runTranscriptWith mcp (ucm ["scratch/main> branch alice", "scratch/alice> ls"])
        expect (not (failed result)),
      scope "a unison block right after an update that couldn't finish is checked" do
        result <- runTranscriptWith mcp updateThatCannotFinish
        case result of
          Left err -> crash (show err)
          Right output ->
            -- The block comes after the file that UCM wrote for the update, and it must get UCM's report of what it
            -- typechecked, like any other unison block.
            expectTextContaining ["I couldn't complete the update", "foo : Text"] (Text.pack (renderOutput output)),
      scope "the reply to a merge that stops says how long its file is, and how many conflicts it has" do
        result <- runTranscriptWith mcp conflictingMerge
        liftIO (listDirectory "." >>= traverse_ removeFile . filter (\file -> "merge-" `isPrefixOf` file && ".u" `isSuffixOf` file))
        expectFailureMentioning ["lines", "1 conflict,", "Search the file for"] result,
      scope "a definition both branches changed on different lines is named as merged at the line level" do
        let version a c = Text.unlines ["f x =", "  a = " <> a, "  b = 7", "  c = " <> c, "  a + b + c"]
            texts = Map.singleton (Name.unsafeParseText "f") <$> Merge.ThreeWay {Merge.alice = version "x + 10" "x + 2", Merge.bob = version "x + 1" "x + 20", Merge.lca = version "x + 1" "x + 2"}
        expect' (lineMergedNames texts == [Name.unsafeParseText "f"])
        expect'
          ( "successfully merged at the line-level: f" `Text.isInfixOf` lineMergedNote (lineMergedNames texts)
          ),
      scope "a definition both branches changed on the same line is left to the conflict markers" do
        let version a = Text.unlines ["f x =", "  " <> a]
            texts = Map.singleton (Name.unsafeParseText "f") <$> Merge.ThreeWay {Merge.alice = version "x + 10", Merge.bob = version "x + 20", Merge.lca = version "x + 1"}
        expect' (null (lineMergedNames texts))
        expect' (Text.null (lineMergedNote (lineMergedNames texts)))
    ]
  where
    mcp = const Transcript.mcpConfig
    testConfig = Transcript.testConfig
    failed = isLeft

undocumented :: Text
undocumented =
  Text.unlines
    [ "``` unison",
      "foo = 1",
      "```",
      "",
      "``` ucm",
      "scratch/main> add",
      "```",
      ""
    ]

ucm :: [Text] -> Text
ucm commands = "``` ucm\n" <> Text.unlines commands <> "```\n"

-- | Two branches change the same definition in different ways, and one is merged into the other.
conflictingMerge :: Text
conflictingMerge =
  Text.unlines
    [ "``` ucm",
      "scratch/main> builtins.merge lib.builtins",
      "```",
      "",
      "``` unison",
      "foo : Nat",
      "foo = 1",
      "```",
      "",
      "``` ucm",
      "scratch/main> add",
      "scratch/main> branch alice",
      "scratch/main> branch bob",
      "```",
      "",
      "``` unison",
      "foo : Nat",
      "foo = 2",
      "```",
      "",
      "``` ucm",
      "scratch/alice> update",
      "```",
      "",
      "``` unison",
      "foo : Nat",
      "foo = 3",
      "```",
      "",
      "``` ucm",
      "scratch/bob> update",
      "scratch/alice> merge /bob",
      "```"
    ]

-- | A definition is added, then changed to a type that its dependent can't use, so the update stops. The transcript goes on (the
-- failure is expected) with a unison block, which has to be loaded.
updateThatCannotFinish :: Text
updateThatCannotFinish =
  Text.unlines
    [ "``` ucm",
      "scratch/main> builtins.merge lib.builtins",
      "```",
      "",
      "``` unison",
      "foo : Nat",
      "foo = 1",
      "",
      "bar : Nat",
      "bar = foo",
      "```",
      "",
      "``` ucm",
      "scratch/main> add",
      "```",
      "",
      "``` unison",
      "foo : Text",
      "foo = \"x\"",
      "```",
      "",
      "``` ucm :error",
      "scratch/main> update",
      "```",
      "",
      "``` unison",
      "foo : Text",
      "foo = \"x\"",
      "",
      "bar : Text",
      "bar = foo",
      "```"
    ]

runTranscriptWith :: (FilePath -> Transcript.Config) -> Text -> Test (Either Transcript.Error Transcript)
runTranscriptWith config source = io do
  tmp <- Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "ucm-mcp-runner-test"
  (do
      jit <- JIT.newJIT =<< JIT.readConfig
      Transcript.withRunner jit (config tmp) Verbosity.Silent "Unison.Test.MCP.Runner" \runner -> do
        result <-
          Codebase.Init.withCreatedCodebase SC.init "ucm-mcp-runner-test" tmp SC.DoLock \codebase -> do
            Codebase.runTransaction codebase (Codebase.installUcmDependencies codebase)
            runner "transcript" (Text.encodeUtf8 source) codebase
        either (fail . show) pure result)
    `finally` removeDirectoryRecursive tmp

expectFailureMentioning :: [Text] -> Either Transcript.Error Transcript -> Test ()
expectFailureMentioning needles = \case
  Left (Transcript.RunFailure failure) -> expectTextContaining needles (Text.pack (renderOutput failure))
  Left other -> crash ("expected a run failure, got " <> show other)
  Right _ -> crash "expected the transcript to fail, but it succeeded"

expectTextContaining :: [Text] -> Text -> Test ()
expectTextContaining needles haystack =
  tests [expect' (needle `Text.isInfixOf` haystack) | needle <- needles]

renderOutput :: Transcript -> String
renderOutput = Text.unpack . Transcript.format
