-- | The MCP server: tools that run Unison transcripts, which differ in which UCM commands they allow.
module Unison.MCP (runOnStdIO, withMessageHandler) where

import CMark qualified
import Control.Concurrent.MVar (newMVar, withMVar)
import Data.Aeson (FromJSON (..), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Network.MCP.Server qualified as MCP
import Network.MCP.Transport.StdIO (newSTDIOTransport)
import Network.MCP.Transport.Types (Message (..), Response (..), Transport (..))
import Text.Megaparsec qualified as P
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.Transcript
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Transcript.Runner qualified as Transcript
import Unison.MCP.Allowlist
import Unison.MCP.Echo
import Unison.MCP.Learn qualified as Learn
import Unison.MCP.LineNumbers
import Unison.MCP.Output
import Unison.MCP.Wrapper
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))
import Unison.Symbol (Symbol)
import UnliftIO qualified

-- | The transcript to run, given either as text or as the path of a file that has it.
data TranscriptArgs = TranscriptArgs
  { transcript :: Maybe Text,
    transcriptFile :: Maybe FilePath,
    -- | Repeat the long `unison` blocks in the reply too.
    echo :: Bool
  }

instance FromJSON TranscriptArgs where
  parseJSON = Aeson.withObject "TranscriptArgs" \o ->
    TranscriptArgs <$> o .:? "transcript" <*> o .:? "transcript-file" <*> (fromMaybe False <$> o .:? "echo")

instance HasInputSchema TranscriptArgs where
  toInputSchema _ =
    Aeson.object
      [ ("type", Aeson.String "object"),
        ( "properties",
          Aeson.object
            [ ( "transcript",
                Aeson.object
                  [ ("type", Aeson.String "string"),
                    ("description", Aeson.String "A Unison transcript: Markdown with ucm and unison code blocks. Give this or transcript-file.")
                  ]
              ),
              ( "transcript-file",
                Aeson.object
                  [ ("type", Aeson.String "string"),
                    ( "description",
                      Aeson.String
                        "The path of a file containing a Unison transcript, relative to the server's working directory. Preferred for anything long: when something goes wrong you can edit just the part that needs fixing and run the file again, without sending the whole transcript each time. The file is not changed."
                    )
                  ]
              ),
              ( "echo",
                Aeson.object
                  [ ("type", Aeson.String "boolean"),
                    ( "description",
                      Aeson.String
                        "Include the code of long unison blocks (more than 10 lines) in the reply. By default they are left out, since you just wrote them; the reply says which lines of the transcript they are."
                    )
                  ]
              )
            ]
        )
      ]

-- | Run the MCP server until we hit EOF.
runOnStdIO :: Transcript.Runner -> Codebase IO Symbol Ann -> IO ()
runOnStdIO runner codebase =
  withMessageHandler runner codebase \handle -> do
    transport <- newSTDIOTransport
    handleMessages transport handle

-- | The MCP server as a function from a message to the reply, if there is one, for a transport to call.
withMessageHandler :: Transcript.Runner -> Codebase IO Symbol Ann -> ((Message -> IO (Maybe Message)) -> IO a) -> IO a
withMessageHandler runner codebase action =
  withOutputDir \outputDir -> do
    -- Transcripts run one at a time: they share the codebase, and they take over stdout while they run.
    lock <- newMVar ()
    let run tier args = withMVar lock \() -> runTranscript runner codebase outputDir tier args
    server <-
      mkServer
        (Implementation "unison-mcp" "0.0.1")
        (Learn.instructions Learn.embeddedGuide)
        (learnTool : (transcriptTool run <$> [minBound .. maxBound]))
    action (fmap (fmap renameAnnotations) . MCP.handleMessage server)

-- | hs-mcp writes a tool's annotations as @readOnly@, @destructive@, @idempotent@ and @openWorld@, but the MCP spec calls
-- them @readOnlyHint@, @destructiveHint@, @idempotentHint@ and @openWorldHint@, and clients only look for those. So
-- rename them on the way out. (Drop this when hs-mcp is fixed.)
renameAnnotations :: Message -> Message
renameAnnotations = \case
  ResponseMessage response@Response {responseResult = Just result} ->
    ResponseMessage response {responseResult = Just (go result)}
  message -> message
  where
    go = \case
      Aeson.Object o -> Aeson.Object (KeyMap.mapWithKey (\key value -> if key == "annotations" then renameKeys value else go value) o)
      Aeson.Array xs -> Aeson.Array (go <$> xs)
      value -> value
    renameKeys = \case
      Aeson.Object o -> Aeson.Object (KeyMap.mapKeyVal rename id o)
      value -> value
    rename key
      | key `elem` ["readOnly", "destructive", "idempotent", "openWorld"] = key <> Key.fromText "Hint"
      | otherwise = key

-- | The topic to look up.
newtype LearnArgs = LearnArgs {path :: Text}

instance FromJSON LearnArgs where
  parseJSON = Aeson.withObject "LearnArgs" \o ->
    LearnArgs . fromMaybe "" <$> o .:? "path"

instance HasInputSchema LearnArgs where
  toInputSchema _ =
    Aeson.object
      [ ("type", Aeson.String "object"),
        ( "properties",
          Aeson.object
            [ ( "path",
                Aeson.object
                  [ ("type", Aeson.String "string"),
                    ( "description",
                      Aeson.String
                        "The path of a topic, like `ucm/merge` or `testing`. Add `/detail` for the long version, like `ucm/merge/detail`. Leave it empty for an overview of all the topics."
                    )
                  ]
              )
            ]
        )
      ]

-- | Look up how to use Unison and this server. The topics are in @docs/mcp/learn.md@.
learnTool :: Tool IO
learnTool =
  Tool
    { toolName = "learn",
      toolDescription =
        "Explains how to use Unison and UCM, and how to write transcripts for the other tools. Give it the path of a topic, "
          <> "like `ucm/merge`, `testing` or `lang/cases`; it replies in Markdown with a summary of the topic and an index of "
          <> "its subtopics. Add `/detail` to the path for the long version, like `ucm/merge/detail`. An empty path gives an "
          <> "overview of all the topics. Links in the replies like `learn:ucm/merge` mean a topic: pass what comes after `learn:` as the path.",
      toolAnnotations =
        ToolAnnotations
          { title = Just "Learn about Unison and UCM",
            readOnlyHint = Just True,
            destructiveHint = Just False,
            idempotentHint = Just True,
            openWorldHint = Just False
          },
      toolArgType = Proxy,
      toolHandler = \LearnArgs {path} ->
        pure case Learn.learn Learn.embeddedGuide path of
          Right text -> textToolResult text
          Left problem -> errorToolResult problem
    }

transcriptTool :: (Tier -> TranscriptArgs -> IO CallToolResult) -> Tier -> Tool IO
transcriptTool run tier =
  Tool
    { toolName = Unison.MCP.Allowlist.toolName tier,
      toolDescription = description <> " " <> howToGiveTheTranscript,
      toolAnnotations = annotations,
      toolArgType = Proxy,
      toolHandler = run tier
    }
  where
    (description, annotations) = case tier of
      ReadOnly ->
        ( "Run a transcript that doesn't change the codebase: searching, viewing, docs, diffs, history, tests, and "
            <> "reading from Share (`share.search`, `share.project-search`, `share.source`, `share.render`). Every project and branch "
            <> "the transcript names must already exist.",
          ToolAnnotations
            { title = Just "Run a read-only transcript",
              readOnlyHint = Just True,
              destructiveHint = Just False,
              idempotentHint = Just True,
              openWorldHint = Just True
            }
        )
      Local ->
        ( "Run a transcript that may change the codebase in ways that are easy to undo (`undo` or the reflog bring "
            <> "them back): adding code with `update`, merging, creating and renaming branches and projects, "
            <> "`lib.install`, `pull`, and deleting definitions. It can't run code or delete a project or branch.",
          ToolAnnotations
            { title = Just "Run a transcript",
              readOnlyHint = Just False,
              destructiveHint = Just False,
              idempotentHint = Just False,
              openWorldHint = Just True
            }
        )
      IO ->
        ( "Run a transcript that may also run code, with `run`, `io.test` and the like. Code can do anything IO can.",
          ToolAnnotations
            { title = Just "Run a transcript that can run code",
              readOnlyHint = Just False,
              destructiveHint = Just True,
              idempotentHint = Just False,
              openWorldHint = Just True
            }
        )
      All ->
        ( "Run a transcript with every UCM command allowed, including deleting projects and branches and pushing to "
            <> "Share.",
          ToolAnnotations
            { title = Just "Run a transcript with no restrictions",
              readOnlyHint = Just False,
              destructiveHint = Just True,
              idempotentHint = Just False,
              openWorldHint = Just True
            }
        )

-- | Added to every tool's description, so agents find `transcript-file` without having to read the schema.
howToGiveTheTranscript :: Text
howToGiveTheTranscript =
  "New to this? The `learn` tool explains the transcript format and how to use UCM and Unison. "
    <> "Give the transcript as `transcript` (its text) or, preferably for anything long, as `transcript-file` (the path of "
    <> "a file that has it), so that when something goes wrong you can edit just the part that needs fixing and run it again."

runTranscript :: Transcript.Runner -> Codebase IO Symbol Ann -> OutputDir -> Tier -> TranscriptArgs -> IO CallToolResult
runTranscript runner codebase outputDir tier TranscriptArgs {transcript, transcriptFile, echo} =
  loadTranscript >>= \case
    Left problem -> pure (errorToolResult problem)
    Right (name, source, hint) -> do
      let sourceName = if isJust transcriptFile then Text.pack name else "your transcript"
      -- Every reply says which branch the codebase is on, since a transcript can end on another one.
      let finish toolResult text = do
            note <- currentBranchNote codebase
            pure (toolResult (text <> hint <> note))
          ok = finish textToolResult
          failed = finish errorToolResult
      let sourceText = Text.decodeUtf8With Text.lenientDecode source
          -- The numbers have to be fixed first, while the blocks are still whole.
          absolutize =
            (if echo then id else elideLongBlocks maxEchoedLines sourceName sourceText)
              . absolutizeLineNumbers sourceName sourceText
      case Transcript.parse name source of
        Left err -> failed ("The transcript didn't parse:\n" <> Text.pack (P.errorBundlePretty err))
        Right parsed -> do
          missing <- if tier == ReadOnly then missingContexts codebase parsed else pure []
          case validate tier parsed ++ missing of
            violations@(_ : _) -> failed (renderViolations tier violations)
            [] -> do
              (stdout, stderr, result) <- captureHandles (runner name source codebase)
              let withProgramOutput rendered =
                    rendered
                      <> section "Standard output" stdout
                      <> section "Standard error" stderr
              case result of
                Right done -> deliver outputDir (withProgramOutput (Transcript.format (absolutize done))) >>= ok
                Left (Transcript.RunFailure failure) -> do
                  -- The error is what the client most needs, so it always comes back inline, even when the transcript before it
                  -- is long enough to go to a file.
                  let (body, trailer) = splitFailure (absolutize failure)
                  shown <- deliver outputDir (withProgramOutput (Transcript.format body))
                  failed (Text.stripEnd shown <> "\n\n" <> Transcript.format trailer)
                Left (Transcript.ParseError err) -> failed (Text.pack (P.errorBundlePretty err))
                Left Transcript.PortBindingFailure -> failed "The codebase server failed to start: no port was available."
                Left (Transcript.Exception err) -> failed ("The transcript failed with an exception: " <> tShow err)
  where
    -- The transcript's name (for error messages), its contents, and what to add to the reply.
    loadTranscript :: IO (Either Text (String, ByteString, Text))
    loadTranscript =
      case (transcript, transcriptFile) of
        (Just text, Nothing) ->
          pure . Right $
            ( "transcript.md",
              Text.encodeUtf8 text,
              if length (Text.lines text) > maxInlineLines then preferFileHint else ""
            )
        (Nothing, Just path) ->
          UnliftIO.tryIO (BS.readFile path) <&> \case
            Left err -> Left ("I couldn't read the transcript file " <> Text.pack path <> ": " <> tShow err)
            Right contents -> Right (path, contents, "")
        (Just _, Just _) -> pure (Left "Give either `transcript` or `transcript-file`, not both.")
        (Nothing, Nothing) -> pure (Left "Give the transcript as `transcript` (its text) or `transcript-file` (the path of a file that has it).")
    section heading text
      | Text.null (Text.strip text) = ""
      | otherwise = "\n" <> heading <> " of the transcript:\n\n" <> text
    preferFileHint =
      Text.unlines
        [ "",
          "",
          "Note: this transcript was sent inline and is more than " <> tShow maxInlineLines <> " lines long. Prefer `transcript-file`: write the",
          "transcript to a file and pass its path. When something goes wrong you can then edit just the part that needs",
          "fixing and run the file again, without spending tokens on sending the whole transcript each time."
        ]

-- | A failed transcript is the stanzas that ran, then a marker (🛑) and what went wrong. Split them there.
splitFailure :: Transcript -> (Transcript, Transcript)
splitFailure Transcript {frontmatter, stanzas} =
  (Transcript frontmatter body, Transcript Aeson.Null trailer)
  where
    (body, trailer) = break isMarker stanzas
    isMarker = \case
      Left (CMark.Node _ CMark.PARAGRAPH [CMark.Node _ (CMark.TEXT "🛑") _]) -> True
      _ -> False

-- | The runner creates a project or branch that a command's context (like @myproject/main>@) names if it doesn't exist,
-- which is a change, so for a tool that makes none, they have to exist already.
missingContexts :: Codebase IO Symbol Ann -> Transcript -> IO [Violation]
missingContexts codebase transcript =
  fmap catMaybes . for contexts $ \(location, command, ProjectAndBranch projectName branchName) -> do
    exists <- Codebase.runTransaction codebase do
      Queries.loadProjectByName projectName >>= \case
        Nothing -> pure False
        Just project -> isJust <$> Queries.loadProjectBranchByName project.projectId branchName
    pure if exists then Nothing else Just (Violation location command Local)
  where
    contexts =
      [ ("code block " <> tShow n <> ", line " <> tShow i, command, projectAndBranch)
      | (n, Ucm _ lines) <- zip [1 :: Int ..] [block | Right block <- stanzas transcript],
        (i, UcmCommand (UcmContextProject projectAndBranch) command) <- zip [1 :: Int ..] lines
      ]

-- | A line that says which project and branch the codebase is on, for the end of a reply. On the branch that an update, upgrade
-- or merge that couldn't finish moved to, it also says how to get off it.
currentBranchNote :: Codebase IO Symbol Ann -> IO Text
currentBranchNote codebase =
  UnliftIO.tryAny (Codebase.runTransaction codebase Codebase.expectCurrentProjectPath) <&> \case
    Left _ -> ""
    Right ProjectPath {project, branch} ->
      "\n\nCurrent branch: "
        <> into @Text project.name
        <> "/"
        <> into @Text branch.name
        <> if branch.isUpdate || branch.isUpgrade || branch.isMerge
          then
            "\nThis is a temporary branch for work in progress. When its file loads with no errors, run `update` to save the "
              <> "work and delete the branch, or run `cancel` to abandon it."
          else ""
