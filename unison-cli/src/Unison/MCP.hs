-- | The MCP server: tools that run Unison transcripts, which differ in which UCM commands they allow.
module Unison.MCP (runOnStdIO) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Aeson
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Network.MCP.Server qualified as MCP
import Network.MCP.Transport.StdIO (newSTDIOTransport)
import Network.MCP.Transport.Types (Message (..), Response (..), Transport (..))
import Text.Megaparsec qualified as P
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Transcript
import Unison.Codebase.Transcript.Parser qualified as Transcript
import Unison.Codebase.Transcript.Runner qualified as Transcript
import Unison.MCP.Allowlist
import Unison.MCP.Output
import Unison.MCP.Wrapper
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..))
import Unison.Symbol (Symbol)

serverDescription :: Text
serverDescription =
  Text.unwords
    [ "Run Unison transcripts, which are Markdown documents with `ucm` code blocks (UCM commands, one per line, as in",
      "`myproject/main> view foo`) and `unison` code blocks (code to typecheck). Anything you can do at the UCM command",
      "line you can do here. The result is the transcript with UCM's output filled in after each command.",
      "The tools differ only in which commands they allow: `transcript-read-only` can't change anything, `transcript`",
      "can make changes that are easy to undo, `transcript-io` can also run code, and `transcript-all` allows",
      "everything, including deleting projects and branches and pushing to Share."
    ]

newtype TranscriptArgs = TranscriptArgs {transcript :: Text}

instance FromJSON TranscriptArgs where
  parseJSON = Aeson.withObject "TranscriptArgs" \o -> TranscriptArgs <$> o .: "transcript"

instance HasInputSchema TranscriptArgs where
  toInputSchema _ =
    Aeson.object
      [ ("type", Aeson.String "object"),
        ( "properties",
          Aeson.object
            [ ( "transcript",
                Aeson.object
                  [ ("type", Aeson.String "string"),
                    ("description", Aeson.String "A Unison transcript: Markdown with ucm and unison code blocks.")
                  ]
              )
            ]
        ),
        ("required", Aeson.toJSON ["transcript" :: Text])
      ]

-- | Run the MCP server until we hit EOF.
runOnStdIO :: Transcript.Runner -> Codebase IO Symbol Ann -> IO ()
runOnStdIO runner codebase =
  withOutputDir \outputDir -> do
    -- Transcripts run one at a time: they share the codebase, and they take over stdout while they run.
    lock <- newMVar ()
    let run tier args = withMVar lock \() -> runTranscript runner codebase outputDir tier args
    server <- mkServer (Implementation "unison-mcp" "0.0.1") serverDescription (transcriptTool run <$> [minBound .. maxBound])
    transport <- newSTDIOTransport
    handleMessages transport (fmap (fmap renameAnnotations) . MCP.handleMessage server)

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

transcriptTool :: (Tier -> TranscriptArgs -> IO CallToolResult) -> Tier -> Tool IO
transcriptTool run tier =
  Tool
    { toolName = Unison.MCP.Allowlist.toolName tier,
      toolDescription = description,
      toolAnnotations = annotations,
      toolArgType = Proxy,
      toolHandler = run tier
    }
  where
    (description, annotations) = case tier of
      ReadOnly ->
        ( "Run a transcript that doesn't change the codebase: searching, viewing, docs, diffs, history, tests, and "
            <> "reading from Share (`share.project-search`, `share.source`, `share.render`). Every project and branch "
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

runTranscript :: Transcript.Runner -> Codebase IO Symbol Ann -> OutputDir -> Tier -> TranscriptArgs -> IO CallToolResult
runTranscript runner codebase outputDir tier TranscriptArgs {transcript = source} =
  case Transcript.parse name (Text.encodeUtf8 source) of
    Left err -> pure (errorToolResult ("The transcript didn't parse:\n" <> Text.pack (P.errorBundlePretty err)))
    Right parsed -> do
      missing <- if tier == ReadOnly then missingContexts codebase parsed else pure []
      case validate tier parsed ++ missing of
        violations@(_ : _) -> pure (errorToolResult (renderViolations tier violations))
        [] -> do
          (stdout, stderr, result) <- captureHandles (runner name (Text.encodeUtf8 source) codebase)
          let withProgramOutput rendered =
                rendered
                  <> section "Standard output" stdout
                  <> section "Standard error" stderr
          case result of
            Right done -> textToolResult <$> deliver outputDir (withProgramOutput (Transcript.format done))
            Left (Transcript.RunFailure failed) -> errorToolResult <$> deliver outputDir (withProgramOutput (Transcript.format failed))
            Left (Transcript.ParseError err) -> pure (errorToolResult (Text.pack (P.errorBundlePretty err)))
            Left Transcript.PortBindingFailure -> pure (errorToolResult "The codebase server failed to start: no port was available.")
            Left (Transcript.Exception err) -> pure (errorToolResult ("The transcript failed with an exception: " <> tShow err))
  where
    name = "transcript.md"
    section heading text
      | Text.null (Text.strip text) = ""
      | otherwise = "\n" <> heading <> " of the transcript:\n\n" <> text

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
