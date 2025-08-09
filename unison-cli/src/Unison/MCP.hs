module Unison.MCP (runOnStdIO, initServer) where

import Network.MCP.Server qualified as MCP
import Network.MCP.Server.StdIO qualified as MCP
import Network.MCP.Types
import Text.RawString.QQ (r)
import Unison.Auth.CredentialManager qualified as AuthN
import Unison.Auth.HTTPClient qualified as AuthN
import Unison.Auth.Tokens qualified as AuthN
import Unison.Codebase (Codebase)
import Unison.Codebase.Runtime (Runtime)
import Unison.MCP.Prompts (prompts)
import Unison.MCP.StaticResources (staticResources)
import Unison.MCP.Tools (tools)
import Unison.MCP.Types
import Unison.MCP.Wrapper qualified as MCPWrapper
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Symbol (Symbol)

serverDescription :: Text
serverDescription =
  [r|
        This server provides tools for interacting with Unison Code locally, such as typechecking or reading documentation, as well as tools for searching Unison Share, which is a platform for sharing Unison projects and libraries.

        It also provides some mechanisms for editing and updating local Unison projects, such as installing libraries from Unison Share.

        Before doing any work in unison please read the file://unison-guide resource for information on how to write unison.
    |]

initServer :: Codebase IO Symbol Ann -> Runtime Symbol -> Runtime Symbol -> Maybe FilePath -> Text -> IO MCP.Server
initServer codebase runtime sbRuntime workDir ucmVersion = do
  credMan <- AuthN.newCredentialManager
  let tokenProvider :: AuthN.TokenProvider
      tokenProvider = AuthN.newTokenProvider credMan
  authenticatedHTTPClient <- AuthN.newAuthenticatedHTTPClient tokenProvider ucmVersion
  let env =
        Env
          { codebase,
            runtime,
            sbRuntime,
            ucmVersion,
            workDir,
            authenticatedHTTPClient
          }
  -- Create server
  let serverInfo = Implementation "unison-mcp" "0.0.1"

  runMCP env $ MCPWrapper.mkServer serverInfo serverDescription staticResources tools prompts

-- | Run the MCP server until we hit EOF.
runOnStdIO :: Codebase IO Symbol Ann -> Runtime Symbol -> Runtime Symbol -> FilePath -> Text -> IO ()
runOnStdIO codebase runtime sbRuntime workDir ucmVersion = do
  server <- initServer codebase runtime sbRuntime (pure workDir) ucmVersion
  -- Start the server with StdIO transport
  MCP.runServerWithSTDIO server
