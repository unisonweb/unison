module Unison.CommandLine.Server.Impl (server) where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as BSC
import Servant.Server
import Unison.Cli.Monad
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput (handleInput)
import qualified Unison.CommandLine as CommandLine
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.CommandLine.Server.API
import Unison.CommandLine.Server.Types
import qualified Unison.Util.Pretty as Pretty
import UnliftIO.STM

runCommand :: RunCommandRequest -> CommandLineServer RunCommandResponse
runCommand RunCommandRequest {namespace, args, command} = do
  let numberedArgs = []
  rootVar <- gets root
  let getRoot = Branch.head <$> (atomically $ readTMVar rootVar)
  input <-
    liftIO (CommandLine.parseInput getRoot namespace numberedArgs IP.patternMap (command : args)) >>= \case
      Left err -> throwError (err400 {errBody = BSC.pack $ Pretty.toPlain 120 err})
      Right input -> pure input
  RunCommandResponse <$> liftCli (handleInput (Right input))

server :: ServerT CommandLineAPI CommandLineServer
server = runCommand
