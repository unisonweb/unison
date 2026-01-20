-- LSP wrappers for common Unison functionality
module Unison.LSP.Util.Wrappers (editDefinitionByFQN) where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map qualified as Map
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (sendRequest)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.HandleInput.ShowDefinition (renderToFile)
import Unison.Codebase.Editor.Input (RelativeToFold (..))
import Unison.Debug qualified as Debug
import Unison.HashQualified qualified as HQ
import Unison.LSP.FileAnalysis qualified as FA
import Unison.LSP.Types
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.Server.Backend qualified as Backend
import Unison.Syntax.Name qualified as Names

editDefinitionByFQN ::
  Maybe LSP.Uri ->
  -- | Fully qualified name of the definition to edit
  Text ->
  ExceptT Text Lsp ()
editDefinitionByFQN mayFileURI fqn = do
  Env {codebase} <- ask
  nameSearch <- getNameSearch
  lastTouchedFileV <- asks lastTouchedFileVar
  mayLastTouchedFile <- liftIO $ atomically $ readTVar lastTouchedFileV
  Debug.debugM Debug.Temp "editDefinitionByFQN: Last touched file:" mayLastTouchedFile
  (mayUnisonFile, fileUri, fp) <- case mayLastTouchedFile of
    Nothing -> pure (Nothing, fromMaybe (filePathToUri "scratch.u") mayFileURI, fromMaybe "scratch.u" $ mayFileURI >>= uriToFilePath)
    Just uri -> do
      mayTypecheckedFile <- runMaybeT do
        FileAnalysis {parsedFile, typecheckedFile} <- FA.getFileAnalysis uri
        hoistMaybe (Right <$> typecheckedFile <|> Left <$> parsedFile)
      pure (mayTypecheckedFile, uri, fromMaybe "scratch.u" $ uriToFilePath uri)
  Debug.debugM Debug.Temp "editDefinitionByFQN: Using file info:" (fileUri, fp)
  parsedFQN <- case Names.parseTextEither fqn of
    Left err -> throwError err
    Right parsedFQN -> do
      pure parsedFQN
  Debug.debugM Debug.Temp "editDefinitionByFQN: Searching for FQN:" parsedFQN
  Backend.DefinitionResults {termResults, typeResults} <- liftIO $ do
    Codebase.runTransaction codebase $ Backend.definitionsByName codebase nameSearch Backend.IncludeCycles Names.ExactName [HQ.NameOnly parsedFQN]
  pped <- currentPPED
  toIO <- lift $ askRunInIO
  let appendText _fp rendered _aboveFold = toIO $ do
        let range = Range (Position 0 0) (Position 0 0)
        let description = "Edit definition: " <> fqn
        let params =
              ApplyWorkspaceEditParams
                (Just description)
                (WorkspaceEdit (Just ((Map.singleton fileUri [TextEdit range rendered]))) Nothing Nothing)
        Debug.debugM Debug.LSP "Applying workspace edit for editDefinitionByFQN" params
        void $ sendRequest Msg.SMethod_WorkspaceApplyEdit params $ \case
          Left err -> Debug.debugM Debug.LSP "Error applying workspace edit" err
          Right _ -> pure ()
  void $ renderToFile codebase appendText mayUnisonFile fp WithinFold pped termResults typeResults
