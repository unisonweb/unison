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
  LSP.Uri ->
  -- | Fully qualified name of the definition to edit
  Text ->
  -- Returns 'True' if the definition was added to the file, False if it was already present
  ExceptT Text Lsp Bool
editDefinitionByFQN fileURI fqn = do
  Env {codebase} <- ask
  nameSearch <- getNameSearch
  lastTouchedFileV <- asks lastTouchedFileVar
  mayLastTouchedFile <- liftIO $ atomically $ readTVar lastTouchedFileV
  (mayUnisonFile, fileUri, fp) <- case mayLastTouchedFile of
    Nothing -> pure (Nothing, fileURI, fromMaybe "scratch.u" $ uriToFilePath fileURI)
    Just uri -> do
      mayTypecheckedFile <- runMaybeT do
        FileAnalysis {parsedFile, typecheckedFile} <- FA.getFileAnalysis uri
        hoistMaybe (Right <$> typecheckedFile <|> Left <$> parsedFile)
      pure (mayTypecheckedFile, uri, fromMaybe "scratch.u" $ uriToFilePath uri)
  parsedFQN <- case Names.parseTextEither fqn of
    Left err -> throwError err
    Right parsedFQN -> do
      pure parsedFQN
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
                (WorkspaceEdit (Just ((Map.singleton fileUri [TextEdit range (rendered <> "\n\n")]))) Nothing Nothing)
        Debug.debugM Debug.LSP "Applying workspace edit for editDefinitionByFQN" params
        void $ sendRequest Msg.SMethod_WorkspaceApplyEdit params $ \case
          Left err -> Debug.debugM Debug.LSP "Error applying workspace edit" err
          Right _ -> pure ()
  renderToFile codebase appendText mayUnisonFile fp WithinFold pped termResults typeResults
