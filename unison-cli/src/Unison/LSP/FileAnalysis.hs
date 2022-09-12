{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.FileAnalysis where

import Control.Lens
import Control.Monad.Reader
import qualified Crypto.Random as Random
import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens (HasUri (uri))
import Unison.Codebase.Editor.HandleInput (typecheckHelper)
import qualified Unison.Debug as Debug
import Unison.LSP.Diagnostics
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import Unison.Prelude
import qualified Unison.Result as Result
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.UnisonFile as UF
import UnliftIO

-- | Lex, parse, and typecheck a file.
analyseFile :: HasUri d Uri => d -> Lsp (Maybe FileAnalysis)
analyseFile doc = runMaybeT $ do
  let fileUri = doc ^. uri
  (fileVersion, contents) <- MaybeT (VFS.getFileContents doc)
  parseNames <- lift getParseNames
  let sourceName = getUri $ doc ^. uri
  let lexedSource = (contents, L.lexer (Text.unpack sourceName) (Text.unpack contents))
  let ambientAbilities = []
  cb <- asks codebase
  let generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG
  r <- (liftIO $ typecheckHelper cb generateUniqueName ambientAbilities parseNames sourceName lexedSource)
  let Result.Result notes mayResult = r
  case mayResult of
    Nothing -> pure $ FileAnalysis {parsedFile = Nothing, typecheckedFile = Nothing, ..}
    Just (Left uf) -> pure $ FileAnalysis {parsedFile = Just uf, typecheckedFile = Nothing, ..}
    Just (Right tf) -> pure $ FileAnalysis {parsedFile = Just (UF.discardTypes tf), typecheckedFile = Just tf, ..}

fileAnalysisWorker :: Lsp ()
fileAnalysisWorker = forever do
  dirtyFilesV <- asks dirtyFilesVar
  checkedFilesV <- asks checkedFilesVar
  -- We may want to debounce this if it typechecks too eagerly,
  -- but typechecking is pretty fast right now when scratch files are small
  dirtyFileIDs <- atomically $ do
    dirty <- readTVar dirtyFilesV
    writeTVar dirtyFilesV mempty
    guard $ not $ null dirty
    pure dirty
  freshlyCheckedFiles <-
    Map.fromList <$> forMaybe (toList dirtyFileIDs) \docUri -> runMaybeT do
      fileInfo <- MaybeT (analyseFile $ TextDocumentIdentifier docUri)
      pure (docUri, fileInfo)
  Debug.debugM Debug.LSP "Typechecked " freshlyCheckedFiles
  -- Overwrite any files we successfully checked
  atomically $ modifyTVar' checkedFilesV (`Map.union` freshlyCheckedFiles)
  for freshlyCheckedFiles \info -> do
    diagnostics <- infoDiagnostics info
    reportDiagnostics (fileUri info) (Just $ fileVersion info) $ diagnostics
