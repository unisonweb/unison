{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.VFS where

import qualified Colog.Core as Colog
import Control.Lens
import qualified Control.Lens as Lens
import Control.Monad.Reader
import Control.Monad.State
import qualified Crypto.Random as Random
import Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope as Rope
import Data.Tuple (swap)
import qualified Language.LSP.Logging as LSP
import Language.LSP.Types
import Language.LSP.Types.Lens (HasCharacter (character), HasParams (params), HasPosition (position), HasTextDocument (textDocument), HasUri (uri), HasVersion (version))
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS as VFS hiding (character)
import Unison.Codebase.Editor.HandleCommand (typecheckCommand)
import qualified Unison.Debug as Debug
import Unison.LSP.Diagnostics
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import qualified Unison.Lexer as L
import Unison.Prelude
import qualified Unison.Result as Result
import UnliftIO
import Witherable (forMaybe)

-- | Some VFS combinators require Monad State, this provides it in a transactionally safe
-- manner.
usingVFS :: forall a. StateT VFS Lsp a -> Lsp a
usingVFS m = do
  vfsVar' <- asks vfsVar
  modifyMVar vfsVar' $ \vfs -> swap <$> runStateT m vfs

getVirtualFile :: (HasTextDocument p docId, HasUri docId Uri) => p -> Lsp (Maybe VirtualFile)
getVirtualFile p = do
  vfs <- asks vfsVar >>= readMVar
  pure $ vfs ^. vfsMap . at (toNormalizedUri $ p ^. textDocument . uri)

getFileContents :: (HasTextDocument p docId, HasUri docId Uri) => p -> Lsp (Maybe Text)
getFileContents p = runMaybeT $ do
  vf <- MaybeT $ getVirtualFile p
  pure . Rope.toText . _file_text $ vf

completionPrefix :: (HasPosition p Position, HasTextDocument p docId, HasUri docId Uri) => p -> Lsp (Maybe (Range, Text))
completionPrefix p = runMaybeT $ do
  (before, _) <- MaybeT $ identifierPartsAtPosition p
  let posLine = p ^. position . LSP.line
  let posChar = (p ^. position . LSP.character)
  let range = mkRange posLine (posChar - fromIntegral (Text.length before)) posLine posChar
  pure (range, before)

identifierPartsAtPosition :: (HasPosition p Position, HasTextDocument p docId, HasUri docId Uri) => p -> Lsp (Maybe (Text, Text))
identifierPartsAtPosition p = runMaybeT $ do
  vf <- MaybeT (getVirtualFile p)
  PosPrefixInfo {fullLine, cursorPos} <- MaybeT (VFS.getCompletionPrefix (p ^. position) vf)
  let (before, after) = Text.splitAt (cursorPos ^. character . Lens.to fromIntegral) fullLine
  pure $ (Text.takeWhileEnd isIdentifierChar before, Text.takeWhile isIdentifierChar after)
  where
    -- Should probably use something from the parser here
    isIdentifierChar = \case
      c
        | isSpace c -> False
        | elem c ("[]()`'\"" :: String) -> False
        | otherwise -> True

identifierAtPosition :: (HasPosition p Position, HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe Text)
identifierAtPosition p = do
  identifierPartsAtPosition p <&> fmap \(before, after) -> (before <> after)

checkFile :: VersionedTextDocumentIdentifier -> Lsp (Maybe FileInfo)
checkFile docId = runMaybeT $ do
  contents <- MaybeT (getFileContents docId)
  parseNames <- asks parseNamesCache >>= readTVarIO
  let sourceName = getUri $ docId ^. uri
  let lexedSource = (contents, L.lexer (Text.unpack sourceName) (Text.unpack contents))
  let ambientAbilities = []
  cb <- asks codebase
  drg <- liftIO Random.getSystemDRG
  r <- (liftIO $ typecheckCommand cb ambientAbilities parseNames sourceName lexedSource drg)
  let Result.Result notes mayResult = r
  case mayResult of
    Nothing -> pure $ FileInfo {parsedFile = Nothing, typecheckedFile = Nothing, ..}
    Just (Left uf) -> pure $ FileInfo {parsedFile = Just uf, typecheckedFile = Nothing, ..}
    Just (Right tf) -> pure $ FileInfo {parsedFile = Nothing, typecheckedFile = Just tf, ..}

fileCheckingWorker :: Lsp ()
fileCheckingWorker = forever do
  dirtyFilesV <- asks dirtyFilesVar
  checkedFilesV <- asks checkedFilesVar
  -- We may want to debounce this if it typechecks too eagerly,
  -- but typechecking is pretty fast right now when scratch files are small
  dirtyFileIDs <- atomically $ do
    dirty <- readTVar dirtyFilesV
    writeTVar dirtyFilesV mempty
    guard $ not $ null dirty
    pure dirty
  freshlyCheckedFiles <- forMaybe dirtyFileIDs checkFile
  Debug.debugM Debug.LSP "Typechecked:" freshlyCheckedFiles
  -- Overwrite any files we successfully checked
  atomically $ modifyTVar' checkedFilesV (`Map.union` freshlyCheckedFiles)
  -- TODO: fork this
  for freshlyCheckedFiles \info -> do
    diagnostics <- infoDiagnostics info
    reportDiagnostics (docId info) $ diagnostics

lspOpenFile :: NotificationMessage 'TextDocumentDidOpen -> Lsp ()
lspOpenFile msg = do
  usingVFS . openVFS vfsLogger $ msg
  let p = msg ^. params
  markFileDirty (VersionedTextDocumentIdentifier (p ^. textDocument . uri) (Just $ p ^. textDocument . version))

lspCloseFile :: NotificationMessage 'TextDocumentDidClose -> Lsp ()
lspCloseFile msg =
  usingVFS . closeVFS vfsLogger $ msg

lspChangeFile :: NotificationMessage 'TextDocumentDidChange -> Lsp ()
lspChangeFile msg = do
  usingVFS . changeFromClientVFS vfsLogger $ msg
  markFileDirty (msg ^. params)

vfsLogger :: Colog.LogAction (StateT VFS Lsp) (Colog.WithSeverity VfsLog)
vfsLogger = Colog.cmap (fmap tShow) (Colog.hoistLogAction lift LSP.defaultClientLogger)

markFileDirty :: (HasTextDocument m VersionedTextDocumentIdentifier) => m -> Lsp ()
markFileDirty doc = do
  dirtyFilesV <- asks dirtyFilesVar
  atomically $ modifyTVar' dirtyFilesV (Map.insertWith latestVersion (doc ^. textDocument . uri) (doc ^. textDocument))
  where
    latestVersion a b =
      if a ^. version > b ^. version
        then a
        else b
