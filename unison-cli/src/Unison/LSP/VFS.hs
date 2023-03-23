{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.VFS where

import qualified Colog.Core as Colog
import Control.Lens hiding (List)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set.Lens (setOf)
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope as Rope
import Data.Tuple (swap)
import qualified Language.LSP.Logging as LSP
import qualified Language.LSP.Server as LSP
import Language.LSP.Types
import Language.LSP.Types.Lens (HasCharacter (character), HasParams (params), HasTextDocument (textDocument), HasUri (uri))
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS as VFS hiding (character)
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import Unison.Prelude
import qualified Unison.Syntax.Lexer as Lexer
import UnliftIO

-- | Some VFS combinators require Monad State, this provides it in a transactionally safe
-- manner so we're sure we don't edit the same file in two different actions at the same time.
usingVFS :: forall a. StateT VFS Lsp a -> Lsp a
usingVFS m = do
  vfsVar' <- asks vfsVar
  modifyMVar vfsVar' $ \vfs -> swap <$> runStateT m vfs

getVirtualFile :: Uri -> MaybeT Lsp VirtualFile
getVirtualFile fileUri = do
  vfs <- asks vfsVar >>= readMVar
  MaybeT . pure $ vfs ^. vfsMap . at (toNormalizedUri $ fileUri)

getFileContents :: Uri -> MaybeT Lsp (FileVersion, Text)
getFileContents fileUri = do
  vf <- getVirtualFile fileUri
  pure (vf ^. lsp_version, Rope.toText $ vf ^. file_text)

vfsLogger :: Colog.LogAction (StateT VFS Lsp) (Colog.WithSeverity VfsLog)
vfsLogger = Colog.cmap (fmap tShow) (Colog.hoistLogAction lift LSP.defaultClientLogger)

-- | Mark some files as needing to be checked.
markFilesDirty :: (Foldable f, HasUri doc Uri) => f doc -> Lsp ()
markFilesDirty docs = do
  dirtyFilesV <- asks dirtyFilesVar
  let dirtyUris = setOf (folded . uri) docs
  atomically $ modifyTVar' dirtyFilesV (Set.union dirtyUris)

-- | Mark all files for re-checking.
--
-- We may want to do this when our names or perspective change.
markAllFilesDirty :: Lsp ()
markAllFilesDirty = do
  vfs <- asks vfsVar >>= readMVar
  markFilesDirty $ Map.keys (vfs ^. vfsMap)

-- | Returns the name or symbol which the provided position is contained in.
identifierAtPosition :: Uri -> Position -> MaybeT Lsp Text
identifierAtPosition uri pos = do
  identifierSplitAtPosition uri pos <&> \(before, after) -> (before <> after)

-- | Returns the prefix and suffix of the symbol which the provided position is contained in.
identifierSplitAtPosition :: Uri -> Position -> MaybeT Lsp (Text, Text)
identifierSplitAtPosition uri pos = do
  vf <- getVirtualFile uri
  PosPrefixInfo {fullLine, cursorPos} <- MaybeT (VFS.getCompletionPrefix pos vf)
  let (before, after) = Text.splitAt (cursorPos ^. character . to fromIntegral) fullLine
  pure (Text.takeWhileEnd isIdentifierChar before, Text.takeWhile isIdentifierChar after)
  where
    isIdentifierChar c =
      Lexer.wordyIdChar c || Lexer.symbolyIdChar c

-- | Returns the prefix of the symbol at the provided location, and the range that prefix
-- spans.
completionPrefix :: Uri -> Position -> MaybeT Lsp (Range, Text)
completionPrefix uri pos = do
  (before, _) <- identifierSplitAtPosition uri pos
  let posLine = pos ^. LSP.line
  let posChar = pos ^. LSP.character
  let range = mkRange posLine (posChar - fromIntegral (Text.length before)) posLine posChar
  pure (range, before)

--- Handlers for tracking file changes.

lspOpenFile :: NotificationMessage 'TextDocumentDidOpen -> Lsp ()
lspOpenFile msg = do
  usingVFS . openVFS vfsLogger $ msg
  markFilesDirty [msg ^. params . textDocument]

lspCloseFile :: NotificationMessage 'TextDocumentDidClose -> Lsp ()
lspCloseFile msg =
  usingVFS . closeVFS vfsLogger $ msg

lspChangeFile :: NotificationMessage 'TextDocumentDidChange -> Lsp ()
lspChangeFile msg = do
  usingVFS . changeFromClientVFS vfsLogger $ msg
  markFilesDirty [msg ^. params . textDocument]

-- | Edit a file in the client, returns a 'waiter' which will block until the edit has been
-- received by the client.
editFileInClient :: Uri -> Text -> Range -> Text -> Lsp (IO (Either ResponseError ApplyWorkspaceEditResponseBody))
editFileInClient uri description range newText = do
  let changes = HashMap.singleton uri (List [TextEdit range newText])
  let workspaceEdit = WorkspaceEdit (Just changes) Nothing Nothing
  blocker <- newEmptyMVar
  void $ LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams (Just description) (workspaceEdit)) (putMVar blocker)
  pure (takeMVar blocker)
