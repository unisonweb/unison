{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.VFS where

import Colog.Core qualified as Colog
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set.Lens (setOf)
import Data.Text qualified as Text
import Data.Text.Utf16.Rope qualified as Rope
import Data.Tuple (swap)
import Language.LSP.Logging qualified as LSP
import Language.LSP.Protocol.Lens (HasCharacter (character), HasParams (params), HasTextDocument (textDocument), HasUri (uri))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as Msg
import Language.LSP.Protocol.Types
import Language.LSP.VFS as VFS hiding (character)
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import Unison.Prelude
import Unison.Syntax.Lexer qualified as Lexer
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
  checkedFilesV <- asks checkedFilesVar
  let dirtyUris = setOf (folded . uri) docs
  atomically $ do
    modifyTVar' dirtyFilesV (Set.union dirtyUris)
    checkedFiles <- readTVar checkedFilesV
    -- Clear the analysis for any files which need to be re-checked.
    for_ dirtyUris \uri -> do
      case Map.lookup uri checkedFiles of
        Nothing -> pure ()
        Just mvar -> void $ tryTakeTMVar mvar

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
  pure
    ( Text.takeWhileEnd isIdentifierChar before,
      -- names can end with '!', and it's not a force, so we include it in the identifier if it's at the end.
      Text.takeWhile (\c -> isIdentifierChar c || c == '!') after
    )
  where
    isIdentifierChar c =
      -- Manually exclude '!' and apostrophe, since those are usually just forces and
      -- delays, which shouldn't be replaced by auto-complete.
      (c /= '!' && c /= '\'')
        && (c == '.' || Lexer.wordyIdChar c || Lexer.symbolyIdChar c)

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

lspOpenFile :: Msg.TNotificationMessage 'Msg.Method_TextDocumentDidOpen -> Lsp ()
lspOpenFile msg = do
  usingVFS . openVFS vfsLogger $ msg
  markFilesDirty [msg ^. params . textDocument]

lspCloseFile :: Msg.TNotificationMessage 'Msg.Method_TextDocumentDidClose -> Lsp ()
lspCloseFile msg =
  usingVFS . closeVFS vfsLogger $ msg

lspChangeFile :: Msg.TNotificationMessage 'Msg.Method_TextDocumentDidChange -> Lsp ()
lspChangeFile msg = do
  usingVFS . changeFromClientVFS vfsLogger $ msg
  markFilesDirty [msg ^. params . textDocument]
