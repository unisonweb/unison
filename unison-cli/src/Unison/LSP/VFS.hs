{-# LANGUAGE DataKinds #-}
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
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope as Rope
import Data.Tuple (swap)
import qualified Language.LSP.Logging as LSP
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS as VFS hiding (character)
import Unison.Codebase.Editor.HandleCommand (typecheckCommand)
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import qualified Unison.Lexer as L
import Unison.Prelude
import qualified Unison.Result as Result
import UnliftIO

-- | Some VFS combinators require Monad State, this provides it in a transactionally safe
-- manner.
usingVFS :: forall a. StateT VFS Lsp a -> Lsp a
usingVFS m = do
  vfsVar' <- asks vfsVar
  modifyMVar vfsVar' $ \vfs -> swap <$> runStateT m vfs

getVirtualFile :: (HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe VirtualFile)
getVirtualFile p = do
  vfs <- asks vfsVar >>= readMVar
  liftIO $ print ("vfsmap" :: String, vfs)
  let (TextDocumentIdentifier uri) = p ^. textDocument
  pure $ vfs ^. vfsMap . at (toNormalizedUri uri)

getFileContents :: (HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe Text)
getFileContents p = runMaybeT $ do
  vf <- MaybeT $ getVirtualFile p
  pure . Rope.toText . _file_text $ vf

completionPrefix :: (HasPosition p Position, HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe (Range, Text))
completionPrefix p = runMaybeT $ do
  (before, _) <- MaybeT $ identifierPartsAtPosition p
  let posLine = p ^. position . LSP.line
  let posChar = (p ^. position . LSP.character)
  let range = mkRange posLine (posChar - fromIntegral (Text.length before)) posLine posChar
  pure (range, before)

identifierPartsAtPosition :: (HasPosition p Position, HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe (Text, Text))
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

checkFile :: TextDocumentIdentifier -> Lsp (Maybe FileInfo)
checkFile docId = runMaybeT $ do
  contents <- MaybeT (getFileContents docId)
  let sourceName = getUri $ docId ^. uri
  let lexedSource = (contents, L.lexer (Text.unpack sourceName) (Text.unpack contents))
  let ambientAbilities = []
  let parseNames = _
  cb <- asks codebase
  drg <- liftIO Random.getSystemDRG
  r <- (liftIO $ typecheckCommand cb ambientAbilities parseNames sourceName lexedSource drg)
  let Result.Result notes mayResult = r
  case mayResult of
    Nothing -> pure $ FileInfo {parsedFile = Nothing, typecheckedFile = Nothing, ..}
    Just (Left uf) -> pure $ FileInfo {parsedFile = Just uf, typecheckedFile = Nothing, ..}
    Just (Right tf) -> pure $ FileInfo {parsedFile = Nothing, typecheckedFile = Just tf, ..}

lspOpenFile :: NotificationMessage 'TextDocumentDidOpen -> Lsp ()
lspOpenFile = usingVFS . openVFS vfsLogger

lspCloseFile :: NotificationMessage 'TextDocumentDidClose -> Lsp ()
lspCloseFile = usingVFS . closeVFS vfsLogger

lspChangeFile :: NotificationMessage 'TextDocumentDidChange -> Lsp ()
lspChangeFile = usingVFS . changeFromClientVFS vfsLogger

vfsLogger :: Colog.LogAction (StateT VFS Lsp) (Colog.WithSeverity VfsLog)
vfsLogger = Colog.cmap (fmap tShow) (Colog.hoistLogAction lift LSP.defaultClientLogger)
