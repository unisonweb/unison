module Unison.LSP.VFS where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Tuple (swap)
import Language.LSP.Types
import Language.LSP.Types.Lens
import Language.LSP.VFS as VFS
import Unison.LSP.Types
import Unison.Prelude
import UnliftIO

-- | Some VFS combinators require Monad State, this provides it in a transactionally safe
-- manner.
usingVFS :: forall a. StateT VFS Lsp a -> Lsp a
usingVFS m = do
  vfsVar' <- asks vfsVar
  modifyMVar vfsVar' $ \vfs -> swap <$> runStateT m vfs

getVirtualFile :: TextDocumentIdentifier -> Lsp (Maybe VirtualFile)
getVirtualFile (TextDocumentIdentifier uri) = do
  vfs <- asks vfsVar >>= readMVar
  liftIO $ print ("vfsmap" :: String, vfs)
  pure $ vfs ^. vfsMap . at (toNormalizedUri uri)

completionPrefix :: (HasPosition p Position, HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe (Text, Text))
completionPrefix p = runMaybeT $ do
  vf <- MaybeT (tap "VFS" $ getVirtualFile (p ^. textDocument))
  PosPrefixInfo {prefixModule, prefixText} <- MaybeT (tap "PREFIX" $ VFS.getCompletionPrefix (p ^. position) vf)
  -- The LSP lib assumes haskell modules, which luckily for us also use dot-separators.
  pure $ (prefixModule, prefixText)
  where
    tap :: forall m a. (MonadIO m, Show a) => String -> m a -> m a
    tap msg m = do
      a <- m
      liftIO $ print (msg, a)
      pure a
