module Unison.LSP.VFS where

import Control.Lens
import qualified Control.Lens as Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (isSpace)
import qualified Data.Text as Text
import Data.Tuple (swap)
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS as VFS hiding (character)
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

completionPrefix :: (HasPosition p Position, HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe (Range, Text))
completionPrefix p = runMaybeT $ do
  (before, _) <- MaybeT $ identifierPartsAtPosition p
  let posLine = p ^. position . LSP.line
  let posChar = (p ^. position . LSP.character)
  let range = mkRange posLine (posChar - fromIntegral (Text.length before)) posLine posChar
  pure (range, before)

identifierPartsAtPosition :: (HasPosition p Position, HasTextDocument p TextDocumentIdentifier) => p -> Lsp (Maybe (Text, Text))
identifierPartsAtPosition p = runMaybeT $ do
  vf <- MaybeT (getVirtualFile (p ^. textDocument))
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
