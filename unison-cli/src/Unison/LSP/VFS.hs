{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.LSP.VFS where

import Control.Lens
import qualified Control.Lens as Lens
import Control.Monad.Reader
import Control.Monad.State
import qualified Crypto.Random as Random
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.Utf16.Rope as Rope
import Data.Tuple (swap)
import Language.LSP.Types
import Language.LSP.Types.Lens
import qualified Language.LSP.Types.Lens as LSP
import Language.LSP.VFS as VFS hiding (character)
import Unison.Codebase.Editor.Command (LexedSource)
import Unison.Codebase.Editor.HandleCommand (typecheck, typecheckCommand)
import qualified Unison.FileParsers as UF
import Unison.LSP.Types
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Result
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
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

data FileInfo = FileInfo
  { lexedSource :: Maybe LexedSource,
    parsedFile :: Maybe (UnisonFile Symbol Ann),
    typecheckedFile :: Maybe (TypecheckedUnisonFile Symbol Ann),
    notes :: Seq (Note Symbol Ann)
  }

checkFile :: TextDocumentIdentifier -> Lsp FileInfo
checkFile docId = do
  let sourceName = getUri $ docId ^. uri
  let lexedSource = _
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
