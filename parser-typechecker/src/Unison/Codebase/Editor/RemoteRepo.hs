{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.RemoteRepo where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import qualified Data.Text as Text
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import qualified Unison.Codebase.ShortCausalHash as SCH
import Unison.Prelude
import Unison.Share.Types
import qualified Unison.Util.Monoid as Monoid

data ReadRepo
  = ReadRepoGit ReadGitRepo
  | ReadRepoShare ShareCodeserver
  deriving stock (Eq, Ord, Show)

data ShareCodeserver
  = DefaultCodeserver
  | CustomCodeserver CodeserverURI
  deriving stock (Eq, Ord, Show)

-- |
-- >>> :set -XOverloadedLists
-- >>> import Data.Maybe (fromJust)
-- >>> import Network.URI
-- >>> displayShareCodeserver DefaultCodeserver "share" ["base", "List"]
-- "share.base.List"
-- >>> displayShareCodeserver DefaultCodeserver "share" []
-- "share"
-- >>> displayShareCodeserver (CustomCodeserver . fromJust $ parseURI "https://share-next.unison-lang.org/api" >>= codeserverFromURI ) "unison" ["base", "List"]
-- "share(https://share-next.unison-lang.org:443/api).unison.base.List"
displayShareCodeserver :: ShareCodeserver -> Text -> Path -> Text
displayShareCodeserver cs repo path =
  let shareServer = case cs of
        DefaultCodeserver -> ""
        CustomCodeserver cu -> "share(" <> tShow cu <> ")."
   in shareServer <> repo <> maybePrintPath path

data ReadGitRepo = ReadGitRepo {url :: Text, ref :: Maybe Text}
  deriving stock (Eq, Ord, Show)

data WriteRepo
  = WriteRepoGit WriteGitRepo
  | WriteRepoShare ShareCodeserver
  deriving stock (Eq, Ord, Show)

data WriteGitRepo = WriteGitRepo {url :: Text, branch :: Maybe Text}
  deriving stock (Eq, Ord, Show)

writeToRead :: WriteRepo -> ReadRepo
writeToRead = \case
  WriteRepoGit repo -> ReadRepoGit (writeToReadGit repo)
  WriteRepoShare repo -> ReadRepoShare repo

writeToReadGit :: WriteGitRepo -> ReadGitRepo
writeToReadGit = \case
  WriteGitRepo {url, branch} -> ReadGitRepo {url = url, ref = branch}

writePathToRead :: WriteRemotePath -> ReadRemoteNamespace
writePathToRead = \case
  WriteRemotePathGit WriteGitRemotePath {repo, path} ->
    ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo = writeToReadGit repo, sch = Nothing, path}
  WriteRemotePathShare WriteShareRemotePath {server, repo, path} ->
    ReadRemoteNamespaceShare ReadShareRemoteNamespace {server, repo, path}

printReadGitRepo :: ReadGitRepo -> Text
printReadGitRepo ReadGitRepo {url, ref} =
  "git(" <> url <> Monoid.fromMaybe (Text.cons ':' <$> ref) <> ")"

printWriteGitRepo :: WriteGitRepo -> Text
printWriteGitRepo WriteGitRepo {url, branch} = "git(" <> url <> Monoid.fromMaybe (Text.cons ':' <$> branch) <> ")"

-- | print remote namespace
printNamespace :: ReadRemoteNamespace -> Text
printNamespace = \case
  ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo, sch, path} ->
    printReadGitRepo repo <> maybePrintSCH sch <> maybePrintPath path
    where
      maybePrintSCH = \case
        Nothing -> mempty
        Just sch -> "#" <> SCH.toText sch
  ReadRemoteNamespaceShare ReadShareRemoteNamespace {server, repo, path} ->
    displayShareCodeserver server repo path

-- | Render a 'WriteRemotePath' as text.
printWriteRemotePath :: WriteRemotePath -> Text
printWriteRemotePath = \case
  WriteRemotePathGit (WriteGitRemotePath {repo, path}) ->
    printWriteGitRepo repo <> maybePrintPath path
  WriteRemotePathShare (WriteShareRemotePath {server, repo, path}) ->
    displayShareCodeserver server repo path

maybePrintPath :: Path -> Text
maybePrintPath path =
  if path == Path.empty
    then mempty
    else "." <> Path.toText path

data ReadRemoteNamespace
  = ReadRemoteNamespaceGit ReadGitRemoteNamespace
  | ReadRemoteNamespaceShare ReadShareRemoteNamespace
  deriving stock (Eq, Show)

data ReadGitRemoteNamespace = ReadGitRemoteNamespace
  { repo :: ReadGitRepo,
    sch :: Maybe ShortCausalHash,
    path :: Path
  }
  deriving stock (Eq, Show)

data ReadShareRemoteNamespace = ReadShareRemoteNamespace
  { server :: ShareCodeserver,
    repo :: Text,
    -- sch :: Maybe ShortCausalHash, -- maybe later
    path :: Path
  }
  deriving stock (Eq, Show)

isPublic :: ReadShareRemoteNamespace -> Bool
isPublic ReadShareRemoteNamespace {path} =
  case path of
    ("public" Path.:< _) -> True
    _ -> False

data WriteRemotePath
  = WriteRemotePathGit WriteGitRemotePath
  | WriteRemotePathShare WriteShareRemotePath
  deriving stock (Eq, Show)

-- | A lens which focuses the path of a remote path.
remotePath_ :: Lens' WriteRemotePath Path
remotePath_ = Lens.lens getter setter
  where
    getter = \case
      WriteRemotePathGit (WriteGitRemotePath _ path) -> path
      WriteRemotePathShare (WriteShareRemotePath _ _ path) -> path
    setter remote path = case remote of
      WriteRemotePathGit (WriteGitRemotePath repo _) -> WriteRemotePathGit $ WriteGitRemotePath repo path
      WriteRemotePathShare (WriteShareRemotePath server repo _) -> WriteRemotePathShare $ WriteShareRemotePath server repo path

data WriteGitRemotePath = WriteGitRemotePath
  { repo :: WriteGitRepo,
    path :: Path
  }
  deriving stock (Eq, Show)

data WriteShareRemotePath = WriteShareRemotePath
  { server :: ShareCodeserver,
    repo :: Text,
    path :: Path
  }
  deriving stock (Eq, Show)
