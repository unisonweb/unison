{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.RemoteRepo where

import qualified Data.Text as Text
import qualified Servant.Client as Servant
import qualified U.Util.Monoid as Monoid
import Unison.Share.Types (CodeserverHost(..))
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.Prelude

data ReadRepo
  = ReadRepoGit ReadGitRepo
  | ReadRepoShare CodeserverHost
  deriving (Eq, Show)

data ReadGitRepo = ReadGitRepo {url :: Text, ref :: Maybe Text}
  deriving (Eq, Show)

shareRepoToBaseUrl :: CodeserverHost -> Servant.BaseUrl
shareRepoToBaseUrl (CodeserverHost host) = Servant.BaseUrl Servant.Https (Text.unpack host) 443 ""

data WriteRepo
  = WriteRepoGit WriteGitRepo
  | WriteRepoShare CodeserverHost
  deriving (Eq, Show)

data WriteGitRepo = WriteGitRepo {url :: Text, branch :: Maybe Text}
  deriving (Eq, Show)

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
    ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo = writeToReadGit repo, sbh = Nothing, path}
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
  ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo, sbh, path} ->
    printReadGitRepo repo <> maybePrintSBH sbh <> maybePrintPath path
    where
      maybePrintSBH = \case
        Nothing -> mempty
        Just sbh -> "#" <> SBH.toText sbh
  ReadRemoteNamespaceShare ReadShareRemoteNamespace {server = CodeserverHost _host, repo, path} ->
    repo <> maybePrintPath path

-- | Render a 'WriteRemotePath' as text.
printWriteRemotePath :: WriteRemotePath -> Text
printWriteRemotePath = \case
  WriteRemotePathGit (WriteGitRemotePath {repo, path}) ->
    printWriteGitRepo repo <> maybePrintPath path
  WriteRemotePathShare (WriteShareRemotePath {server = CodeserverHost _host, repo, path}) ->
    repo <> maybePrintPath path

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
    sbh :: Maybe ShortBranchHash,
    path :: Path
  }
  deriving stock (Eq, Show)

data ReadShareRemoteNamespace = ReadShareRemoteNamespace
  { server :: CodeserverHost,
    repo :: Text,
    -- sbh :: Maybe ShortBranchHash, -- maybe later
    path :: Path
  }
  deriving stock (Eq, Show)

data WriteRemotePath
  = WriteRemotePathGit WriteGitRemotePath
  | WriteRemotePathShare WriteShareRemotePath
  deriving stock (Eq, Show)

data WriteGitRemotePath = WriteGitRemotePath
  { repo :: WriteGitRepo,
    path :: Path
  }
  deriving stock (Eq, Show)

data WriteShareRemotePath = WriteShareRemotePath
  { server :: CodeserverHost,
    repo :: Text,
    path :: Path
  }
  deriving stock (Eq, Show)
