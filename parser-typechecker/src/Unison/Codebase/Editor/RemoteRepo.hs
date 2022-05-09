{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Codebase.Editor.RemoteRepo where

import qualified Data.Text as Text
import qualified U.Util.Monoid as Monoid
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.Prelude

data ReadRepo
  = ReadRepoGit ReadGitRepo
  deriving (Show)

data ReadGitRepo = ReadGitRepo {url :: Text, ref :: Maybe Text}
  deriving (Show)

data WriteRepo = WriteRepoGit WriteGitRepo
  deriving (Show)

data WriteGitRepo = WriteGitRepo {url' :: Text, branch :: Maybe Text}
  deriving (Show)

writeToRead :: WriteRepo -> ReadRepo
writeToRead = \case
  WriteRepoGit repo -> ReadRepoGit (writeToReadGit repo)

writeToReadGit :: WriteGitRepo -> ReadGitRepo
writeToReadGit = \case
  WriteGitRepo {url', branch} -> ReadGitRepo {url = url', ref = branch}

writePathToRead :: WriteRemotePath -> ReadRemoteNamespace
writePathToRead (w, p) = (writeToRead w, Nothing, p)

printReadRepo :: ReadRepo -> Text
printReadRepo = \case
  ReadRepoGit ReadGitRepo {url, ref} -> url <> Monoid.fromMaybe (Text.cons ':' <$> ref)

printWriteRepo :: WriteRepo -> Text
printWriteRepo = \case
  WriteRepoGit WriteGitRepo {url', branch} -> url' <> Monoid.fromMaybe (Text.cons ':' <$> branch)

printNamespace :: ReadRepo -> Maybe ShortBranchHash -> Path -> Text
printNamespace repo sbh path =
  printReadRepo repo <> case sbh of
    Nothing ->
      if path == Path.empty
        then mempty
        else ":." <> Path.toText path
    Just sbh ->
      ":#" <> SBH.toText sbh
        <> if path == Path.empty
          then mempty
          else "." <> Path.toText path

printHead :: WriteRepo -> Path -> Text
printHead repo path =
  printWriteRepo repo
    <> if path == Path.empty then mempty else ":." <> Path.toText path

type GReadRemoteNamespace a = (a, Maybe ShortBranchHash, Path)

type ReadRemoteNamespace = GReadRemoteNamespace ReadRepo

type ReadGitRemoteNamespace = GReadRemoteNamespace ReadGitRepo

type WriteRemotePath = (WriteRepo, Path)
