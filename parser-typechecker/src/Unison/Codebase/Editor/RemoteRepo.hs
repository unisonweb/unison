{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Unison.Codebase.Editor.RemoteRepo where

import Unison.Prelude
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Path (Path)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH

data ReadRepo = ReadGitRepo { url :: Text {-, commitish :: Maybe Text -}} deriving (Eq, Ord, Show)
data WriteRepo = WriteGitRepo { url' :: Text {-, branch :: Maybe Text -}} deriving (Eq, Ord, Show)

writeToRead :: WriteRepo -> ReadRepo
writeToRead (WriteGitRepo url) = ReadGitRepo url

writePathToRead :: WriteRemotePath -> ReadRemoteNamespace
writePathToRead (w, p) = (writeToRead w, Nothing, p)

printReadRepo :: ReadRepo -> Text
printReadRepo ReadGitRepo{..} = url -- <> Monoid.fromMaybe (Text.cons ':' <$> commit)
printWriteRepo :: WriteRepo -> Text
printWriteRepo WriteGitRepo{..} = url' -- <> Monoid.fromMaybe (Text.cons ':' <$> branch)

printNamespace :: ReadRepo -> Maybe ShortBranchHash -> Path 'Path.Absolute -> Text
printNamespace repo sbh path =
  printReadRepo repo <> case sbh of
    Nothing -> if Path.isRoot path then mempty
      else ":" <> Path.toText path
    Just sbh -> ":#" <> SBH.toText sbh <>
      if Path.isRoot path then mempty
      else "." <> Path.toText path

printHead :: WriteRepo -> Path 'Path.Absolute -> Text
printHead repo path =
  printWriteRepo repo
    <> if Path.isRoot path then mempty else ":" <> Path.toText path

type ReadRemoteNamespace = (ReadRepo, Maybe ShortBranchHash, Path 'Path.Absolute)
type WriteRemotePath = (WriteRepo, Path 'Path.Absolute)
