{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Unison.Codebase.Editor.RemoteRepo where

import Unison.Prelude
import Unison.Util.Monoid as Monoid
import Data.Text as Text
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Path (Path)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.Codebase.Editor.GitProtocol (GitProtocol(..), printProtocol)

data RemoteRepo = GitRepo { gitProtocol :: GitProtocol, commit :: Maybe Text }
  deriving (Eq, Ord, Show)

url :: RemoteRepo -> Text
url = printProtocol . gitProtocol 

printRepo :: RemoteRepo -> Text
printRepo repo@GitRepo{} = 
  url repo <> Monoid.fromMaybe (Text.cons ':' <$> commit repo)

printNamespace :: RemoteRepo -> Maybe ShortBranchHash -> Path -> Text
printNamespace repo sbh path =
  printRepo repo <> case sbh of
    Nothing -> if path == Path.empty then mempty
      else ":." <> Path.toText path
    Just sbh -> ":#" <> SBH.toText sbh <>
      if path == Path.empty then mempty
      else "." <> Path.toText path

type RemoteNamespace = (RemoteRepo, Maybe ShortBranchHash, Path)
type RemoteHead = (RemoteRepo, Path)
