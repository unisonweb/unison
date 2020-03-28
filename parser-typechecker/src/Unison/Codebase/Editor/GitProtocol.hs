{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.GitProtocol where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.String (IsString, fromString)

data GitProtocol
  = HttpsProtocol (Maybe User) HostInfo UrlPath
  | SshProtocol (Maybe User) HostInfo UrlPath
  | ScpProtocol (Maybe User) Host UrlPath
  | FileProtocol UrlPath
  | LocalProtocol UrlPath
  deriving (Eq, Ord, Show)

printProtocol :: GitProtocol -> Text
--printProtocol x | traceShow x False = undefined
printProtocol x = case x of
  HttpsProtocol muser hostInfo path -> "https://"
    <> printUser muser
    <> printHostInfo hostInfo
    <> path
  SshProtocol muser hostInfo path -> "ssh://"
    <> printUser muser
    <> printHostInfo hostInfo
    <> path
  ScpProtocol muser host path -> printUser muser <> host <> ":" <> path
  FileProtocol path -> "file://" <> path
  LocalProtocol path -> path
  where
  printUser = maybe mempty (\(User u) -> u <> "@")
  printHostInfo :: HostInfo -> Text
  printHostInfo (HostInfo hostname mport) =
    hostname <> maybe mempty (Text.cons ':') mport

data Scheme = Ssh | Https
  deriving (Eq, Ord, Show)

newtype User = User Text
  deriving (Eq, Ord, Show)

instance IsString User where
  fromString = User . fromString

type UrlPath = Text

data HostInfo = HostInfo Host (Maybe Port)
  deriving (Eq, Ord, Show)

type Host = Text
type Port = Text
