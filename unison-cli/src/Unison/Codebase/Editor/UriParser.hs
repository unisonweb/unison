{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.UriParser (repoPath, writeRepo, writeRepoPath) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Sequence as Seq
import Data.Text as Text
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Unison.Codebase.Editor.RemoteRepo (ReadGitRepo (..), ReadRemoteNamespace, ReadRepo (..), WriteGitRepo (..), WriteRemotePath, WriteRepo (..))
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash (..))
import qualified Unison.Hash as Hash
import qualified Unison.Lexer
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude

type P = P.Parsec Void Text.Text

-- Here are the git protocols that we know how to parse
-- Local Protocol

-- $ git clone /srv/git/project.git

-- $ git clone /srv/git/project.git[:treeish][:[#hash][.path]]
-- File Protocol

-- $ git clone file:///srv/git/project.git[:treeish][:[#hash][.path]]
-- Smart / Dumb HTTP protocol

-- $ git clone https://example.com/gitproject.git[:treeish][:[#hash][.path]]
-- SSH Protocol

-- $ git clone ssh://[user@]server/project.git[:treeish][:[#hash][.path]]

-- $ git clone [user@]server:project.git[:treeish][:[#hash][.path]]

repoPath :: P ReadRemoteNamespace
repoPath = P.label "generic git repo" $ do
  protocol <- parseProtocol
  treeish <- P.optional treeishSuffix
  let repo = (ReadRepoGit ReadGitRepo {url = printProtocol protocol, ref = treeish})
  nshashPath <- P.optional (C.char ':' *> namespaceHashPath)
  case nshashPath of
    Nothing -> pure (repo, Nothing, Path.empty)
    Just (sbh, p) -> pure (repo, sbh, p)

writeRepo :: P WriteRepo
writeRepo = P.label "repo root for writing" $ do
  uri <- parseProtocol
  treeish <- P.optional treeishSuffix
  pure (WriteRepoGit WriteGitRepo {url = printProtocol uri, branch = treeish})

writeRepoPath :: P WriteRemotePath
writeRepoPath = P.label "generic git repo" $ do
  repo <- writeRepo
  path <- P.optional (C.char ':' *> absolutePath)
  pure (repo, fromMaybe Path.empty path)

-- does this not exist somewhere in megaparsec? yes in 7.0
symbol :: Text -> P Text
symbol = L.symbol (pure ())

data GitProtocol
  = HttpsProtocol (Maybe User) HostInfo UrlPath
  | SshProtocol (Maybe User) HostInfo UrlPath
  | ScpProtocol (Maybe User) Host UrlPath
  | FileProtocol UrlPath
  | LocalProtocol UrlPath
  deriving (Eq, Ord, Show)

printProtocol :: GitProtocol -> Text
-- printProtocol x | traceShow x False = undefined
printProtocol x = case x of
  HttpsProtocol muser hostInfo path ->
    "https://"
      <> printUser muser
      <> printHostInfo hostInfo
      <> path
  SshProtocol muser hostInfo path ->
    "ssh://"
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

data User = User Text
  deriving (Eq, Ord, Show)

type UrlPath = Text

data HostInfo = HostInfo Text (Maybe Text)
  deriving (Eq, Ord, Show)

type Host = Text -- no port

-- doesn't yet handle basic authentication like https://user:pass@server.com
-- (does anyone even want that?)
-- or handle ipv6 addresses (https://en.wikipedia.org/wiki/IPv6#Addressing)
parseProtocol :: P GitProtocol
parseProtocol =
  P.label "parseProtocol" $
    fileRepo <|> httpsRepo <|> sshRepo <|> scpRepo <|> localRepo
  where
    localRepo, fileRepo, httpsRepo, sshRepo, scpRepo :: P GitProtocol
    parsePath =
      P.takeWhile1P
        (Just "repo path character")
        (\c -> not (isSpace c || c == ':'))
    localRepo = LocalProtocol <$> parsePath
    fileRepo = P.label "fileRepo" $ do
      void $ symbol "file://"
      FileProtocol <$> parsePath
    httpsRepo = P.label "httpsRepo" $ do
      void $ symbol "https://"
      HttpsProtocol <$> P.optional userInfo <*> parseHostInfo <*> parsePath
    sshRepo = P.label "sshRepo" $ do
      void $ symbol "ssh://"
      SshProtocol <$> P.optional userInfo <*> parseHostInfo <*> parsePath
    scpRepo =
      P.label "scpRepo" . P.try $
        ScpProtocol <$> P.optional userInfo <*> parseHost <* symbol ":" <*> parsePath
    userInfo :: P User
    userInfo = P.label "userInfo" . P.try $ do
      username <- P.takeWhile1P (Just "username character") (/= '@')
      void $ C.char '@'
      pure $ User username
    parseHostInfo :: P HostInfo
    parseHostInfo =
      P.label "parseHostInfo" $
        HostInfo <$> parseHost
          <*> ( P.optional $ do
                  void $ symbol ":"
                  P.takeWhile1P (Just "digits") isDigit
              )

    parseHost = P.label "parseHost" $ hostname <|> ipv4 -- <|> ipv6
      where
        hostname =
          P.takeWhile1P
            (Just "hostname character")
            (\c -> isAlphaNum c || c == '.' || c == '-')
        ipv4 = P.label "ipv4 address" $ do
          o1 <- decOctet
          void $ C.char '.'
          o2 <- decOctet
          void $ C.char '.'
          o3 <- decOctet
          void $ C.char '.'
          o4 <- decOctet
          pure $ Text.pack $ o1 <> "." <> o2 <> "." <> o3 <> "." <> o4
        decOctet = P.count' 1 3 C.digitChar

-- #nshashabc.path.foo.bar or .path.foo.bar
namespaceHashPath :: P (Maybe ShortBranchHash, Path)
namespaceHashPath = do
  sbh <- P.optional shortBranchHash
  p <- P.optional absolutePath
  pure (sbh, fromMaybe Path.empty p)

absolutePath :: P Path
absolutePath = do
  void $ C.char '.'
  Path . Seq.fromList . fmap (NameSegment . Text.pack)
    <$> P.sepBy1
      ( (:) <$> C.satisfy Unison.Lexer.wordyIdStartChar
          <*> P.many (C.satisfy Unison.Lexer.wordyIdChar)
      )
      (C.char '.')

treeishSuffix :: P Text
treeishSuffix = P.label "git treeish" . P.try $ do
  void $ C.char ':'
  notdothash <- C.noneOf @[] ".#:"
  rest <- P.takeWhileP (Just "not colon") (/= ':')
  pure $ Text.cons notdothash rest

shortBranchHash :: P ShortBranchHash
shortBranchHash = P.label "short branch hash" $ do
  void $ C.char '#'
  ShortBranchHash
    <$> P.takeWhile1P (Just "base32hex chars") (`elem` Hash.validBase32HexChars)
