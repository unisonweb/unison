{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-imports #-}

module Unison.Codebase.Editor.UriParser (repoPath, webRepoPath) where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Data.Text as Text

import Data.Text (Text)
import Unison.Codebase.Path (Path(..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo(..))
import Unison.Codebase.Editor.EncodedRepo (EncodedRepo(..))
import qualified Unison.Codebase.Editor.EncodedRepo as ER
import Unison.Codebase.ShortBranchHash (ShortBranchHash(..))
import Unison.Prelude
import qualified Unison.Hash as Hash
import qualified Unison.Lexer
import Unison.Codebase.NameSegment (NameSegment(..))
import Data.Sequence as Seq
import Data.Char (isAlphaNum, isSpace, isDigit)

type P = P.Parsec () Text

--Sidebar on Unison URLs:
--
--Frustratingly, git branches may include /, ., #, =, etc. which make them hard or impossible to parse a branch name among /-separated elements (as is needed to parse branches out of Github urls) without using IO to check whether a given parse corresponds to an actual branch. At least : isn't allowed in a branch name, though it is unfortunately allowed in scp-style git repo addresses (e.g. git@github.com:foo/bar). Saving grace: Github doesn't allow # or : or / in repo names, so that's something.
--
--gh:<user>/<repo>[/<treeish>][:[<nshash>[.<path>] | [.]<path>]]
--
--Examples:
--
--gh:aryairani/unisonbase
--gh:aryairani/unisonbase:libs.v1
--gh:aryairani/unisonbase:#abc123sbh
--gh:aryairani/unisonbase/mybranch
--gh:aryairani/unisonbase:#abc123sbh.path
--gh:aryairani/unisonbase/dev
--gh:aryairani/unisonbase/dev:mybranch

-- colon alternative:
--gh:<user>/<repo>[:<treeish>][':' ( '#' <nshash> ['.' <path>] | '.' <path> )]

--gh:aryairani/unisonbase
--gh:aryairani/unisonbase:.libs.v1
--gh:aryairani/unisonbase:#abc123sbh
--gh:aryairani/unisonbase:mybranch
--gh:aryairani/unisonbase:#abc123sbh.path
--gh:aryairani/unisonbase:dev
--gh:aryairani/unisonbase:dev:.mybranch

--gh:<user>/<repo>[/<treeish>]:namespace:[<nshash>.<path>|<path>]]
--gh:<user>/<repo>[/<treeish>]:term:[<displayname>#]<hash>
--gh:<user>/<repo>[/<treeish>]:type:[<displayname>#]<hash>

--type Repo = GitCommit { url :: Text, commit :: Text }

-- gh:<user>/<repo>[/<treeish]

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

data User = User Text
  deriving (Eq, Ord, Show)

type UrlPath = Text

data HostInfo = HostInfo Text (Maybe Text)
  deriving (Eq, Ord, Show)

type Host = Text -- no port

-- doesn't currently handle passwords like https://user:pass@server.com
-- (is that even a thing for any of these?)
-- or handle ipv6 addresses (https://en.wikipedia.org/wiki/IPv6#Addressing)
parseProtocol :: P GitProtocol
parseProtocol = P.label "parseProtocol" $
  fileRepo <|> httpsRepo <|> sshRepo <|> scpRepo <|> localRepo
  where
  localRepo, fileRepo, httpsRepo, sshRepo, scpRepo :: P GitProtocol
  parsePath =
    P.takeWhile1P (Just "repo path character")
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
  scpRepo = P.label "scpRepo" . P.try $
    ScpProtocol <$> P.optional userInfo <*> parseHost <* symbol ":" <*> parsePath
  userInfo :: P User
  userInfo = P.label "userInfo" . P.try $ do
    username <- P.takeWhile1P (Just "username character") (/= '@')
    void $ C.char '@'
    pure $ User username
  parseHostInfo :: P HostInfo
  parseHostInfo = P.label "parseHostInfo" $
    HostInfo <$> parseHost <*> (P.optional $ do
      void $ symbol ":"
      P.takeWhile1P (Just "digits") isDigit)

  parseHost = P.label "parseHost" $ hostname <|> ipv4 -- <|> ipv6
    where
    hostname =
      P.takeWhile1P (Just "hostname character")
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

repoPath :: P (RemoteRepo, Maybe ShortBranchHash, Path)
repoPath = P.label "generic git repo" $ do
  protocol <- parseProtocol
  treeish <- P.optional treeishSuffix
  let repo = GitRepo (printProtocol protocol) treeish
  nshashPath <- P.optional (C.char ':' *> namespaceHashPath)
  case nshashPath of
    Nothing -> pure (repo, Nothing, Path.empty)
    Just (sbh, p) -> pure (repo, sbh, p)

-- Local Protocol
-- $ git clone /srv/git/project.git
-- $ git clone /srv/git/project.git[:treeish][:[#hash][.path]]
-- File Protocol
-- $ git clone file:///srv/git/project.git[:treeish][:[#hash][.path]] <- imagined
-- Smart / Dumb HTTP protocol
-- $ git clone https://example.com/gitproject.git[:treeish][:[#hash][.path]] <- imagined
-- SSH Protocol
-- $ git clone ssh://[user@]server/project.git[:treeish][:[#hash][.path]]
-- $ git clone [user@]server:project.git[:treeish][:[#hash][.path]]
-- Git Protocol (obsolete)


webRepoPath :: P (EncodedRepo, Maybe ShortBranchHash, Path)
webRepoPath = P.label "hosted git repo path" $ do
  repo <- webRepoTreeish
  nshashPath <- P.optional (C.char ':' *> namespaceHashPath)
  case nshashPath of
    Nothing -> pure (repo, Nothing, Path.empty)
    Just (sbh, p) -> pure (repo, sbh, p)

-- #nshashabc.path.foo.bar or .path.foo.bar
namespaceHashPath :: P (Maybe ShortBranchHash, Path)
namespaceHashPath = do
  sbh <- P.optional shortBranchHash
  p <- P.optional $ do
    void $ C.char '.'
    P.sepBy1
      ((:) <$> C.satisfy Unison.Lexer.wordyIdStartChar
           <*> P.many (C.satisfy Unison.Lexer.wordyIdChar))
      (C.char '.')
  case p of
    Nothing -> pure (sbh, Path.empty)
    Just p  -> pure (sbh, makePath p)
  where makePath = Path . Seq.fromList . fmap (NameSegment . Text.pack)

-- double-check me
treeishSuffix :: P Text
treeishSuffix = P.label "git treeish" . P.try $ do
  void $ C.char ':'
  notdothash <- C.noneOf @[] ".#:"
  rest <- P.takeWhileP (Just "not colon") (/= ':')
  pure $ Text.cons notdothash rest

webRepoTreeish :: P EncodedRepo
webRepoTreeish = P.label "hosted git repo + treeish" $ do
  svc <- service
  void $ C.char ':'
  user <- gitwebUser
  void $ C.char '/'
  repo <- gitwebSlug
  treeish <- P.optional treeishSuffix
  pure $ ER.UserSlugRepo svc user repo treeish
  where
  service = ((symbol "gh" <|> symbol "github") $> ER.Github)
        <|> ((symbol "gl" <|> symbol "gitlab") $> ER.Gitlab)
        <|> ((symbol "bb" <|> symbol "bitbucket") $> ER.Bitbucket)
  -- Github: "Username may only contain alphanumeric characters or single hyphens,
  --          and cannot begin or end with a hyphen."
  -- Gitlab: [a-zA-Z0-9_\.][a-zA-Z0-9_\-\.]*[a-zA-Z0-9_\-]|[a-zA-Z0-9_]
  gitwebUser:: P Text
  gitwebUser =
    -- permissive approximation
    P.takeWhile1P (Just "web username character")
      (\c -> isAlphaNum c || elem @[] c "_-.")

  -- Gitlab: "Path can contain only letters, digits, '_', '-' and '.'.
  --          Cannot start with '-', end in '.git' or end in '.atom'"
  gitwebSlug = gitwebUser

-- <nshash>[.<path>] | [.]<path>
--remoteNameSpace ::

shortBranchHash :: P ShortBranchHash
shortBranchHash = P.label "short branch hash" $ do
  void $ C.char '#'
  ShortBranchHash <$>
    P.takeWhile1P (Just "base32hex chars") (`elem` Hash.validBase32HexChars)
