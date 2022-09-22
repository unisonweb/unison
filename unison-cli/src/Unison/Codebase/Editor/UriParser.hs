{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.UriParser
  ( repoPath,
    writeGitRepo,
    deprecatedWriteGitRemotePath,
    writeRemotePath,
  )
where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadGitRepo (..),
    ReadRemoteNamespace (..),
    ReadShareRemoteNamespace (..),
    ShareCodeserver (DefaultCodeserver),
    WriteGitRemotePath (..),
    WriteGitRepo (..),
    WriteRemotePath (..),
    WriteShareRemotePath (..),
  )
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash (..))
import qualified Unison.Hash as Hash
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import qualified Unison.Syntax.Lexer

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
repoPath =
  P.label "generic repo" $
    fmap ReadRemoteNamespaceGit readGitRemoteNamespace
      <|> fmap ReadRemoteNamespaceShare readShareRemoteNamespace

-- >>> P.parseMaybe writeRemotePath "unisonweb.base._releases.M4"
-- >>> P.parseMaybe writeRemotePath "git(git@github.com:unisonweb/base:v3)._releases.M3"
-- Just (WriteRemotePathShare (WriteShareRemotePath {server = ShareRepo, repo = "unisonweb", path = base._releases.M4}))
-- Just (WriteRemotePathGit (WriteGitRemotePath {repo = WriteGitRepo {url = "git@github.com:unisonweb/base", branch = Just "v3"}, path = _releases.M3}))
writeRemotePath :: P WriteRemotePath
writeRemotePath =
  (fmap WriteRemotePathGit writeGitRemotePath)
    <|> fmap WriteRemotePathShare writeShareRemotePath

-- >>> P.parseMaybe writeShareRemotePath "unisonweb.base._releases.M4"
-- Just (WriteShareRemotePath {server = ShareRepo, repo = "unisonweb", path = base._releases.M4})
writeShareRemotePath :: P WriteShareRemotePath
writeShareRemotePath =
  P.label "write share remote path" $
    WriteShareRemotePath
      <$> pure DefaultCodeserver
      <*> (NameSegment.toText <$> nameSegment)
      <*> (Path.fromList <$> P.many (C.char '.' *> nameSegment))

-- >>> P.parseMaybe readShareRemoteNamespace ".unisonweb.base._releases.M4"
-- >>> P.parseMaybe readShareRemoteNamespace "unisonweb.base._releases.M4"
-- Nothing
-- Just (ReadShareRemoteNamespace {server = ShareRepo, repo = "unisonweb", path = base._releases.M4})
readShareRemoteNamespace :: P ReadShareRemoteNamespace
readShareRemoteNamespace = do
  P.label "read share remote namespace" $
    ReadShareRemoteNamespace
      <$> pure DefaultCodeserver
      -- <*> sbh <- P.optional shortBranchHash
      <*> (NameSegment.toText <$> nameSegment)
      <*> (Path.fromList <$> P.many (C.char '.' *> nameSegment))

-- >>> P.parseMaybe readGitRemoteNamespace "git(user@server:project.git:branch)#asdf"
-- >>> P.parseMaybe readGitRemoteNamespace "git(user@server:project.git:branch)#asdf."
-- >>> P.parseMaybe readGitRemoteNamespace "git(user@server:project.git:branch)"
-- >>> P.parseMaybe readGitRemoteNamespace "git(git@github.com:unisonweb/base:v3)._releases.M3"
-- >>> P.parseMaybe readGitRemoteNamespace "git( user@server:project.git:branch )#asdf.foo.bar"
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sbh = Just #asdf, path = })
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sbh = Just #asdf, path = })
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sbh = Nothing, path = })
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "git@github.com:unisonweb/base", ref = Just "v3"}, sbh = Nothing, path = _releases.M3})
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sbh = Just #asdf, path = foo.bar})
readGitRemoteNamespace :: P ReadGitRemoteNamespace
readGitRemoteNamespace = P.label "generic git repo" $ do
  C.string "git("
  protocol <- parseGitProtocol
  treeish <- P.optional gitTreeishSuffix
  let repo = ReadGitRepo {url = printProtocol protocol, ref = treeish}
  C.string ")"
  nshashPath <- P.optional namespaceHashPath
  pure case nshashPath of
    Nothing -> ReadGitRemoteNamespace {repo, sbh = Nothing, path = Path.empty}
    Just (sbh, path) -> ReadGitRemoteNamespace {repo, sbh, path}

-- >>> P.parseMaybe writeGitRepo "git(/srv/git/project.git)"
-- >>> P.parseMaybe writeGitRepo "git(/srv/git/project.git:branch)"
-- Just (WriteGitRepo {url = "/srv/git/project.git", branch = Nothing})
-- Just (WriteGitRepo {url = "/srv/git/project.git", branch = Just "branch"})
--
-- >>> P.parseMaybe writeGitRepo "git(file:///srv/git/project.git)"
-- >>> P.parseMaybe writeGitRepo "git(file:///srv/git/project.git:branch)"
-- Just (WriteGitRepo {url = "file:///srv/git/project.git", branch = Nothing})
-- Just (WriteGitRepo {url = "file:///srv/git/project.git", branch = Just "branch"})
--
-- >>> P.parseMaybe writeGitRepo "git(https://example.com/gitproject.git)"
-- >>> P.parseMaybe writeGitRepo "git(https://example.com/gitproject.git:base)"
-- Just (WriteGitRepo {url = "https://example.com/gitproject.git", branch = Nothing})
-- Just (WriteGitRepo {url = "https://example.com/gitproject.git", branch = Just "base"})
--
-- >>> P.parseMaybe writeGitRepo "git(ssh://user@server/project.git)"
-- >>> P.parseMaybe writeGitRepo "git(ssh://user@server/project.git:branch)"
-- >>> P.parseMaybe writeGitRepo "git(ssh://server/project.git)"
-- >>> P.parseMaybe writeGitRepo "git(ssh://server/project.git:branch)"
-- Just (WriteGitRepo {url = "ssh://user@server/project.git", branch = Nothing})
-- Just (WriteGitRepo {url = "ssh://user@server/project.git", branch = Just "branch"})
-- Just (WriteGitRepo {url = "ssh://server/project.git", branch = Nothing})
-- Just (WriteGitRepo {url = "ssh://server/project.git", branch = Just "branch"})
--
-- >>> P.parseMaybe writeGitRepo "git(server:project)"
-- >>> P.parseMaybe writeGitRepo "git(user@server:project.git:branch)"
-- Just (WriteGitRepo {url = "server:project", branch = Nothing})
-- Just (WriteGitRepo {url = "user@server:project.git", branch = Just "branch"})
writeGitRepo :: P WriteGitRepo
writeGitRepo = P.label "repo root for writing" $ do
  C.string "git("
  uri <- parseGitProtocol
  treeish <- P.optional gitTreeishSuffix
  C.string ")"
  pure WriteGitRepo {url = printProtocol uri, branch = treeish}

-- | A parser for the deprecated format of git URLs, which may still exist in old GitURL
-- unisonConfigs.
--
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "/srv/git/project.git:.namespace"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "/srv/git/project.git:branch:.namespace"
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "/srv/git/project.git", branch = Nothing}, path = namespace})
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "/srv/git/project.git", branch = Just "branch"}, path = namespace})
--
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "file:///srv/git/project.git"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "file:///srv/git/project.git:branch"
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "file:///srv/git/project.git", branch = Nothing}, path = })
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "file:///srv/git/project.git", branch = Just "branch"}, path = })
--
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "https://example.com/gitproject.git"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "https://example.com/gitproject.git:base"
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "https://example.com/gitproject.git", branch = Nothing}, path = })
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "https://example.com/gitproject.git", branch = Just "base"}, path = })
--
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "ssh://user@server/project.git"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "ssh://user@server/project.git:branch"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "ssh://server/project.git"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "ssh://server/project.git:branch"
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "ssh://user@server/project.git", branch = Nothing}, path = })
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "ssh://user@server/project.git", branch = Just "branch"}, path = })
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "ssh://server/project.git", branch = Nothing}, path = })
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "ssh://server/project.git", branch = Just "branch"}, path = })
--
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "server:project"
-- >>> P.parseMaybe deprecatedWriteGitRemotePath "user@server:project.git:branch"
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "server:project", branch = Nothing}, path = })
-- Just (WriteGitRemotePath {repo = WriteGitRepo {url = "user@server:project.git", branch = Just "branch"}, path = })
deprecatedWriteGitRemotePath :: P WriteGitRemotePath
deprecatedWriteGitRemotePath = P.label "generic write repo" $ do
  repo <- deprecatedWriteGitRepo
  path <- P.optional (C.char ':' *> absolutePath)
  pure WriteGitRemotePath {repo, path = fromMaybe Path.empty path}
  where
    deprecatedWriteGitRepo :: P WriteGitRepo
    deprecatedWriteGitRepo = do
      P.label "repo root for writing" $ do
        uri <- parseGitProtocol
        treeish <- P.optional deprecatedTreeishSuffix
        pure WriteGitRepo {url = printProtocol uri, branch = treeish}
    deprecatedTreeishSuffix :: P Text
    deprecatedTreeishSuffix = P.label "git treeish" . P.try $ do
      void $ C.char ':'
      notdothash <- P.noneOf @[] ".#:"
      rest <- P.takeWhileP (Just "not colon") (/= ':')
      pure $ Text.cons notdothash rest

-- git(myrepo@git.com).foo.bar
writeGitRemotePath :: P WriteGitRemotePath
writeGitRemotePath = P.label "generic write repo" $ do
  repo <- writeGitRepo
  path <- P.optional absolutePath
  pure WriteGitRemotePath {repo, path = fromMaybe Path.empty path}

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
parseGitProtocol :: P GitProtocol
parseGitProtocol =
  P.label "parseGitProtocol" $
    fileRepo <|> httpsRepo <|> sshRepo <|> scpRepo <|> localRepo
  where
    localRepo, fileRepo, httpsRepo, sshRepo, scpRepo :: P GitProtocol
    parsePath =
      P.takeWhile1P
        (Just "repo path character")
        (\c -> not (isSpace c || c == ':' || c == ')'))
    localRepo = LocalProtocol <$> parsePath
    fileRepo = P.label "fileRepo" $ do
      void $ C.string "file://"
      FileProtocol <$> parsePath
    httpsRepo = P.label "httpsRepo" $ do
      void $ C.string "https://"
      HttpsProtocol <$> P.optional userInfo <*> parseHostInfo <*> parsePath
    sshRepo = P.label "sshRepo" $ do
      void $ C.string "ssh://"
      SshProtocol <$> P.optional userInfo <*> parseHostInfo <*> parsePath
    scpRepo =
      P.label "scpRepo" . P.try $
        ScpProtocol <$> P.optional userInfo <*> parseHost <* C.string ":" <*> parsePath
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
                  void $ C.char ':'
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

-- >>> P.parseMaybe namespaceHashPath "#nshashabc.path.foo.bar"
-- Just (Just #nshashabc,path.foo.bar)
--
-- >>> P.parseMaybe namespaceHashPath ".path.foo.bar"
-- Just (Nothing,path.foo.bar)
--
-- >>> P.parseMaybe namespaceHashPath "#nshashabc"
-- Just (Just #nshashabc,)
--
-- >>> P.parseMaybe namespaceHashPath "#nshashabc."
-- Just (Just #nshashabc,)
--
-- >>> P.parseMaybe namespaceHashPath "."
-- Just (Nothing,)
namespaceHashPath :: P (Maybe ShortBranchHash, Path)
namespaceHashPath = do
  sbh <- P.optional shortBranchHash
  p <- P.optional absolutePath
  pure (sbh, fromMaybe Path.empty p)

-- >>> P.parseMaybe absolutePath "."
-- Just
--
-- >>> P.parseMaybe absolutePath ".path.foo.bar"
-- Just path.foo.bar
absolutePath :: P Path
absolutePath = do
  void $ C.char '.'
  Path . Seq.fromList <$> P.sepBy nameSegment (C.char '.')

nameSegment :: P NameSegment
nameSegment =
  NameSegment . Text.pack
    <$> ( (:) <$> P.satisfy Unison.Syntax.Lexer.wordyIdStartChar
            <*> P.many (P.satisfy Unison.Syntax.Lexer.wordyIdChar)
        )

gitTreeishSuffix :: P Text
gitTreeishSuffix = P.label "git treeish" . P.try $ do
  void $ C.char ':'
  P.takeWhile1P (Just "not close paren") (/= ')')

shortBranchHash :: P ShortBranchHash
shortBranchHash = P.label "short branch hash" $ do
  void $ C.char '#'
  ShortBranchHash
    <$> P.takeWhile1P (Just "base32hex chars") (`elem` Hash.validBase32HexChars)
