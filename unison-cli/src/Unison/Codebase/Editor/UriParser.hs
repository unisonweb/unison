module Unison.Codebase.Editor.UriParser
  ( repoPath,
    writeGitRepo,
    deprecatedWriteGitRemoteNamespace,
    writeRemoteNamespace,
    writeRemoteNamespaceWith,
    parseReadRemoteNamespace,
    parseReadShareLooseCode,
  )
where

import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Sequence as Seq
import qualified Data.Text as Text
import Data.These (These)
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified U.Util.Base32Hex as Base32Hex
import Unison.Codebase.Editor.RemoteRepo
  ( ReadGitRemoteNamespace (..),
    ReadGitRepo (..),
    ReadRemoteNamespace (..),
    ReadShareLooseCode (..),
    ShareCodeserver (DefaultCodeserver),
    ShareUserHandle (..),
    WriteGitRemoteNamespace (..),
    WriteGitRepo (..),
    WriteRemoteNamespace (..),
    WriteShareRemoteNamespace (..),
  )
import Unison.Codebase.Path (Path (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash (..))
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Project (ProjectBranchName, ProjectName, projectAndBranchNamesParser)
import qualified Unison.Syntax.Lexer
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.Pretty.MegaParsec as P

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

repoPath :: P (ReadRemoteNamespace (These ProjectName ProjectBranchName))
repoPath =
  P.label "generic repo" $
    ReadRemoteNamespaceGit <$> readGitRemoteNamespace
      <|> ReadShare'ProjectBranch <$> projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths
      <|> ReadShare'LooseCode <$> readShareLooseCode

projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths :: P (These ProjectName ProjectBranchName)
projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths =
  P.try do
    projectAndBranch <- projectAndBranchNamesParser
    -- we don't want to succeed parsing the 'foo' off of 'foo.bar', leaving '.bar' behind
    P.notFollowedBy (C.char '.')
    pure projectAndBranch

parseReadRemoteNamespace ::
  String ->
  String ->
  Either (P.Pretty P.ColorText) (ReadRemoteNamespace (These ProjectName ProjectBranchName))
parseReadRemoteNamespace label input =
  let printError err = P.lines [P.string "I couldn't parse the repository address given above.", P.prettyPrintParseError input err]
   in first printError (P.parse repoPath label (Text.pack input))

parseReadShareLooseCode :: String -> String -> Either (P.Pretty P.ColorText) ReadShareLooseCode
parseReadShareLooseCode label input =
  let printError err = P.lines [P.string "I couldn't parse this as a share path.", P.prettyPrintParseError input err]
   in first printError (P.parse readShareLooseCode label (Text.pack input))

-- >>> P.parseMaybe writeRemoteNamespace "unisonweb.base._releases.M4"
-- >>> P.parseMaybe writeRemoteNamespace "git(git@github.com:unisonweb/base:v3)._releases.M3"
-- Just (WriteRemoteNamespaceShare (WriteShareRemoteNamespace {server = ShareRepo, repo = "unisonweb", path = base._releases.M4}))
-- Just (WriteRemoteNamespaceGit (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "git@github.com:unisonweb/base", branch = Just "v3"}, path = _releases.M3}))
writeRemoteNamespace :: P (WriteRemoteNamespace (These ProjectName ProjectBranchName))
writeRemoteNamespace =
  writeRemoteNamespaceWith projectAndBranchNamesParserInTheContextOfAlsoParsingLooseCodePaths

writeRemoteNamespaceWith :: P a -> P (WriteRemoteNamespace a)
writeRemoteNamespaceWith projectBranchParser =
  WriteRemoteProjectBranch <$> projectBranchParser
    <|> WriteRemoteNamespaceGit <$> writeGitRemoteNamespace
    <|> WriteRemoteNamespaceShare <$> writeShareRemoteNamespace

-- >>> P.parseMaybe writeShareRemoteNamespace "unisonweb.base._releases.M4"
-- Just (WriteShareRemoteNamespace {server = ShareRepo, repo = "unisonweb", path = base._releases.M4})
writeShareRemoteNamespace :: P WriteShareRemoteNamespace
writeShareRemoteNamespace =
  P.label "write share remote namespace" $
    WriteShareRemoteNamespace
      <$> pure DefaultCodeserver
      <*> shareUserHandle
      <*> (Path.fromList <$> P.many (C.char '.' *> nameSegment))

-- >>> P.parseMaybe readShareLooseCode ".unisonweb.base._releases.M4"
-- >>> P.parseMaybe readShareLooseCode "unisonweb.base._releases.M4"
-- Nothing
-- Just (ReadShareLooseCode {server = ShareRepo, repo = "unisonweb", path = base._releases.M4})
readShareLooseCode :: P ReadShareLooseCode
readShareLooseCode = do
  P.label "read share loose code" $
    ReadShareLooseCode
      <$> pure DefaultCodeserver
      -- <*> sch <- P.optional shortBranchHash
      <*> shareUserHandle
      <*> (Path.fromList <$> P.many (C.char '.' *> nameSegment))

-- | We're lax in our share user rules here, Share is the source of truth
-- for this stuff and can provide better error messages if required.
--
-- >>> P.parseMaybe shareUserHandle "unison"
-- Just (ShareUserHandle {shareUserHandleToText = "unison"})
--
-- >>> P.parseMaybe shareUserHandle "unison-1337"
-- Just (ShareUserHandle {shareUserHandleToText = "unison-1337"})
shareUserHandle :: P ShareUserHandle
shareUserHandle = do
  ShareUserHandle . Text.pack <$> P.some (P.satisfy \c -> isAlphaNum c || c == '-' || c == '_')

-- >>> P.parseMaybe readGitRemoteNamespace "git(user@server:project.git:branch)#asdf"
-- >>> P.parseMaybe readGitRemoteNamespace "git(user@server:project.git:branch)#asdf."
-- >>> P.parseMaybe readGitRemoteNamespace "git(user@server:project.git:branch)"
-- >>> P.parseMaybe readGitRemoteNamespace "git(git@github.com:unisonweb/base:v3)._releases.M3"
-- >>> P.parseMaybe readGitRemoteNamespace "git( user@server:project.git:branch )#asdf.foo.bar"
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sch = Just #asdf, path = })
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sch = Just #asdf, path = })
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sch = Nothing, path = })
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "git@github.com:unisonweb/base", ref = Just "v3"}, sch = Nothing, path = _releases.M3})
-- Just (ReadGitRemoteNamespace {repo = ReadGitRepo {url = "user@server:project.git", ref = Just "branch"}, sch = Just #asdf, path = foo.bar})
readGitRemoteNamespace :: P ReadGitRemoteNamespace
readGitRemoteNamespace = P.label "generic git repo" $ do
  C.string "git("
  protocol <- parseGitProtocol
  treeish <- P.optional gitTreeishSuffix
  let repo = ReadGitRepo {url = printProtocol protocol, ref = treeish}
  C.string ")"
  nshashPath <- P.optional namespaceHashPath
  pure case nshashPath of
    Nothing -> ReadGitRemoteNamespace {repo, sch = Nothing, path = Path.empty}
    Just (sch, path) -> ReadGitRemoteNamespace {repo, sch, path}

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
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "/srv/git/project.git:.namespace"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "/srv/git/project.git:branch:.namespace"
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "/srv/git/project.git", branch = Nothing}, path = namespace})
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "/srv/git/project.git", branch = Just "branch"}, path = namespace})
--
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "file:///srv/git/project.git"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "file:///srv/git/project.git:branch"
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "file:///srv/git/project.git", branch = Nothing}, path = })
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "file:///srv/git/project.git", branch = Just "branch"}, path = })
--
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "https://example.com/gitproject.git"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "https://example.com/gitproject.git:base"
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "https://example.com/gitproject.git", branch = Nothing}, path = })
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "https://example.com/gitproject.git", branch = Just "base"}, path = })
--
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "ssh://user@server/project.git"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "ssh://user@server/project.git:branch"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "ssh://server/project.git"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "ssh://server/project.git:branch"
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "ssh://user@server/project.git", branch = Nothing}, path = })
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "ssh://user@server/project.git", branch = Just "branch"}, path = })
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "ssh://server/project.git", branch = Nothing}, path = })
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "ssh://server/project.git", branch = Just "branch"}, path = })
--
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "server:project"
-- >>> P.parseMaybe deprecatedWriteGitRemoteNamespace "user@server:project.git:branch"
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "server:project", branch = Nothing}, path = })
-- Just (WriteGitRemoteNamespace {repo = WriteGitRepo {url = "user@server:project.git", branch = Just "branch"}, path = })
deprecatedWriteGitRemoteNamespace :: P WriteGitRemoteNamespace
deprecatedWriteGitRemoteNamespace = P.label "generic write repo" $ do
  repo <- deprecatedWriteGitRepo
  path <- P.optional (C.char ':' *> absolutePath)
  pure WriteGitRemoteNamespace {repo, path = fromMaybe Path.empty path}
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
writeGitRemoteNamespace :: P WriteGitRemoteNamespace
writeGitRemoteNamespace = P.label "generic write repo" $ do
  repo <- writeGitRepo
  path <- P.optional absolutePath
  pure WriteGitRemoteNamespace {repo, path = fromMaybe Path.empty path}

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
        HostInfo
          <$> parseHost
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
namespaceHashPath :: P (Maybe ShortCausalHash, Path)
namespaceHashPath = do
  sch <- P.optional shortCausalHash
  p <- P.optional absolutePath
  pure (sch, fromMaybe Path.empty p)

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
    <$> ( (:)
            <$> P.satisfy Unison.Syntax.Lexer.wordyIdStartChar
            <*> P.many (P.satisfy Unison.Syntax.Lexer.wordyIdChar)
        )

gitTreeishSuffix :: P Text
gitTreeishSuffix = P.label "git treeish" . P.try $ do
  void $ C.char ':'
  P.takeWhile1P (Just "not close paren") (/= ')')

shortCausalHash :: P ShortCausalHash
shortCausalHash = P.label "short causal hash" $ do
  void $ C.char '#'
  ShortCausalHash
    <$> P.takeWhile1P (Just "base32hex chars") (`elem` Base32Hex.validChars)
