{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.UriParser (repoPath, GitProtocol(..), printProtocol) where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Data.Text as Text

import Data.Text (Text)
import Unison.Codebase.Path (Path(..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo(..), RemoteNamespace)
import Unison.Codebase.ShortBranchHash (ShortBranchHash(..))
import Unison.Prelude
import qualified Unison.Hash as Hash
import qualified Unison.Lexer
import Unison.Codebase.NameSegment (NameSegment(..))
import Data.Sequence as Seq
import Data.Char (isAlphaNum, isSpace, isDigit)
import Unison.Codebase.Editor.GitProtocol (GitProtocol(..), User(..), HostInfo(..), printProtocol)

type P = P.Parsec () Text

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
-- Git Protocol (obsolete)
repoPath :: P RemoteNamespace
repoPath = P.label "generic git repo" $ do
  protocol <- parseProtocol
  treeish <- P.optional treeishSuffix
  let repo = GitRepo protocol treeish
  nshashPath <- P.optional (C.char ':' *> namespaceHashPath)
  case nshashPath of
    Nothing -> pure (repo, Nothing, Path.empty)
    Just (sbh, p) -> pure (repo, sbh, p)

-- does this not exist somewhere in megaparsec? yes in 7.0
symbol :: Text -> P Text
symbol = L.symbol (pure ())

-- doesn't yet handle basic authentication like https://user:pass@server.com
-- (does anyone even want that?)
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

treeishSuffix :: P Text
treeishSuffix = P.label "git treeish" . P.try $ do
  void $ C.char ':'
  notdothash <- C.noneOf @[] ".#:"
  rest <- P.takeWhileP (Just "not colon") (/= ':')
  pure $ Text.cons notdothash rest

shortBranchHash :: P ShortBranchHash
shortBranchHash = P.label "short branch hash" $ do
  void $ C.char '#'
  ShortBranchHash <$>
    P.takeWhile1P (Just "base32hex chars") (`elem` Hash.validBase32HexChars)
