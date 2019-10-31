{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.UriParser where

import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (alphaNumChar, char, oneOf)
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

repoPath :: P (RemoteRepo, Maybe ShortBranchHash, Path)
repoPath = P.label "generic git repo" $
  fileRepo <|> urlRepo <|> localRepo <|> scpRepo
  where
  localRepo, fileRepo, urlRepo, scpRepo
    :: P (RemoteRepo, Maybe ShortBranchHash, Path)
  localRepo = error "todo"
  fileRepo = error "todo"
  urlRepo = error "todo"
  scpRepo = error "todo" 

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
    void $ char '.'
    P.sepBy1
      ((:) <$> C.satisfy Unison.Lexer.wordyIdStartChar
           <*> P.many (C.satisfy Unison.Lexer.wordyIdChar))
      (char '.')
  case p of
    Nothing -> pure (sbh, Path.empty)
    Just p  -> pure (sbh, makePath p)
  where makePath = Path . Seq.fromList . fmap (NameSegment . Text.pack)

-- double-check me
treeishSuffix :: P Text
treeishSuffix = P.label "git treeish" . P.try $ do
  void $ char ':'
  notdothash <- C.noneOf @[] ".#:"
  rest <- (P.many . C.satisfy) (/= ':')
  pure . Text.pack $ notdothash : rest

webRepoTreeish :: P EncodedRepo
webRepoTreeish = P.label "hosted git repo + treeish" $ do
  svc <- service
  void $ char ':'
  user <- gitwebUser
  void $ char '/'
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
    Text.pack <$> P.many (alphaNumChar <|> char '_' <|> char '-' <|> char '.')

  -- Gitlab: "Path can contain only letters, digits, '_', '-' and '.'.
  --          Cannot start with '-', end in '.git' or end in '.atom'"
  gitwebSlug = gitwebUser

-- <nshash>[.<path>] | [.]<path>
--remoteNameSpace ::

shortBranchHash :: P ShortBranchHash
shortBranchHash = P.label "short branch hash" $ do
  void $ char '#'
  ShortBranchHash . Text.pack <$>
    P.some (oneOf Hash.validBase32HexChars)
