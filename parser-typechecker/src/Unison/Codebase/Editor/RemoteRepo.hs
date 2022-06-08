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
import Unison.Share.Types

data ReadRepo
  = ReadRepoGit ReadGitRepo
  | ReadRepoShare Codeserver
  deriving stock (Eq, Ord, Show)

-- | A not-yet-resolved codeserver location, specifically one which came from a user input.
-- It's tagged with whether it was manually specified or not.
data CodeserverLocation
  = DefaultShare
  | CustomShare CodeserverRoot
  deriving stock (Eq, Ord, Show)

codeserverLocation :: Codeserver -> CodeserverLocation
codeserverLocation Codeserver {codeserverRoot, codeserverProvenance} =
  case codeserverProvenance of
    DefaultCodeserver -> DefaultShare
    CustomCodeserver -> CustomShare codeserverRoot

-- |
-- >>> :set -XOverloadedLists
-- >>> import Data.Maybe (fromJust)
-- >>> import Network.URI
-- >>> displayShareCodeserver DefaultCodeserver "share" ["base", "List"]
-- "share.base.List"
-- >>> displayShareCodeserver DefaultCodeserver "share" []
-- "share"
-- >>> displayShareCodeserver (CustomCodeserver . fromJust $ parseURI "https://share-next.unison-lang.org/api" >>= codeserverFromURI ) "unison" ["base", "List"]
-- "share(https://share-next.unison-lang.org:443/api).unison.base.List"
displayShareCodeserver :: CodeserverLocation -> Text -> Path -> Text
displayShareCodeserver cs repo path =
  let shareServer = case cs of
        DefaultShare -> ""
        CustomShare cu -> "share(" <> tShow cu <> ")."
   in shareServer <> repo <> maybePrintPath path

-- -- |
-- -- >>> :set -XOverloadedLists
-- -- >>> import Data.Maybe (fromJust)
-- -- >>> import Network.URI
-- -- >>> displayCodeserver DefaultCodeserver "share" ["base", "List"]
-- -- "share.base.List"
-- -- >>> displayCodeserver DefaultCodeserver "share" []
-- -- "share"
-- -- >>> displayCodeserver (CustomCodeserver . fromJust $ parseURI "https://share-next.unison-lang.org/api" >>= codeserverFromURI ) "unison" ["base", "List"]
-- -- "share(https://share-next.unison-lang.org:443/api).unison.base.List"
-- displayCodeserver :: Codeserver -> Text -> Path -> Text
-- displayCodeserver Codeserver {codeserverProvenance, codeserverRoot} repo path =
--   let shareServer = case codeserverProvenance of
--         DefaultCodeserver -> ""
--         CustomCodeserver -> "share(" <> tShow codeserverRoot <> ")."
--    in shareServer <> repo <> maybePrintPath path

data ReadGitRepo = ReadGitRepo {url :: Text, ref :: Maybe Text}
  deriving stock (Eq, Ord, Show)

data WriteRepo
  = WriteRepoGit WriteGitRepo
  | WriteRepoShare Codeserver
  deriving stock (Eq, Ord, Show)

data WriteGitRepo = WriteGitRepo {url :: Text, branch :: Maybe Text}
  deriving stock (Eq, Ord, Show)

writeToRead :: WriteRepo -> ReadRepo
writeToRead = \case
  WriteRepoGit repo -> ReadRepoGit (writeToReadGit repo)
  WriteRepoShare repo -> ReadRepoShare repo

writeToReadGit :: WriteGitRepo -> ReadGitRepo
writeToReadGit = \case
  WriteGitRepo {url, branch} -> ReadGitRepo {url = url, ref = branch}

writePathToRead :: WriteRemotePath server -> ReadRemoteNamespace server
writePathToRead = \case
  WriteRemotePathGit WriteGitRemotePath {repo, path} ->
    ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo = writeToReadGit repo, sbh = Nothing, path}
  WriteRemotePathShare WriteShareRemotePath {server, repo, path} ->
    ReadRemoteNamespaceShare ReadShareRemoteNamespace {server, repo, path}

printReadGitRepo :: ReadGitRepo -> Text
printReadGitRepo ReadGitRepo {url, ref} =
  "git(" <> url <> Monoid.fromMaybe (Text.cons ':' <$> ref) <> ")"

printWriteGitRepo :: WriteGitRepo -> Text
printWriteGitRepo WriteGitRepo {url, branch} = "git(" <> url <> Monoid.fromMaybe (Text.cons ':' <$> branch) <> ")"

-- | print remote namespace
printNamespace :: ReadRemoteNamespace CodeserverLocation -> Text
printNamespace = \case
  ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo, sbh, path} ->
    printReadGitRepo repo <> maybePrintSBH sbh <> maybePrintPath path
    where
      maybePrintSBH = \case
        Nothing -> mempty
        Just sbh -> "#" <> SBH.toText sbh
  ReadRemoteNamespaceShare ReadShareRemoteNamespace {server, repo, path} ->
    displayShareCodeserver server repo path

-- | Render a 'WriteRemotePath' as text.
printWriteRemotePath :: WriteRemotePath CodeserverLocation -> Text
printWriteRemotePath = \case
  WriteRemotePathGit (WriteGitRemotePath {repo, path}) ->
    printWriteGitRepo repo <> maybePrintPath path
  WriteRemotePathShare (WriteShareRemotePath {server, repo, path}) ->
    displayShareCodeserver server repo path

maybePrintPath :: Path -> Text
maybePrintPath path =
  if path == Path.empty
    then mempty
    else "." <> Path.toText path

data ReadRemoteNamespace server
  = ReadRemoteNamespaceGit ReadGitRemoteNamespace
  | ReadRemoteNamespaceShare (ReadShareRemoteNamespace server)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data ReadGitRemoteNamespace = ReadGitRemoteNamespace
  { repo :: ReadGitRepo,
    sbh :: Maybe ShortBranchHash,
    path :: Path
  }
  deriving stock (Eq, Show)

data ReadShareRemoteNamespace server = ReadShareRemoteNamespace
  { server :: server,
    repo :: Text,
    -- sbh :: Maybe ShortBranchHash, -- maybe later
    path :: Path
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data WriteRemotePath server
  = WriteRemotePathGit WriteGitRemotePath
  | WriteRemotePathShare (WriteShareRemotePath server)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data WriteGitRemotePath = WriteGitRemotePath
  { repo :: WriteGitRepo,
    path :: Path
  }
  deriving stock (Eq, Show)

data WriteShareRemotePath server = WriteShareRemotePath
  { server :: server,
    repo :: Text,
    path :: Path
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
