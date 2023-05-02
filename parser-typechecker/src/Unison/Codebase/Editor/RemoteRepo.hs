module Unison.Codebase.Editor.RemoteRepo where

import Control.Lens (Lens')
import qualified Control.Lens as Lens
import qualified Data.Text as Text
import Data.Void (absurd)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import qualified Unison.Codebase.ShortCausalHash as SCH
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.Types
import qualified Unison.Util.Monoid as Monoid

data ReadRepo
  = ReadRepoGit ReadGitRepo
  | ReadRepoShare ShareCodeserver
  deriving stock (Eq, Ord, Show)

data ShareCodeserver
  = DefaultCodeserver
  | CustomCodeserver CodeserverURI
  deriving stock (Eq, Ord, Show)

newtype ShareUserHandle = ShareUserHandle {shareUserHandleToText :: Text}
  deriving stock (Eq, Ord, Show)

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
displayShareCodeserver :: ShareCodeserver -> ShareUserHandle -> Path -> Text
displayShareCodeserver cs shareUser path =
  let shareServer = case cs of
        DefaultCodeserver -> ""
        CustomCodeserver cu -> "share(" <> tShow cu <> ")."
   in shareServer <> shareUserHandleToText shareUser <> maybePrintPath path

data ReadGitRepo = ReadGitRepo {url :: Text, ref :: Maybe Text}
  deriving stock (Eq, Ord, Show)

data WriteRepo
  = WriteRepoGit WriteGitRepo
  | WriteRepoShare ShareCodeserver
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

writeNamespaceToRead :: WriteRemoteNamespace Void -> ReadRemoteNamespace void
writeNamespaceToRead = \case
  WriteRemoteNamespaceGit WriteGitRemoteNamespace {repo, path} ->
    ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo = writeToReadGit repo, sch = Nothing, path}
  WriteRemoteNamespaceShare WriteShareRemoteNamespace {server, repo, path} ->
    ReadShare'LooseCode ReadShareLooseCode {server, repo, path}
  WriteRemoteProjectBranch v -> absurd v

printReadGitRepo :: ReadGitRepo -> Text
printReadGitRepo ReadGitRepo {url, ref} =
  "git(" <> url <> Monoid.fromMaybe (Text.cons ':' <$> ref) <> ")"

printWriteGitRepo :: WriteGitRepo -> Text
printWriteGitRepo WriteGitRepo {url, branch} = "git(" <> url <> Monoid.fromMaybe (Text.cons ':' <$> branch) <> ")"

-- | print remote namespace
printReadRemoteNamespace :: (a -> Text) -> ReadRemoteNamespace a -> Text
printReadRemoteNamespace printProject = \case
  ReadRemoteNamespaceGit ReadGitRemoteNamespace {repo, sch, path} ->
    printReadGitRepo repo <> maybePrintSCH sch <> maybePrintPath path
    where
      maybePrintSCH = \case
        Nothing -> mempty
        Just sch -> "#" <> SCH.toText sch
  ReadShare'LooseCode ReadShareLooseCode {server, repo, path} -> displayShareCodeserver server repo path
  ReadShare'ProjectBranch project -> printProject project

-- | Render a 'WriteRemoteNamespace' as text.
printWriteRemoteNamespace :: WriteRemoteNamespace (ProjectAndBranch ProjectName ProjectBranchName) -> Text
printWriteRemoteNamespace = \case
  WriteRemoteNamespaceGit (WriteGitRemoteNamespace {repo, path}) ->
    printWriteGitRepo repo <> maybePrintPath path
  WriteRemoteNamespaceShare (WriteShareRemoteNamespace {server, repo, path}) ->
    displayShareCodeserver server repo path
  WriteRemoteProjectBranch projectAndBranch -> into @Text projectAndBranch

maybePrintPath :: Path -> Text
maybePrintPath path =
  if path == Path.empty
    then mempty
    else "." <> Path.toText path

data ReadRemoteNamespace a
  = ReadRemoteNamespaceGit !ReadGitRemoteNamespace
  | ReadShare'LooseCode !ReadShareLooseCode
  | -- | A remote project+branch, specified by name (e.g. @unison/base/main).
    -- Currently assumed to be hosted on Share, though we could include a ShareCodeserver in here, too.
    ReadShare'ProjectBranch !a
  deriving stock (Eq, Show, Generic)

data ReadGitRemoteNamespace = ReadGitRemoteNamespace
  { repo :: !ReadGitRepo,
    sch :: !(Maybe ShortCausalHash),
    path :: !Path
  }
  deriving stock (Eq, Show)

data ReadShareLooseCode = ReadShareLooseCode
  { server :: !ShareCodeserver,
    repo :: !ShareUserHandle,
    -- sch :: Maybe ShortCausalHash, -- maybe later
    path :: !Path
  }
  deriving stock (Eq, Show)

isPublic :: ReadShareLooseCode -> Bool
isPublic ReadShareLooseCode {path} =
  case path of
    ("public" Path.:< _) -> True
    _ -> False

data WriteRemoteNamespace a
  = WriteRemoteNamespaceGit !WriteGitRemoteNamespace
  | WriteRemoteNamespaceShare !WriteShareRemoteNamespace
  | WriteRemoteProjectBranch a
  deriving stock (Eq, Functor, Show)

-- | A lens which focuses the path of a remote namespace.
remotePath_ :: Lens' (WriteRemoteNamespace Void) Path
remotePath_ = Lens.lens getter setter
  where
    getter = \case
      WriteRemoteNamespaceGit (WriteGitRemoteNamespace _ path) -> path
      WriteRemoteNamespaceShare (WriteShareRemoteNamespace _ _ path) -> path
      WriteRemoteProjectBranch v -> absurd v
    setter remote path =
      case remote of
        WriteRemoteNamespaceGit (WriteGitRemoteNamespace repo _) ->
          WriteRemoteNamespaceGit $ WriteGitRemoteNamespace repo path
        WriteRemoteNamespaceShare (WriteShareRemoteNamespace server repo _) ->
          WriteRemoteNamespaceShare $ WriteShareRemoteNamespace server repo path
        WriteRemoteProjectBranch v -> absurd v

data WriteGitRemoteNamespace = WriteGitRemoteNamespace
  { repo :: !WriteGitRepo,
    path :: !Path
  }
  deriving stock (Eq, Generic, Show)

data WriteShareRemoteNamespace = WriteShareRemoteNamespace
  { server :: !ShareCodeserver,
    repo :: !ShareUserHandle,
    path :: !Path
  }
  deriving stock (Eq, Show)
