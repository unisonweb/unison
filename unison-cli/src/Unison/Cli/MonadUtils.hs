-- | This module contains miscellaneous helper utils for rote actions in the Cli monad, like resolving a relative path
-- to an absolute path, per the current path.
module Unison.Cli.MonadUtils
  ( -- * @.unisonConfig@ things
    getConfig,

    -- * Paths
    getCurrentPath,
    resolvePath',
    resolveSplit',

    -- * Branches

    -- ** Resolving branch identifiers
    resolveAbsBranchId,
    resolveShortBranchHash,

    -- ** Getting/setting branches
    getRootBranch,
    setRootBranch,
    modifyRootBranch,
    getRootBranch0,
    getCurrentBranch,
    getCurrentBranch0,
    getBranchAt,
    getBranch0At,
    getLastSavedRoot,
    setLastSavedRoot,
    getMaybeBranchAt,
    expectBranchAtPath',
    assertNoBranchAtPath',
    branchExistsAtPath',

    -- ** Updating branches
    updateRoot,
    updateAtM,
    updateAt,

    -- * Terms
    getTermsAt,

    -- * Types
    getTypesAt,

    -- * Patches

    -- ** Default patch
    defaultPatchNameSegment,
    defaultPatchPath,

    -- ** Getting patches
    getPatchAt,
    getMaybePatchAt,
    expectPatchAt,
    assertNoPatchAt,

    -- * Latest touched Unison file
    getLatestFile,
    expectLatestFile,
    getLatestTypecheckedFile,
    expectLatestTypecheckedFile,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator
import qualified Data.Set as Set
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.BranchUtil as BranchUtil
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.Editor.Input as Input
import qualified Unison.Codebase.Editor.Output as Output
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Patch as Patch
import Unison.Codebase.Path (Path' (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.HashQualified' as HQ'
import Unison.NameSegment (NameSegment)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Symbol (Symbol)
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.Util.Set as Set
import UnliftIO.STM

------------------------------------------------------------------------------------------------------------------------
-- .unisonConfig things

-- | Lookup a config value by key.
getConfig :: Configurator.Configured a => Text -> Cli r (Maybe a)
getConfig key = do
  Cli.Env {config} <- ask
  liftIO (Configurator.lookup config key)

------------------------------------------------------------------------------------------------------------------------
-- Getting paths, path resolution, etc.

-- | Get the current path.
getCurrentPath :: Cli r Path.Absolute
getCurrentPath = do
  use #currentPath

-- | Resolve a @Path'@ to a @Path.Absolute@, per the current path.
resolvePath' :: Path' -> Cli r Path.Absolute
resolvePath' path = do
  currentPath <- getCurrentPath
  pure (Path.resolve currentPath path)

-- | Resolve a path split, per the current path.
resolveSplit' :: (Path', a) -> Cli r (Path.Absolute, a)
resolveSplit' =
  traverseOf _1 resolvePath'

------------------------------------------------------------------------------------------------------------------------
-- Branch resolution

-- | Resolve an @AbsBranchId@ to the corresponding @Branch IO@, or fail if no such branch hash is found. (Non-existent
-- branches by path are OK - the empty branch will be returned).
resolveAbsBranchId :: Input.AbsBranchId -> Cli r (Branch IO)
resolveAbsBranchId = \case
  Left hash -> resolveShortBranchHash hash
  Right path -> getBranchAt path

-- | Resolve a @ShortBranchHash@ to the corresponding @Branch IO@, or fail if no such branch hash is found.
resolveShortBranchHash :: ShortBranchHash -> Cli r (Branch IO)
resolveShortBranchHash hash = do
  Cli.time "resolveShortBranchHash" do
    Cli.Env {codebase} <- ask
    hashSet <- liftIO (Codebase.branchHashesByPrefix codebase hash)
    len <- liftIO (Codebase.branchHashLength codebase)
    h <-
      Set.asSingleton hashSet & onNothing do
        Cli.returnEarly
          if Set.null hashSet
            then Output.NoBranchWithHash hash
            else Output.BranchHashAmbiguous hash (Set.map (SBH.fromHash len) hashSet)
    branch <- liftIO (Codebase.getBranchForHash codebase h)
    pure (fromMaybe Branch.empty branch)

------------------------------------------------------------------------------------------------------------------------
-- Getting/Setting branches

-- | Get the root branch.
getRootBranch :: Cli r (Branch IO)
getRootBranch = do
  use #root >>= atomically . readTMVar

-- | Get the root branch0.
getRootBranch0 :: Cli r (Branch0 IO)
getRootBranch0 =
  Branch.head <$> getRootBranch

-- | Set a new root branch.
-- Note: This does _not_ update the codebase, the caller is responsible for that.
setRootBranch :: Branch IO -> Cli r ()
setRootBranch b = do
  void $ modifyRootBranch (const b)

-- | Get the root branch.
modifyRootBranch :: (Branch IO -> Branch IO) -> Cli r (Branch IO)
modifyRootBranch f = do
  rootVar <- use #root
  atomically do
    root <- takeTMVar rootVar
    let newRoot = f root
    putTMVar rootVar newRoot
    pure newRoot

-- | Get the current branch.
getCurrentBranch :: Cli r (Branch IO)
getCurrentBranch = do
  path <- getCurrentPath
  getBranchAt path

-- | Get the current branch0.
getCurrentBranch0 :: Cli r (Branch0 IO)
getCurrentBranch0 = do
  Branch.head <$> getCurrentBranch

-- | Get the last saved root.
getLastSavedRoot :: Cli r (Branch IO)
getLastSavedRoot = do
  use #lastSavedRoot >>= atomically . readTMVar

-- | Set a new root branch.
-- Note: This does _not_ update the codebase, the caller is responsible for that.
setLastSavedRoot :: Branch IO -> Cli r ()
setLastSavedRoot b = do
  lastRootVar <- use #lastSavedRoot
  _ <- liftIO . atomically $ swapTMVar lastRootVar b
  pure ()

-- | Get the branch at an absolute path.
getBranchAt :: Path.Absolute -> Cli r (Branch IO)
getBranchAt path =
  getMaybeBranchAt path <&> fromMaybe Branch.empty

-- | Get the branch0 at an absolute path.
getBranch0At :: Path.Absolute -> Cli r (Branch0 IO)
getBranch0At path =
  Branch.head <$> getBranchAt path

-- | Get the maybe-branch at an absolute path.
getMaybeBranchAt :: Path.Absolute -> Cli r (Maybe (Branch IO))
getMaybeBranchAt path = do
  rootBranch <- getRootBranch
  pure (Branch.getAt (Path.unabsolute path) rootBranch)

-- | Get the branch at an absolute or relative path, or return early if there's no such branch.
expectBranchAtPath' :: Path' -> Cli r (Branch IO)
expectBranchAtPath' path0 = do
  path <- resolvePath' path0
  getMaybeBranchAt path & onNothingM (Cli.returnEarly (Output.BranchNotFound path0))

-- | Assert that there's "no branch" at an absolute or relative path, or return early if there is one, where "no branch"
-- means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
assertNoBranchAtPath' :: Path' -> Cli r ()
assertNoBranchAtPath' path' = do
  whenM (branchExistsAtPath' path') do
    Cli.returnEarly (Output.BranchAlreadyExists path')

-- | Check if there's a branch at an absolute or relative path
--
-- "no branch" means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
branchExistsAtPath' :: Path' -> Cli r Bool
branchExistsAtPath' path' = do
  path <- resolvePath' path'
  getMaybeBranchAt path <&> \case
    Nothing -> False
    Just branch ->
      not (Branch.isEmpty0 (Branch.head branch))

------------------------------------------------------------------------------------------------------------------------
-- Getting terms

getTermsAt :: (Path.Absolute, HQ'.HQSegment) -> Cli r (Set Referent)
getTermsAt path = do
  rootBranch0 <- getRootBranch0
  pure (BranchUtil.getTerm (Path.convert path) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting types

getTypesAt :: (Path.Absolute, HQ'.HQSegment) -> Cli r (Set TypeReference)
getTypesAt path = do
  rootBranch0 <- getRootBranch0
  pure (BranchUtil.getType (Path.convert path) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting patches

defaultPatchNameSegment :: NameSegment
defaultPatchNameSegment = "patch"

-- | The default patch path.
defaultPatchPath :: Path.Split'
defaultPatchPath =
  (Path.RelativePath' (Path.Relative Path.empty), defaultPatchNameSegment)

-- | Get the patch at a path, or the empty patch if there's no such patch.
getPatchAt :: Path.Split' -> Cli r Patch
getPatchAt path =
  getMaybePatchAt path <&> fromMaybe Patch.empty

-- | Get the patch at a path.
getMaybePatchAt :: Path.Split' -> Cli r (Maybe Patch)
getMaybePatchAt path0 = do
  (path, name) <- resolveSplit' path0
  branch <- getBranch0At path
  liftIO (Branch.getMaybePatch name branch)

-- | Get the patch at a path, or return early if there's no such patch.
expectPatchAt :: Path.Split' -> Cli r Patch
expectPatchAt path =
  getMaybePatchAt path & onNothingM (Cli.returnEarly (Output.PatchNotFound path))

-- | Assert that there's no patch at a path, or return early if there is one.
assertNoPatchAt :: Path.Split' -> Cli r ()
assertNoPatchAt path = do
  whenJustM (getMaybePatchAt path) \_ -> Cli.returnEarly (Output.PatchAlreadyExists path)

------------------------------------------------------------------------------------------------------------------------
-- Latest (typechecked) unison file utils

getLatestFile :: Cli r (Maybe (FilePath, Bool))
getLatestFile = do
  use #latestFile

expectLatestFile :: Cli r (FilePath, Bool)
expectLatestFile = do
  getLatestFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- | Get the latest typechecked unison file.
getLatestTypecheckedFile :: Cli r (Maybe (TypecheckedUnisonFile Symbol Ann))
getLatestTypecheckedFile = do
  use #latestTypecheckedFile

-- | Get the latest typechecked unison file, or return early if there isn't one.
expectLatestTypecheckedFile :: Cli r (TypecheckedUnisonFile Symbol Ann)
expectLatestTypecheckedFile =
  getLatestTypecheckedFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM ::
  Text ->
  Path.Absolute ->
  (Branch IO -> Cli r (Branch IO)) ->
  Cli r Bool
updateAtM reason (Path.Absolute p) f = do
  b <- getLastSavedRoot
  b' <- Branch.modifyAtM p f b
  updateRoot b' reason
  pure $ b /= b'

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAt ::
  Text ->
  Path.Absolute ->
  (Branch IO -> Branch IO) ->
  Cli r Bool
updateAt reason p f = do
  updateAtM reason p (pure . f)

updateRoot :: Branch IO -> Text -> Cli r ()
updateRoot new reason =
  Cli.time "updateRoot" do
    Cli.Env {codebase} <- ask
    old <- getLastSavedRoot
    when (old /= new) do
      setRootBranch new
      liftIO (Codebase.putRootBranch codebase new)
      liftIO (Codebase.appendReflog codebase reason old new)
      setLastSavedRoot new
