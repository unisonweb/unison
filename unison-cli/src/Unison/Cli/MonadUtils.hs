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
    getLastSavedRootHash,
    setLastSavedRootHash,
    getMaybeBranchAt,
    expectBranchAtPath',
    assertNoBranchAtPath',
    branchExistsAtPath',

    -- ** Updating branches
    stepAt',
    stepAt,
    stepAtM,
    stepAtNoSync',
    stepAtNoSync,
    stepManyAt,
    stepManyAtMNoSync,
    stepManyAtNoSync,
    syncRoot,
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
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Causal as V2Causal
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
import Unison.Codebase.Path (Path, Path' (..))
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
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
getConfig :: Configurator.Configured a => Text -> Cli (Maybe a)
getConfig key = do
  Cli.Env {config} <- ask
  liftIO (Configurator.lookup config key)

------------------------------------------------------------------------------------------------------------------------
-- Getting paths, path resolution, etc.

-- | Get the current path.
getCurrentPath :: Cli Path.Absolute
getCurrentPath = do
  use #currentPath

-- | Resolve a @Path'@ to a @Path.Absolute@, per the current path.
resolvePath' :: Path' -> Cli Path.Absolute
resolvePath' path = do
  currentPath <- getCurrentPath
  pure (Path.resolve currentPath path)

-- | Resolve a path split, per the current path.
resolveSplit' :: (Path', a) -> Cli (Path.Absolute, a)
resolveSplit' =
  traverseOf _1 resolvePath'

------------------------------------------------------------------------------------------------------------------------
-- Branch resolution

-- | Resolve an @AbsBranchId@ to the corresponding @Branch IO@, or fail if no such branch hash is found. (Non-existent
-- branches by path are OK - the empty branch will be returned).
resolveAbsBranchId :: Input.AbsBranchId -> Cli (Branch IO)
resolveAbsBranchId = \case
  Left hash -> resolveShortBranchHash hash
  Right path -> getBranchAt path

-- | Resolve a @ShortBranchHash@ to the corresponding @Branch IO@, or fail if no such branch hash is found.
resolveShortBranchHash :: ShortBranchHash -> Cli (Branch IO)
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
getRootBranch :: Cli (Branch IO)
getRootBranch = do
  use #root >>= atomically . readTMVar

-- | Get the root branch0.
getRootBranch0 :: Cli (Branch0 IO)
getRootBranch0 =
  Branch.head <$> getRootBranch

-- | Set a new root branch.
-- Note: This does _not_ update the codebase, the caller is responsible for that.
setRootBranch :: Branch IO -> Cli ()
setRootBranch b = do
  void $ modifyRootBranch (const b)

-- | Get the root branch.
modifyRootBranch :: (Branch IO -> Branch IO) -> Cli (Branch IO)
modifyRootBranch f = do
  rootVar <- use #root
  atomically do
    root <- takeTMVar rootVar
    let newRoot = f root
    putTMVar rootVar $! newRoot
    pure newRoot

-- | Get the current branch.
getCurrentBranch :: Cli (Branch IO)
getCurrentBranch = do
  path <- getCurrentPath
  getBranchAt path

-- | Get the current branch0.
getCurrentBranch0 :: Cli (Branch0 IO)
getCurrentBranch0 = do
  Branch.head <$> getCurrentBranch

-- | Get the last saved root hash.
getLastSavedRootHash :: Cli V2Branch.CausalHash
getLastSavedRootHash = do
  use #lastSavedRootHash

-- | Set a new root branch.
-- Note: This does _not_ update the codebase, the caller is responsible for that.
setLastSavedRootHash :: V2Branch.CausalHash -> Cli ()
setLastSavedRootHash ch = do
  #lastSavedRootHash .= ch

-- | Get the branch at an absolute path.
getBranchAt :: Path.Absolute -> Cli (Branch IO)
getBranchAt path =
  getMaybeBranchAt path <&> fromMaybe Branch.empty

-- | Get the branch0 at an absolute path.
getBranch0At :: Path.Absolute -> Cli (Branch0 IO)
getBranch0At path =
  Branch.head <$> getBranchAt path

-- | Get the maybe-branch at an absolute path.
getMaybeBranchAt :: Path.Absolute -> Cli (Maybe (Branch IO))
getMaybeBranchAt path = do
  rootBranch <- getRootBranch
  pure (Branch.getAt (Path.unabsolute path) rootBranch)

-- | Get the branch at an absolute or relative path, or return early if there's no such branch.
expectBranchAtPath' :: Path' -> Cli (Branch IO)
expectBranchAtPath' path0 = do
  path <- resolvePath' path0
  getMaybeBranchAt path & onNothingM (Cli.returnEarly (Output.BranchNotFound path0))

-- | Assert that there's "no branch" at an absolute or relative path, or return early if there is one, where "no branch"
-- means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
assertNoBranchAtPath' :: Path' -> Cli ()
assertNoBranchAtPath' path' = do
  whenM (branchExistsAtPath' path') do
    Cli.returnEarly (Output.BranchAlreadyExists path')

-- | Check if there's a branch at an absolute or relative path
--
-- "no branch" means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
branchExistsAtPath' :: Path' -> Cli Bool
branchExistsAtPath' path' = do
  absPath <- resolvePath' path'
  Cli.Env {codebase} <- ask
  liftIO $ do
    causal <- Codebase.getShallowCausalFromRoot codebase Nothing (Path.unabsolute absPath)
    branch <- V2Causal.value causal
    isEmpty <- Codebase.runTransaction codebase $ V2Branch.isEmpty branch
    pure (not isEmpty)

------------------------------------------------------------------------------------------------------------------------
-- Updating branches

stepAt ::
  Text ->
  (Path, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepAt cause = stepManyAt @[] cause . pure

stepAt' ::
  Text ->
  (Path, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepAt' cause = stepManyAt' @[] cause . pure

stepAtNoSync' ::
  (Path, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepAtNoSync' = stepManyAtNoSync' @[] . pure

stepAtNoSync ::
  (Path, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepAtNoSync = stepManyAtNoSync @[] . pure

stepAtM ::
  Text ->
  (Path, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepAtM cause = stepManyAtM @[] cause . pure

stepManyAt ::
  Foldable f =>
  Text ->
  f (Path, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepManyAt reason actions = do
  stepManyAtNoSync actions
  syncRoot reason

stepManyAt' ::
  Foldable f =>
  Text ->
  f (Path, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepManyAt' reason actions = do
  res <- stepManyAtNoSync' actions
  syncRoot reason
  pure res

stepManyAtNoSync' ::
  Foldable f =>
  f (Path, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepManyAtNoSync' actions = do
  origRoot <- getRootBranch
  newRoot <- Branch.stepManyAtM actions origRoot
  setRootBranch newRoot
  pure (origRoot /= newRoot)

-- Like stepManyAt, but doesn't update the last saved root
stepManyAtNoSync ::
  Foldable f =>
  f (Path, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepManyAtNoSync actions =
  void . modifyRootBranch $ Branch.stepManyAt actions

stepManyAtM ::
  Foldable f =>
  Text ->
  f (Path, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepManyAtM reason actions = do
  stepManyAtMNoSync actions
  syncRoot reason

stepManyAtMNoSync ::
  Foldable f =>
  f (Path, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepManyAtMNoSync actions = do
  oldRoot <- getRootBranch
  newRoot <- liftIO (Branch.stepManyAtM actions oldRoot)
  setRootBranch newRoot

-- | Sync the in-memory root branch.
syncRoot :: Text -> Cli ()
syncRoot description = do
  rootBranch <- getRootBranch
  updateRoot rootBranch description

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM ::
  Text ->
  Path.Absolute ->
  (Branch IO -> Cli (Branch IO)) ->
  Cli Bool
updateAtM reason (Path.Absolute p) f = do
  b <- getRootBranch
  b' <- Branch.modifyAtM p f b
  updateRoot b' reason
  pure $ b /= b'

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAt ::
  Text ->
  Path.Absolute ->
  (Branch IO -> Branch IO) ->
  Cli Bool
updateAt reason p f = do
  updateAtM reason p (pure . f)

updateRoot :: Branch IO -> Text -> Cli ()
updateRoot new reason =
  Cli.time "updateRoot" do
    Cli.Env {codebase} <- ask
    let newHash = Cv.causalHash1to2 $ Branch.headHash new
    oldHash <- getLastSavedRootHash
    when (oldHash /= newHash) do
      setRootBranch new
      liftIO (Codebase.putRootBranch codebase reason new)
      setLastSavedRootHash newHash

------------------------------------------------------------------------------------------------------------------------
-- Getting terms

getTermsAt :: (Path.Absolute, HQ'.HQSegment) -> Cli (Set Referent)
getTermsAt path = do
  rootBranch0 <- getRootBranch0
  pure (BranchUtil.getTerm (Path.convert path) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting types

getTypesAt :: (Path.Absolute, HQ'.HQSegment) -> Cli (Set TypeReference)
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
getPatchAt :: Path.Split' -> Cli Patch
getPatchAt path =
  getMaybePatchAt path <&> fromMaybe Patch.empty

-- | Get the patch at a path.
getMaybePatchAt :: Path.Split' -> Cli (Maybe Patch)
getMaybePatchAt path0 = do
  (path, name) <- resolveSplit' path0
  branch <- getBranch0At path
  liftIO (Branch.getMaybePatch name branch)

-- | Get the patch at a path, or return early if there's no such patch.
expectPatchAt :: Path.Split' -> Cli Patch
expectPatchAt path =
  getMaybePatchAt path & onNothingM (Cli.returnEarly (Output.PatchNotFound path))

-- | Assert that there's no patch at a path, or return early if there is one.
assertNoPatchAt :: Path.Split' -> Cli ()
assertNoPatchAt path = do
  whenJustM (getMaybePatchAt path) \_ -> Cli.returnEarly (Output.PatchAlreadyExists path)

------------------------------------------------------------------------------------------------------------------------
-- Latest (typechecked) unison file utils

getLatestFile :: Cli (Maybe (FilePath, Bool))
getLatestFile = do
  use #latestFile

expectLatestFile :: Cli (FilePath, Bool)
expectLatestFile = do
  getLatestFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- | Get the latest typechecked unison file.
getLatestTypecheckedFile :: Cli (Maybe (TypecheckedUnisonFile Symbol Ann))
getLatestTypecheckedFile = do
  use #latestTypecheckedFile

-- | Get the latest typechecked unison file, or return early if there isn't one.
expectLatestTypecheckedFile :: Cli (TypecheckedUnisonFile Symbol Ann)
expectLatestTypecheckedFile =
  getLatestTypecheckedFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)
