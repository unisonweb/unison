-- TODO export list, document
module Unison.Cli.MonadUtils where

import Control.Lens
import Control.Monad.Reader (ask)
import Data.Configurator ()
import qualified Data.Set as Set
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0 (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.BranchUtil as BranchUtil
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Editor.Command as Command
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output
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

------------------------------------------------------------------------------------------------------------------------
-- Latest (typechecked) unison file utils

getLatestFile :: Cli r (Maybe (FilePath, Bool))
getLatestFile = do
  loopState <- Cli.getLoopState
  pure (loopState ^. #latestFile)

expectLatestFile :: Cli r (FilePath, Bool)
expectLatestFile = do
  getLatestFile & onNothingM (Cli.returnEarly NoUnisonFile)

-- | Get the latest typechecked unison file.
getLatestTypecheckedFile :: Cli r (Maybe (TypecheckedUnisonFile Symbol Ann))
getLatestTypecheckedFile = do
  loopState <- Cli.getLoopState
  pure (loopState ^. #latestTypecheckedFile)

-- | Get the latest typechecked unison file, or return early if there isn't one.
expectLatestTypecheckedFile :: Cli r (TypecheckedUnisonFile Symbol Ann)
expectLatestTypecheckedFile =
  getLatestTypecheckedFile & onNothingM (Cli.returnEarly NoUnisonFile)

------------------------------------------------------------------------------------------------------------------------
-- Getting paths, path resolution, etc.

-- | Get the current path.
getCurrentPath :: Cli r Path.Absolute
getCurrentPath = do
  loopState <- Cli.getLoopState
  pure (loopState ^. #currentPath)

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
resolveAbsBranchId :: AbsBranchId -> Cli r (Branch IO)
resolveAbsBranchId = \case
  Left hash -> resolveShortBranchHash hash
  Right path -> getBranchAt path

-- | Resolve a @ShortBranchHash@ to the corresponding @Branch IO@, or fail if no such branch hash is found.
resolveShortBranchHash :: ShortBranchHash -> Cli r (Branch IO)
resolveShortBranchHash hash = do
  Cli.newBlock do
    Cli.time "resolveShortBranchHash"
    Env {codebase} <- ask
    hashSet <- liftIO (Codebase.branchHashesByPrefix codebase hash)
    len <- liftIO (Codebase.branchHashLength codebase)
    h <-
      Set.asSingleton hashSet & onNothing do
        Cli.returnEarly
          if Set.null hashSet
            then NoBranchWithHash hash
            else BranchHashAmbiguous hash (Set.map (SBH.fromHash len) hashSet)
    branch <- liftIO (Codebase.getBranchForHash codebase h)
    pure (fromMaybe Branch.empty branch)

------------------------------------------------------------------------------------------------------------------------
-- Getting branches

-- | Get the root branch.
getRootBranch :: Cli r (Branch IO)
getRootBranch = do
  loopState <- Cli.getLoopState
  pure (loopState ^. #root)

-- | Get the root branch0.
getRootBranch0 :: Cli r (Branch0 IO)
getRootBranch0 =
  Branch.head <$> getRootBranch

-- | Get the current branch.
getCurrentBranch :: Cli r (Branch IO)
getCurrentBranch = do
  path <- getCurrentPath
  getBranchAt path

-- | Get the current branch0.
getCurrentBranch0 :: Cli r (Branch0 IO)
getCurrentBranch0 = do
  Branch.head <$> getCurrentBranch

-- | Get the branch at an absolute path.
getBranchAt :: Path.Absolute -> Cli r (Branch IO)
getBranchAt path =
  getMaybeBranchAt path <&> fromMaybe Branch.empty

-- | Get the maybe-branch at an absolute path.
getMaybeBranchAt :: Path.Absolute -> Cli r (Maybe (Branch IO))
getMaybeBranchAt path = do
  rootBranch <- getRootBranch
  pure (Branch.getAt (Path.unabsolute path) rootBranch)

-- | Get the branch at an absolute or relative path, or return early if there's no such branch.
expectBranchAtPath' :: Path' -> Cli r (Branch IO)
expectBranchAtPath' path0 = do
  path <- resolvePath' path0
  getMaybeBranchAt path & onNothingM (Cli.returnEarly (BranchNotFound path0))

-- | Assert that there's "no branch" at an absolute or relative path, or return early if there is one, where "no branch"
-- means either there's actually no branch, or there is a branch whose head is empty (i.e. it may have a history, but no
-- current terms/types etc).
assertNoBranchAtPath' :: Path' -> Cli r ()
assertNoBranchAtPath' path0 = do
  path <- resolvePath' path0
  whenJustM (getMaybeBranchAt path) \branch ->
    when (not (Branch.isEmpty0 (Branch.head branch))) do
      Cli.returnEarly (BranchAlreadyExists path0)

-- | Get the branch0 at an absolute path.
getBranch0At :: Path.Absolute -> Cli r (Branch0 IO)
getBranch0At path =
  Branch.head <$> getBranchAt path

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
  getMaybePatchAt path & onNothingM (Cli.returnEarly (PatchNotFound path))

-- | Assert that there's no patch at a path, or return early if there is one.
assertNoPatchAt :: Path.Split' -> Cli r ()
assertNoPatchAt path = do
  whenJustM (getMaybePatchAt path) \_ -> Cli.returnEarly (PatchAlreadyExists path)
