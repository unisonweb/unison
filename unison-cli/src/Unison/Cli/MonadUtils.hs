-- | This module contains miscellaneous helper utils for rote actions in the Cli monad, like resolving a relative path
-- to an absolute path, per the current path.
module Unison.Cli.MonadUtils
  ( -- * @.unisonConfig@ things
    getConfig,

    -- * Paths
    getCurrentPath,
    getCurrentProjectName,
    getCurrentProjectBranchName,
    getCurrentProjectPath,
    resolvePath,
    resolvePath',
    resolveSplit',

    -- * Branches

    -- ** Resolving branch identifiers
    resolveAbsBranchId,
    resolveAbsBranchIdV2,
    resolveBranchId,
    resolveBranchIdToAbsBranchId,
    resolveShortCausalHash,

    -- ** Getting/setting branches
    setCurrentProjectRoot,
    modifyProjectRoot,
    getProjectRoot,
    getProjectRoot0,
    getCurrentBranch,
    getCurrentBranch0,
    getBranchFromProjectPath,
    getBranch0FromProjectPath,
    getMaybeBranchFromProjectPath,
    getMaybeBranch0FromProjectPath,
    expectBranchAtPath,
    expectBranchAtPath',
    expectBranch0AtPath,
    expectBranch0AtPath',
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
    updateProjectBranchRoot,
    updateAtM,
    updateAt,
    updateAndStepAt,

    -- * Terms
    getTermsAt,

    -- * Types
    getTypesAt,

    -- * Patches

    -- ** Default patch
    defaultPatchPath,

    -- ** Getting patches
    getPatchAt,
    getMaybePatchAt,
    expectPatchAt,
    assertNoPatchAt,

    -- * Latest touched Unison file
    getLatestFile,
    getLatestParsedFile,
    getNamesFromLatestFile,
    getTermFromLatestParsedFile,
    expectLatestFile,
    expectLatestParsedFile,
    getLatestTypecheckedFile,
    expectLatestTypecheckedFile,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State
import Data.Configurator qualified as Configurator
import Data.Configurator.Types qualified as Configurator
import Data.Foldable
import Data.Set qualified as Set
import U.Codebase.Branch qualified as V2 (Branch)
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash (..))
import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch (..), Branch0 (..))
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Patch qualified as Patch
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath, ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Term qualified as Term
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UFN
import Unison.Util.Set qualified as Set
import Unison.Var qualified as Var
import UnliftIO.STM

------------------------------------------------------------------------------------------------------------------------
-- .unisonConfig things

-- | Lookup a config value by key.
getConfig :: (Configurator.Configured a) => Text -> Cli (Maybe a)
getConfig key = do
  Cli.Env {config} <- ask
  liftIO (Configurator.lookup config key)

------------------------------------------------------------------------------------------------------------------------
-- Getting paths, path resolution, etc.

getCurrentProjectPath :: Cli PP.ProjectPath
getCurrentProjectPath = do
  (PP.ProjectPath projId branchId path) <- Cli.getProjectPathIds
  -- TODO: Reset to a valid project on error.
  (proj, branch) <- fmap (fromMaybe (error $ reportBug "E794202" ("Project branch not found in database for ids: " <> show (projId, branchId)))) . Cli.runTransaction . runMaybeT $ do
    project <- MaybeT $ Q.loadProject projId
    branch <- MaybeT $ Q.loadProjectBranch projId branchId
    pure (project, branch)
  pure (PP.ProjectPath proj branch path)

-- | Get the current path relative to the current project.
getCurrentPath :: Cli Path.Absolute
getCurrentPath = do
  view PP.absPath_ <$> getCurrentProjectPath

getCurrentProjectName :: Cli ProjectName
getCurrentProjectName = do
  view (#project . #name) <$> getCurrentProjectPath

getCurrentProjectBranchName :: Cli ProjectBranchName
getCurrentProjectBranchName = do
  view (#branch . #name) <$> getCurrentProjectPath

-- | Resolve a @Path@ (interpreted as relative) to a @Path.Absolute@, per the current path.
resolvePath :: Path -> Cli PP.ProjectPath
resolvePath path = do
  pp <- getCurrentProjectPath
  pure $ pp & PP.absPath_ %~ \p -> Path.resolve p path

-- | Resolve a @Path'@ to a @Path.Absolute@, per the current path.
resolvePath' :: Path' -> Cli PP.ProjectPath
resolvePath' path' = do
  pp <- getCurrentProjectPath
  pure $ pp & PP.absPath_ %~ \p -> Path.resolve p path'

-- | Resolve a path split, per the current path.
resolveSplit' :: (Path', a) -> Cli (PP.ProjectPath, a)
resolveSplit' =
  traverseOf _1 resolvePath'

------------------------------------------------------------------------------------------------------------------------
-- Branch resolution

-- | Resolve an @AbsBranchId@ to the corresponding @Branch IO@, or fail if no such branch hash is found. (Non-existent
-- branches by path are OK - the empty branch will be returned).
resolveAbsBranchId :: Input.AbsBranchId -> Cli (Branch IO)
resolveAbsBranchId = \case
  Left hash -> resolveShortCausalHash hash
  Right absPath -> do
    pp <- resolvePath' (Path' (Left absPath))
    getBranchFromProjectPath pp

-- | V2 version of 'resolveAbsBranchId2'.
resolveAbsBranchIdV2 ::
  (forall void. Output.Output -> Sqlite.Transaction void) ->
  ProjectAndBranch Project ProjectBranch ->
  Input.AbsBranchId ->
  Sqlite.Transaction (V2.Branch Sqlite.Transaction)
resolveAbsBranchIdV2 rollback (ProjectAndBranch proj branch) = \case
  Left shortHash -> do
    hash <- resolveShortCausalHashToCausalHash rollback shortHash
    causal <- (Codebase.expectCausalBranchByCausalHash hash)
    V2Causal.value causal
  Right absPath -> do
    let pp = PP.ProjectPath proj branch absPath
    Codebase.getShallowBranchAtProjectPath pp

-- | Resolve a @BranchId@ to the corresponding @Branch IO@, or fail if no such branch hash is found. (Non-existent
-- branches by path are OK - the empty branch will be returned).
resolveBranchId :: Input.BranchId -> Cli (Branch IO)
resolveBranchId branchId = do
  absBranchId <- resolveBranchIdToAbsBranchId branchId
  resolveAbsBranchId absBranchId

-- | Resolve a @BranchId@ to an @AbsBranchId@.
resolveBranchIdToAbsBranchId :: Input.BranchId -> Cli Input.AbsBranchId
resolveBranchIdToAbsBranchId =
  traverseOf _Right (fmap (view PP.absPath_) . resolvePath')

-- | Resolve a @ShortCausalHash@ to the corresponding @Branch IO@, or fail if no such branch hash is found.
resolveShortCausalHash :: ShortCausalHash -> Cli (Branch IO)
resolveShortCausalHash shortHash = do
  Cli.time "resolveShortCausalHash" do
    Cli.Env {codebase} <- ask
    hash <- Cli.runTransactionWithRollback \rollback -> resolveShortCausalHashToCausalHash rollback shortHash
    branch <- liftIO (Codebase.getBranchForHash codebase hash)
    pure (fromMaybe Branch.empty branch)

resolveShortCausalHashToCausalHash ::
  (forall void. Output.Output -> Sqlite.Transaction void) ->
  ShortCausalHash ->
  Sqlite.Transaction CausalHash
resolveShortCausalHashToCausalHash rollback shortHash = do
  hashes <- Codebase.causalHashesByPrefix shortHash
  Set.asSingleton hashes & onNothing do
    if Set.null hashes
      then rollback (Output.NoBranchWithHash shortHash)
      else do
        len <- Codebase.branchHashLength
        rollback (Output.BranchHashAmbiguous shortHash (Set.map (SCH.fromHash len) hashes))

------------------------------------------------------------------------------------------------------------------------
-- Getting/Setting branches

-- | Get the root branch.
getProjectRoot :: Cli (Branch IO)
getProjectRoot = do
  use #currentProjectRoot >>= atomically . readTMVar

-- | Get the root branch0.
getProjectRoot0 :: Cli (Branch0 IO)
getProjectRoot0 =
  Branch.head <$> getProjectRoot

-- | Set a new root branch.
--
-- Note: This does _not_ update the codebase, the caller is responsible for that.
setCurrentProjectRoot :: Branch IO -> Cli ()
setCurrentProjectRoot b = do
  void $ modifyProjectRoot (const b)

-- | Modify the root branch.
--
-- Note: This does _not_ update the codebase, the caller is responsible for that.
modifyProjectRoot :: (Branch IO -> Branch IO) -> Cli (Branch IO)
modifyProjectRoot f = do
  rootVar <- use #currentProjectRoot
  atomically do
    root <- takeTMVar rootVar
    let !newRoot = f root
    putTMVar rootVar newRoot
    pure newRoot

-- | Get the current branch.
getCurrentBranch :: Cli (Branch IO)
getCurrentBranch = do
  path <- getCurrentPath
  Cli.Env {codebase} <- ask
  liftIO $ Codebase.getBranchAtPath codebase path

-- | Get the current branch0.
getCurrentBranch0 :: Cli (Branch0 IO)
getCurrentBranch0 = do
  Branch.head <$> getCurrentBranch

-- | Get the branch at an absolute path from the project root.
getBranchFromProjectPath :: PP.ProjectPath -> Cli (Branch IO)
getBranchFromProjectPath pp =
  getMaybeBranchFromProjectPath pp <&> fromMaybe Branch.empty

-- | Get the branch0 at an absolute path.
getBranch0FromProjectPath :: PP.ProjectPath -> Cli (Branch0 IO)
getBranch0FromProjectPath pp =
  Branch.head <$> getBranchFromProjectPath pp

-- | Get the maybe-branch at an absolute path.
getMaybeBranchFromProjectPath :: PP.ProjectPath -> Cli (Maybe (Branch IO))
getMaybeBranchFromProjectPath pp = do
  Cli.Env {codebase} <- ask
  let ProjectBranch {causalHashId} = pp ^. #branch
  causalHash <- Cli.runTransaction $ Q.expectCausalHash causalHashId
  rootBranch <- liftIO $ Codebase.expectBranchForHash codebase causalHash
  pure (Branch.getAt (pp ^. PP.path_) rootBranch)

-- | Get the maybe-branch0 at an absolute path.
getMaybeBranch0FromProjectPath :: PP.ProjectPath -> Cli (Maybe (Branch0 IO))
getMaybeBranch0FromProjectPath pp =
  fmap Branch.head <$> getMaybeBranchFromProjectPath pp

-- | Get the branch at a relative path, or return early if there's no such branch.
expectBranchAtPath :: Path -> Cli (Branch IO)
expectBranchAtPath =
  expectBranchAtPath' . Path' . Right . Path.Relative

-- | Get the branch at an absolute or relative path, or return early if there's no such branch.
expectBranchAtPath' :: Path' -> Cli (Branch IO)
expectBranchAtPath' path0 = do
  path <- resolvePath' path0
  getMaybeBranchFromProjectPath path & onNothingM (Cli.returnEarly (Output.BranchNotFound path0))

-- | Get the branch0 at an absolute or relative path, or return early if there's no such branch.
expectBranch0AtPath' :: Path' -> Cli (Branch0 IO)
expectBranch0AtPath' =
  fmap Branch.head . expectBranchAtPath'

-- | Get the branch0 at a relative path, or return early if there's no such branch.
expectBranch0AtPath :: Path -> Cli (Branch0 IO)
expectBranch0AtPath =
  expectBranch0AtPath' . Path' . Right . Path.Relative

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
  pp <- resolvePath' path'
  Cli.runTransaction do
    branch <- Codebase.getShallowBranchAtProjectPath pp
    isEmpty <- V2Branch.isEmpty branch
    pure (not isEmpty)

------------------------------------------------------------------------------------------------------------------------
-- Updating branches

relativizeActions :: (Foldable f) => f (Path.Absolute, x) -> [(Path, x)]
relativizeActions actions =
  toList actions
    & traversed . _1 %~ Path.unabsolute

stepAt ::
  Text ->
  (Path.Absolute, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepAt cause = stepManyAt @[] cause . pure

stepAt' ::
  Text ->
  (Path.Absolute, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepAt' cause = stepManyAt' @[] cause . pure

stepAtNoSync' ::
  (Path.Absolute, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepAtNoSync' = stepManyAtNoSync' @[] . pure

stepAtNoSync ::
  (Path.Absolute, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepAtNoSync = stepManyAtNoSync @[] . pure

stepAtM ::
  Text ->
  (Path.Absolute, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepAtM cause = stepManyAtM @[] cause . pure

stepManyAt ::
  (Foldable f) =>
  Text ->
  f (Path.Absolute, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepManyAt reason actions = do
  stepManyAtNoSync actions
  syncRoot reason

stepManyAt' ::
  (Foldable f) =>
  Text ->
  f (Path.Absolute, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepManyAt' reason actions = do
  res <- stepManyAtNoSync' actions
  syncRoot reason
  pure res

stepManyAtNoSync' ::
  (Foldable f) =>
  f (Path.Absolute, Branch0 IO -> Cli (Branch0 IO)) ->
  Cli Bool
stepManyAtNoSync' actions = do
  origRoot <- getProjectRoot
  newRoot <- Branch.stepManyAtM (relativizeActions actions) origRoot
  setCurrentProjectRoot newRoot
  pure (origRoot /= newRoot)

-- Like stepManyAt, but doesn't update the last saved root
stepManyAtNoSync ::
  (Foldable f) =>
  f (Path.Absolute, Branch0 IO -> Branch0 IO) ->
  Cli ()
stepManyAtNoSync actions = do
  void . modifyProjectRoot $ Branch.stepManyAt (relativizeActions actions)

stepManyAtM ::
  (Foldable f) =>
  Text ->
  f (Path.Absolute, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepManyAtM reason actions = do
  stepManyAtMNoSync actions
  syncRoot reason

stepManyAtMNoSync ::
  (Foldable f) =>
  f (Path.Absolute, Branch0 IO -> IO (Branch0 IO)) ->
  Cli ()
stepManyAtMNoSync actions = do
  oldRoot <- getProjectRoot
  newRoot <- liftIO (Branch.stepManyAtM (relativizeActions actions) oldRoot)
  setCurrentProjectRoot newRoot

-- | Sync the in-memory root branch.
syncRoot :: Text -> Cli ()
syncRoot description = do
  rootBranch <- getProjectRoot
  updateCurrentProjectRoot rootBranch description

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAtM ::
  Text ->
  ProjectPath ->
  (Branch IO -> Cli (Branch IO)) ->
  Cli Bool
updateAtM reason pp f = do
  b <- getBranchFromProjectPath (pp & PP.absPath_ .~ Path.absoluteEmpty)
  b' <- Branch.modifyAtM (pp ^. PP.path_) f b
  updateCurrentProjectRoot b' reason
  pure $ b /= b'

-- | Update a branch at the given path, returning `True` if
-- an update occurred and false otherwise
updateAt ::
  Text ->
  ProjectPath ->
  (Branch IO -> Branch IO) ->
  Cli Bool
updateAt reason p f = do
  updateAtM reason p (pure . f)

updateAndStepAt ::
  (Foldable f, Foldable g) =>
  Text ->
  f (Path.Absolute, Branch IO -> Branch IO) ->
  g (Path, Branch0 IO -> Branch0 IO) ->
  Cli ()
updateAndStepAt reason updates steps = do
  root <-
    (Branch.stepManyAt steps)
      . (\root -> foldl' (\b (Path.Absolute p, f) -> Branch.modifyAt p f b) root updates)
      <$> getProjectRoot
  ProjectPath _ projBranch _ <- getCurrentProjectPath
  updateProjectBranchRoot projBranch root reason

updateProjectBranchRoot :: ProjectBranch -> Branch IO -> Text -> Cli ()
updateProjectBranchRoot projectBranch new reason =
  Cli.time "updateCurrentProjectRoot" do
    runTransaction $ Q.setProjectBranchHead (projectBranch ^. #branchId) (Branch.headHash new)
    setCurrentProjectRoot new

------------------------------------------------------------------------------------------------------------------------
-- Getting terms

getTermsAt :: (PP.ProjectPath, HQ'.HQSegment) -> Cli (Set Referent)
getTermsAt (pp, hqSeg) = do
  rootBranch0 <- getBranch0FromProjectPath pp
  pure (BranchUtil.getTerm (mempty, hqSeg) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting types

getTypesAt :: (PP.ProjectPath, HQ'.HQSegment) -> Cli (Set TypeReference)
getTypesAt (pp, hqSeg) = do
  rootBranch0 <- getBranch0FromProjectPath pp
  pure (BranchUtil.getType (mempty, hqSeg) rootBranch0)

------------------------------------------------------------------------------------------------------------------------
-- Getting patches

-- | The default patch path.
defaultPatchPath :: Path.Split'
defaultPatchPath =
  (Path.RelativePath' (Path.Relative Path.empty), NameSegment.defaultPatchSegment)

-- | Get the patch at a path, or the empty patch if there's no such patch.
getPatchAt :: Path.Split' -> Cli Patch
getPatchAt path =
  getMaybePatchAt path <&> fromMaybe Patch.empty

-- | Get the patch at a path.
getMaybePatchAt :: Path.Split' -> Cli (Maybe Patch)
getMaybePatchAt path0 = do
  (pp, name) <- resolveSplit' path0
  branch <- getBranch0FromProjectPath pp
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
  oe <- use #latestTypecheckedFile
  pure $ case oe of
    Just (Right tf) -> Just tf
    _ -> Nothing

-- | Get the latest parsed unison file.
getLatestParsedFile :: Cli (Maybe (UnisonFile Symbol Ann))
getLatestParsedFile = do
  oe <- use #latestTypecheckedFile
  pure $ case oe of
    Just (Left uf) -> Just uf
    Just (Right tf) -> Just $ UF.discardTypes tf
    _ -> Nothing

expectLatestParsedFile :: Cli (UnisonFile Symbol Ann)
expectLatestParsedFile =
  getLatestParsedFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)

-- | Returns a parsed term (potentially with free variables) from the latest file.
-- This term will refer to other terms in the file by vars, not by hash.
-- Used to implement rewriting and other refactorings on the current file.
getTermFromLatestParsedFile :: HQ.HashQualified Name.Name -> Cli (Maybe (Term.Term Symbol Ann))
getTermFromLatestParsedFile (HQ.NameOnly n) = do
  uf <- getLatestParsedFile
  pure $ case uf of
    Nothing -> Nothing
    Just uf ->
      case UF.typecheckingTerm uf of
        Term.LetRecNamed' bs _ -> lookup (Var.named (Name.toText n)) bs
        _ -> Nothing
getTermFromLatestParsedFile _ = pure Nothing

-- | Gets the names from the latest typechecked unison file, or latest parsed file if it
-- didn't typecheck.
getNamesFromLatestFile :: Cli Names
getNamesFromLatestFile = do
  use #latestTypecheckedFile <&> \case
    Just (Right tf) -> UFN.typecheckedToNames tf
    Just (Left uf) -> UFN.toNames uf
    Nothing -> mempty

-- | Get the latest typechecked unison file, or return early if there isn't one.
expectLatestTypecheckedFile :: Cli (TypecheckedUnisonFile Symbol Ann)
expectLatestTypecheckedFile =
  getLatestTypecheckedFile & onNothingM (Cli.returnEarly Output.NoUnisonFile)
