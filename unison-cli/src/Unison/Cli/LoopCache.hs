module Unison.Cli.LoopCache
  ( newLoopCacheVar,
    getLoopCache,
    LoopCache (..),
    LoopCacheVar,
  )
where

import U.Codebase.Sqlite.DbId (ProjectBranchId)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project.Util (ProjectContext (..), projectRootPathFromContext)
import UnliftIO.STM (TVar)
import UnliftIO.STM qualified as STM

-- | Cache which is keyed to a specific project branch.
newtype LoopCacheVar = LoopCacheVar (TVar (Maybe (Maybe ProjectBranchId, LoopCache)))

newLoopCacheVar :: IO LoopCacheVar
newLoopCacheVar = do
  var <- STM.newTVarIO Nothing
  pure $ LoopCacheVar var

-- | State we can cache while within a specific project branch.
data LoopCache = LoopCache
  { projectBranchAllNames :: Names,
    projectBranchNamesWithoutTransitiveLibs :: Names,
    projectBranchNamesWithoutAnyLibs :: Names,
    projectBranchPPEDAllNames :: PPED.PrettyPrintEnvDecl,
    projectBranchPPEDNamesWithoutTransitiveLibs :: PPED.PrettyPrintEnvDecl
  }

recomputeLoopCache :: Codebase IO v a -> ProjectContext -> LoopCacheVar -> IO (LoopCache)
recomputeLoopCache codebase pc (LoopCacheVar var) = do
  hashLength <- Codebase.runTransaction codebase $ Codebase.hashLength
  let pathToProjectRoot = projectRootPathFromContext pc
  projectRootBranch <- Codebase.getBranchAtPath codebase pathToProjectRoot
  let newLoopCache = buildLoopCache hashLength (Branch.head projectRootBranch)
  STM.atomically $ STM.writeTVar var (Just $ (currentBranchId, newLoopCache))
  pure newLoopCache
  where
    currentBranchId = case pc of
      LooseCodePath _ -> Nothing
      ProjectBranchPath _projId projBranchId _path -> Just projBranchId

buildLoopCache :: Int -> Branch0 m -> LoopCache
buildLoopCache hashLength branch = do
  let withoutTransitiveLibs = Branch.withoutTransitiveLibs branch
  let withoutAnyLibs = Branch.withoutLib branch
  let allNames = Branch.toNames $ branch
  let withoutTransitiveLibsNames = Branch.toNames $ withoutTransitiveLibs
  let withoutAnyLibsNames = Branch.toNames $ withoutAnyLibs
  let allNamesPPED = PPED.fromNamesDecl hashLength (NamesWithHistory.fromCurrentNames allNames)
  let withoutTransitiveLibsPPED = PPED.fromNamesDecl hashLength (NamesWithHistory.fromCurrentNames withoutTransitiveLibsNames)
  LoopCache
    { projectBranchAllNames = allNames,
      projectBranchNamesWithoutTransitiveLibs = withoutTransitiveLibsNames,
      projectBranchNamesWithoutAnyLibs = withoutAnyLibsNames,
      projectBranchPPEDAllNames = allNamesPPED,
      projectBranchPPEDNamesWithoutTransitiveLibs = withoutTransitiveLibsPPED
    }

getLoopCache :: Codebase IO v a -> ProjectContext -> LoopCacheVar -> IO LoopCache
getLoopCache codebase pc (LoopCacheVar var) = do
  action <- STM.atomically $ do
    STM.readTVar var >>= \case
      Nothing -> pure $ recomputeLoopCache codebase pc (LoopCacheVar var)
      Just (prevBranchId, loopCache)
        | prevBranchId == currentBranchId -> pure (pure loopCache)
        | otherwise -> pure $ recomputeLoopCache codebase pc (LoopCacheVar var)
  action
  where
    currentBranchId = case pc of
      LooseCodePath _ -> Nothing
      ProjectBranchPath _projId projBranchId _path -> Just projBranchId
