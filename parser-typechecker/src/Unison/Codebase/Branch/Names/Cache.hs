module Unison.Codebase.Branch.Names.Cache (BranchNames (..), expectNamesForBranch) where

import Data.Coerce (coerce)
import GHC.IO qualified as IO
import U.Codebase.HashTags (BranchHash (..), CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (NamespaceHash)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Branch.Type (Branch0)
import Unison.Codebase.Branch.Type qualified as Branch
import Unison.Hash
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Util.Cache qualified as Cache

data BranchNames = BranchNames
  { branchNames :: Names,
    branchNamesWithoutTransitiveLibs :: Names,
    branchPPED :: PPED.PrettyPrintEnvDecl,
    branchPPEDWithoutTransitiveLibs :: PPED.PrettyPrintEnvDecl
  }

type BranchNamesCache = Cache.Cache BranchHash BranchNames

-- | The number of branch names sets to cache in memory.
-- We don't want this too high since names and ppe's can use a lot of memory, but also
-- want to ensure we can store enough names to avoid re-computing them if the user is
-- switching back and forth between a couple projects/branches.
branchNamesCacheSize :: Word
branchNamesCacheSize = 4

globalNamesCache :: BranchNamesCache
globalNamesCache = IO.unsafePerformIO $ Cache.semispaceCache branchNamesCacheSize
{-# NOINLINE globalNamesCache #-}

-- | Given a branch hash, return the names for that branch, caching the result
-- for future fetches.
expectNamesForBranch :: Codebase IO v a -> CausalHash -> IO BranchNames
expectNamesForBranch codebase causalHash = do
  hashLen <- Codebase.runTransaction codebase Codebase.hashLength
  branch <- Codebase.expectBranchForHash codebase causalHash
  let namespaceHash = Branch.namespaceHash branch
  let branchHash = (coerce @(NamespaceHash IO) @BranchHash namespaceHash)
  Cache.lookup globalNamesCache branchHash >>= \case
    Just branchNames -> pure branchNames
    Nothing -> do
      let branchNames = buildNamesForBranch hashLen (Branch.head branch)
      Cache.insert globalNamesCache branchHash branchNames
      pure branchNames
  where
    buildNamesForBranch :: Int -> Branch0 m -> BranchNames
    buildNamesForBranch hashLen branch0 = do
      let withoutTransitiveLibs = Branch.withoutTransitiveLibs branch0
      let allNames = Branch.toNames $ branch0
      let withoutTransitiveLibsNames = Branch.toNames $ withoutTransitiveLibs
      let allNamesPPED = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames allNames)
      let withoutTransitiveLibsPPED = PPED.fromNamesDecl hashLen (NamesWithHistory.fromCurrentNames withoutTransitiveLibsNames)
      BranchNames
        { branchNames = allNames,
          branchNamesWithoutTransitiveLibs = withoutTransitiveLibsNames,
          branchPPED = allNamesPPED,
          branchPPEDWithoutTransitiveLibs = withoutTransitiveLibsPPED
        }
