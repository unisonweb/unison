module Unison.Codebase.FileCodebase (codebaseExists) where

import System.FilePath ((</>))
import Unison.Codebase (CodebasePath)
import Unison.Prelude (MonadIO)
import UnliftIO.Directory (doesDirectoryExist)

-- checks if a minimal codebase structure exists at `path`
codebaseExists :: MonadIO m => CodebasePath -> m Bool
codebaseExists root =
  and <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

  where
    -- checks if `path` looks like a unison codebase
    minimalCodebaseStructure :: CodebasePath -> [FilePath]
    minimalCodebaseStructure root = [ branchHeadDir root ]

    branchesDir root = root </> codebasePath </> "paths"
    branchHeadDir root = branchesDir root </> "_head"

    codebasePath :: FilePath
    codebasePath = ".unison" </> "v1"
