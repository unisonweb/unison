module Unison.Codebase.SqliteCodebase.Paths
  ( codebasePath,
    makeCodebasePath,
    makeCodebaseDirPath,
    backupCodebasePath,
  )
where

import Data.Time (NominalDiffTime)
import System.FilePath ((</>))
import Unison.Codebase (CodebasePath)

-- | Prefer makeCodebasePath or makeCodebaseDirPath when possible.
codebasePath :: FilePath
codebasePath = ".unison" </> "v2" </> "unison.sqlite3"

-- | Makes a path to a sqlite database from a codebase path.
makeCodebasePath :: CodebasePath -> FilePath
makeCodebasePath root = makeCodebaseDirPath root </> "unison.sqlite3"

-- | Makes a path to the location where sqlite files are stored within a codebase path.
makeCodebaseDirPath :: CodebasePath -> FilePath
makeCodebaseDirPath root = root </> ".unison" </> "v2"

-- | Makes a path to store a backup of a sqlite database given the current time.
backupCodebasePath :: NominalDiffTime -> FilePath
backupCodebasePath now =
  codebasePath ++ "." ++ show @Int (floor now)
