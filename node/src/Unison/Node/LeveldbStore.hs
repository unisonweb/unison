module Unison.Node.LeveldbStore where

import System.FilePath ((</>))
import Data.Aeson (ToJSON(..),FromJSON(..))
import Unison.Metadata (Metadata)
import Unison.Note (Noted,Note)
import Unison.Node.Store (Store, Store(..))
import Unison.Reference (Reference)
import qualified Database.LevelDB as DB

-- | Create a 'Store' rooted at the given path.
make :: (Ord v, ToJSON v, FromJSON v) => FilePath -> IO (Store IO v)
make root =
  let
    hashes = error "undefined"
    readTerm = error "undefined"
    writeTerm = error "undefined"
    typeOfTerm = error "undefined"
    annotateTerm = error "undefined"
    readMetadata = error "undefined"
    writeMetadata = error "undefined"
  in do
    {-- 
    Directory.createDirectoryIfMissing True (root </> "terms")
    Directory.createDirectoryIfMissing True (root </> "types")
    Directory.createDirectoryIfMissing True (root </> "type-of")
    Directory.createDirectoryIfMissing True (root </> "builtin-type-of")
    Directory.createDirectoryIfMissing True (root </> "metadata")
    Directory.createDirectoryIfMissing True (root </> "builtin-metadata")
--}
    pure $ Store hashes readTerm writeTerm typeOfTerm annotateTerm readMetadata writeMetadata
