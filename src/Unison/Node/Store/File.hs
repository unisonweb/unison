module Unison.Node.Store.File where

import Control.Applicative
import qualified Data.Set as S
import Data.Set (Set)
import Data.Aeson as J
import System.FilePath
import System.Directory
import Unison.Syntax.Hash
import Unison.Node.Store
import Unison.Note as N

{-
-- | Create a 'Store' rooted at the given path
store :: FilePath -> Store IO
store root =
  let
    hashesIn :: FilePath -> Set Hash
    hashesIn dir = N.lift $
      S.fromList . (map Hash) <$> getDirectoryContents (joinPath [root, dir])
    read :: FilePath -> Hash
    read dir h = undefined
    write dir h v = undefined

    hashes limit =
      let limitf = maybe id S.intersection limit
      in liftA2 S.union (hashesIn "terms") (hashesIn "types")

    readTerm = read "terms"
    writeTerm = write "terms"
    readType = read "types"
    writeType = write "types"
    readMetadata = read "metadata"
    writeMetadata = write "metadata"
  in Store hashes readTerm writeTerm readType writeType readMetadata writeMetadata
-}
