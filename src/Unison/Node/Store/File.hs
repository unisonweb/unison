module Unison.Node.Store.File where

import Control.Applicative
import qualified Data.Set as S
import Data.Set (Set)
import Data.Aeson as J
import qualified Data.Text as T
import System.FilePath
import System.Directory
import Unison.Syntax.Hash as H
import Unison.Node.Store
import Unison.Note as N

-- | Create a 'Store' rooted at the given path.
-- This creates directories "/terms", "/types", and "/metadata"
-- with a file named via the base64 encoding of each 'Hash'
store :: FilePath -> Store IO
store root =
  let
    hashesIn :: FilePath -> Noted IO (Set Hash)
    hashesIn dir = N.lift $
      S.fromList . (map (H.fromBase64 . T.pack)) <$>
        getDirectoryContents (joinPath [root, dir])
    read :: FromJSON a => FilePath -> Hash -> Noted IO a
    read dir h = undefined

    write :: ToJSON a => FilePath -> Hash -> a -> Noted IO ()
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
