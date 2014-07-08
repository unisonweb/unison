module Unison.Node.Store.File where

import Control.Applicative
import qualified Data.Set as S
import Data.Set (Set)
import Data.Aeson as J
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
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
      S.fromList . (map (H.fromBase64 . T.pack . reverse . drop 5 . reverse)) <$>
        getDirectoryContents (joinPath [root, dir])

    n :: Either String a -> Either Note a
    n (Left e) = Left (N.note e)
    n (Right a) = Right a

    read :: FromJSON a => FilePath -> Hash -> Noted IO a
    read dir h =
      let file = joinPath [root, dir, T.unpack (H.base64 h) ++ ".json"]
      in N.noted $ (n . J.eitherDecodeStrict) <$> B.readFile file

    write :: ToJSON a => FilePath -> Hash -> a -> Noted IO ()
    write dir h v =
      let file = joinPath [root, dir, T.unpack (H.base64 h) ++ ".json"]
      in N.lift $ B.writeFile file (LB.toStrict (J.encode v))
      -- unfortunate that writeFile takes a strict bytestring

    hashes limit =
      let limitf = maybe id S.intersection limit
      in liftA2 S.union (limitf <$> hashesIn "terms") (limitf <$> hashesIn "types")

    readTerm = read "terms"
    writeTerm = write "terms"
    readType = read "types"
    writeType = write "types"
    readMetadata = read "metadata"
    writeMetadata = write "metadata"
  in Store hashes readTerm writeTerm readType writeType readMetadata writeMetadata
