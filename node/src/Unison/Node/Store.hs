module Unison.Node.Store where

import Control.Applicative
import Data.Aeson as J
import Data.Set (Set)
import System.Directory
import System.FilePath
import Unison.Hash (Hash)
import Unison.Metadata (Metadata)
import Unison.Note (Note, Noted)
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Unison.Hash as H
import qualified Unison.Note as N
import qualified Unison.Reference as R

data Store f = Store {
  hashes :: Maybe (Set Reference) -> Noted f (Set Reference), -- ^ The set of hashes in this store, optionally constrained to intersect the given set
  readTerm :: Hash -> Noted f Term,
  writeTerm :: Hash -> Term -> Noted f (),
  typeOfTerm :: Reference -> Noted f Type,
  annotateTerm :: Reference -> Type -> Noted f (),
  readMetadata :: Reference -> Noted f (Metadata Reference),
  writeMetadata :: Reference -> Metadata Reference -> Noted f ()
}

-- | Create a 'Store' rooted at the given path.
-- This creates directories "/terms", "/types", and "/metadata"
-- with a file named via the base64 encoding of each 'Hash'
store :: FilePath -> Store IO
store root =
  let
    hashesIn :: (String -> Reference) -> FilePath -> Noted IO (Set Reference)
    hashesIn f dir = N.lift $
      -- the `drop 2` strips out '.' and '..', gak
      S.fromList . (map (f . reverse . drop 5 . reverse) . drop 2) <$> -- strip out .json
        getDirectoryContents (joinPath [root, dir])

    n :: Either String a -> Either Note a
    n (Left e) = Left (N.note e)
    n (Right a) = Right a

    read :: FromJSON a => (h -> T.Text) -> FilePath -> h -> Noted IO a
    read f dir h =
      let file = joinPath [root, dir, T.unpack (f h) ++ ".json"]
      in N.noted $ (n . J.eitherDecodeStrict) <$> B.readFile file

    write :: ToJSON a => (h -> T.Text) -> FilePath -> h -> a -> Noted IO ()
    write f dir h v =
      let file = joinPath [root, dir, T.unpack (f h) ++ ".json"]
      in N.lift $ B.writeFile file (LB.toStrict (J.encode v))
      -- unfortunate that writeFile takes a strict bytestring

    read' :: FromJSON a => FilePath -> Hash -> Noted IO a
    read' = read H.base64

    write' :: ToJSON a => FilePath -> Hash -> a -> Noted IO ()
    write' = write H.base64

    hashes limit =
      let r = R.Derived . H.fromBase64 . T.pack
          limitf = maybe id S.intersection limit
          union a b c = a `S.union` b `S.union` c
      in liftA3 union (limitf <$> hashesIn r "terms")
                      (limitf <$> hashesIn r "types")
                      (limitf <$> hashesIn (R.Builtin . T.pack) "builtin-metadata")

    readTerm = read' "terms"
    writeTerm = write' "terms"

    typeOfTerm (R.Derived h) = read' "type-of" h
    typeOfTerm (R.Builtin b) = read id "builtin-type-of" b

    annotateTerm (R.Derived h) = write' "type-of" h
    annotateTerm (R.Builtin b) = write id "builtin-type-of" b

    readMetadata (R.Derived h) = read' "metadata" h
    readMetadata (R.Builtin b) = read id "builtin-metadata" b

    writeMetadata (R.Derived h) = write' "metadata" h
    writeMetadata (R.Builtin b) = write id "builtin-metadata" b

  in Store hashes readTerm writeTerm typeOfTerm annotateTerm readMetadata writeMetadata
