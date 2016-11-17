module Unison.Codebase.FileStore where

import Control.Applicative
import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.Set (Set)
import Data.Text (Text)
import System.FilePath ((</>))
import Unison.Hash (Hash)
import Unison.Note (Noted,Note)
import Unison.Codebase.Store (Store, Store(..))
import Unison.Reference (Reference)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Unison.Hash as Hash
import qualified Unison.Note as Note
import qualified Unison.Reference as Reference

-- | Create a 'Store' rooted at the given path.
make :: (Ord v, ToJSON v, FromJSON v) => FilePath -> IO (Store IO v)
make root =
  let
    hashesIn :: (String -> Reference) -> FilePath -> Noted IO (Set Reference)
    hashesIn f dir =
      let matchJSON path = case FilePath.splitExtension path of
            (name, ".json") -> Set.singleton (f name)
            _ -> Set.empty
      in Note.lift $ Set.unions . map matchJSON
                  <$> Directory.getDirectoryContents (root </> dir)

    n :: Either String a -> Either Note a
    n (Left e) = Left (Note.note e)
    n (Right a) = Right a

    read :: FromJSON a => (h -> Text) -> FilePath -> h -> Noted IO a
    read f dir h =
      let file = FilePath.joinPath [root, dir, Text.unpack (f h) ++ ".json"]
      in Note.noted $ (n . Aeson.eitherDecodeStrict) <$> ByteString.readFile file

    write :: ToJSON a => (h -> Text) -> FilePath -> h -> a -> Noted IO ()
    write f dir h v =
      let file = FilePath.joinPath [root, dir, Text.unpack (f h) ++ ".json"]
      in Note.lift $ ByteString.writeFile file (LazyByteString.toStrict (Aeson.encode v))
      -- unfortunate that writeFile takes a strict bytestring

    read' :: FromJSON a => FilePath -> Hash -> Noted IO a
    read' = read Hash.base64

    write' :: ToJSON a => FilePath -> Hash -> a -> Noted IO ()
    write' = write Hash.base64

    hashes limit =
      let r = Reference.Derived . Hash.fromBase64 . Text.pack
          limitf = maybe id Set.intersection limit
          union a b c = a `Set.union` b `Set.union` c
      in liftA3 union (limitf <$> hashesIn r "terms")
                      (limitf <$> hashesIn r "types")
                      (limitf <$> hashesIn (Reference.Builtin . Text.pack) "builtin-metadata")

    readTerm = read' "terms"
    writeTerm = write' "terms"

    -- replace slashes with dashes
    mangle = Text.map unslash where
      unslash '/' = '-'
      unslash ch = ch

    typeOfTerm (Reference.Derived h) = read' "type-of" h
    typeOfTerm (Reference.Builtin b) = read id "builtin-type-of" (mangle b)

    annotateTerm (Reference.Derived h) = write' "type-of" h
    annotateTerm (Reference.Builtin b) = write id "builtin-type-of" (mangle b)

    readMetadata (Reference.Derived h) = read' "metadata" h
    readMetadata (Reference.Builtin b) = read id "builtin-metadata" (mangle b)

    writeMetadata (Reference.Derived h) = write' "metadata" h
    writeMetadata (Reference.Builtin b) = write id "builtin-metadata" (mangle b)

  in do
    Directory.createDirectoryIfMissing True (root </> "terms")
    Directory.createDirectoryIfMissing True (root </> "types")
    Directory.createDirectoryIfMissing True (root </> "type-of")
    Directory.createDirectoryIfMissing True (root </> "builtin-type-of")
    Directory.createDirectoryIfMissing True (root </> "metadata")
    Directory.createDirectoryIfMissing True (root </> "builtin-metadata")
    pure $ Store hashes readTerm writeTerm typeOfTerm annotateTerm readMetadata writeMetadata
