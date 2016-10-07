module Unison.Codebase.LeveldbStore where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Set (Set)
import System.FilePath ((</>))
import Data.Aeson (ToJSON(..),FromJSON(..))
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Unison.Hash (Hash)
--import Unison.Metadata (Metadata)
import Unison.Note (Noted,Note)
import Unison.Node.Store (Store, Store(..))
import Unison.Reference (Reference)
import qualified Data.Aeson as Aeson
import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Iterator as IT
--import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Set as Set
import qualified Unison.Hash as Hash
import qualified Unison.Note as Note
import qualified Unison.Reference as Reference

-- | Create a 'Store' rooted at the given path.
make :: (MonadIO m, MonadMask m, Ord v, ToJSON v, FromJSON v) => FilePath -> m (Store m v)
make root =
  let
    dbOptions = DB.defaultOptions { DB.createIfMissing = True }

    hashesIn :: (MonadIO m, MonadMask m) =>
                (ByteString -> Reference) -> DB.DB -> Noted m (Set Reference)
    hashesIn f db =
      let keyToReference it = fmap (fmap f) (DB.iterKey it)
          referenceList l it = keyToReference it >>=
            maybe (pure l) (\i -> referenceList (i:l) it)
          referenceSet it = Set.fromList <$> referenceList [] it
          runSet = IT.withIter db DB.defaultReadOptions referenceSet
      in Note.lift runSet

    n :: Either String a -> Either Note a
    n (Left e) = Left (Note.note e)
    n (Right a) = Right a

    maybeToEither b Nothing = Left b
    maybeToEither _ (Just a) = Right a

    read :: (MonadIO m, FromJSON a) => (h -> ByteString) -> DB.DB -> h -> Noted m a
    read f db h = Note.noted $
      (n . (>>= Aeson.eitherDecodeStrict) . maybeToEither "knf")
      <$> DB.get db DB.defaultReadOptions (f h)

    write :: (MonadIO m, ToJSON a) => (h -> ByteString) -> DB.DB -> h -> a -> Noted m ()
    write f db h v = Note.lift $
      DB.put db DB.defaultWriteOptions (f h) (LazyByteString.toStrict (Aeson.encode v))

    read' :: (MonadIO m, FromJSON a) => DB.DB -> Hash -> Noted m a
    read' = read Hash.toBytes

    write' :: (MonadIO m, ToJSON a) => DB.DB -> Hash -> a -> Noted m ()
    write' = write Hash.toBytes

    hashes :: (MonadIO m, MonadMask m) =>
              DB.DB -> DB.DB -> Maybe (Set Reference) -> Noted m (Set Reference)
    hashes termsDB builtinMetadataDB limit =
      let limitf = maybe id Set.intersection limit
      in liftA2 Set.union
         (limitf <$> hashesIn (Reference.Derived . Hash.fromBytes) termsDB)
         (limitf <$> hashesIn (Reference.Builtin . decodeUtf8) builtinMetadataDB)

  in do
    termsDB <- DB.open (root </> "terms") dbOptions
    metadataDB <- DB.open (root </> "metadata") dbOptions
    builtinTypesDB <- DB.open (root </> "builtinTypes") dbOptions
    builtinMetadataDB <- DB.open (root </> "builtinMetadata") dbOptions

    pure $ Store
      (hashes termsDB builtinMetadataDB) -- hashes
      (read' termsDB)                    -- readTerm
      (write' termsDB)                   -- writeTerm
      (\r -> case r of                   -- typeOfTerm
          Reference.Derived h -> read' termsDB h
          Reference.Builtin b -> read encodeUtf8 builtinTypesDB b)
      (\r -> case r of                   -- annotateTerm
          Reference.Derived h -> write' termsDB h
          Reference.Builtin b -> write encodeUtf8 builtinTypesDB b)
      (\r -> case r of                   -- readMetadata
          Reference.Derived h -> read' metadataDB h
          Reference.Builtin b -> read encodeUtf8 builtinMetadataDB b)
      (\r -> case r of                   -- writeMetadata
          Reference.Derived h -> write' metadataDB h
          Reference.Builtin b -> write encodeUtf8 builtinMetadataDB b)
