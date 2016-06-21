{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Unison.Runtime.KeyValueStore
  (Unison.Runtime.KeyValueStore.lookup
  ,cleanup
  ,close
  ,delete
  ,empty
  ,insert
  ,load
  ,lookupGT
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.SafeCopy
import Data.Typeable
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import System.FilePath ((</>))
import System.Random
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified System.Directory as Directory
import qualified Unison.Hash as Hash

cleanupThreshold :: Int
cleanupThreshold = 1000

type Key = ByteString
type KeyHash = ByteString
type Value = ByteString

data KeyValue = KeyValue
  { store :: !(Map.Map KeyHash (Key, Value))
  , writeCount :: !Int
  } deriving (Typeable)
$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: KeyHash ->  (Key, Value) -> Update KeyValue ()
insertKey keyHash (key, value)
    = do KeyValue store wc <- get
         put (KeyValue (Map.insert keyHash (key, value) store) (wc + 1))

deleteKey :: KeyHash -> Update KeyValue ()
deleteKey keyHash = do
  KeyValue store wc <- get
  let newStore = Map.delete keyHash store
  put (KeyValue newStore (wc + 1))

lookupKey :: KeyHash -> Query KeyValue (Maybe (Key, Value))
lookupKey key
    = do KeyValue store dc <- ask
         return (Map.lookup key store)

lookupGT_ :: KeyHash -> Query KeyValue (Maybe (KeyHash, (Key, Value)))
lookupGT_ keyHash = do
  KeyValue store dc <- ask
  pure $ Map.lookupGT keyHash store

getWriteCount :: Query KeyValue Int
getWriteCount = do
  KeyValue store dc <- ask
  pure dc

resetWriteCount :: Update KeyValue ()
resetWriteCount = do
  KeyValue store wc <- get
  put (KeyValue store 0)

$(makeAcidic ''KeyValue ['insertKey, 'deleteKey, 'lookupKey, 'lookupGT_, 'getWriteCount, 'resetWriteCount])

data Db = Db { acidState :: AcidState KeyValue, uid :: Hash }

empty :: IO Db
empty = do
  g <- getStdGen
  let (rHash, _) = random g :: (Hash, StdGen)
      fName = Text.unpack $ Hash.base64 rHash
  acidState <- openLocalStateFrom fName $ KeyValue Map.empty 0
  pure $ Db acidState rHash

-- if acidstore doesn't exist, one is created
load :: Hash -> IO Db
load rHash = do
  let fName = Text.unpack $ Hash.base64 rHash
  acidState <- openLocalStateFrom fName $ KeyValue Map.empty 0
  pure $ Db acidState rHash

close :: Db -> IO ()
close db = closeAcidState $ acidState db

cleanup :: Db -> IO ()
cleanup db = do
  close db
  Directory.removeDirectoryRecursive . Text.unpack . Hash.base64 . uid $ db

maybeCollectGarbage :: Db -> IO ()
maybeCollectGarbage (Db acidState uid) = do
  wc <- query acidState GetWriteCount
  case wc of
    wc | wc >= cleanupThreshold -> do
      let storeDirectory = Text.unpack . Hash.base64 $ uid
      hasArchive <- Directory.doesDirectoryExist $ storeDirectory </> "Archive"
      if hasArchive then Directory.removeDirectoryRecursive $ storeDirectory </> "Archive"
        else pure ()
      createArchive acidState
      createCheckpoint acidState
      update acidState ResetWriteCount
    _ -> pure ()

insert :: KeyHash -> (Key, Value) -> Db -> IO ()
insert kh kv db@(Db acidState _) = do
  update acidState $ InsertKey kh kv
  maybeCollectGarbage db

delete :: KeyHash -> Db -> IO ()
delete kh db@(Db acidState uid) = do
  update acidState $ DeleteKey kh
  maybeCollectGarbage db

lookup :: KeyHash -> Db -> IO (Maybe Value)
lookup kh (Db acidState _) = do
  result <- query acidState $ LookupKey kh
  pure $ case result of
    Nothing -> Nothing
    Just (k, v) -> Just v

-- | Find next key in the Db whose key is greater than the provided key
lookupGT :: KeyHash -> Db -> IO (Maybe (KeyHash, (Key, Value)))
lookupGT kh (Db acidState _) = query acidState $ LookupGT_ kh
