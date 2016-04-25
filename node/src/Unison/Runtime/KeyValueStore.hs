{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Unison.Runtime.KeyValueStore where

import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Acid.Advanced
import Data.ByteString (ByteString)
import Data.SafeCopy
import Data.Typeable
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import System.Random
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Unison.Hash as Hash

data KeyValue = KeyValue !(Map.Map ByteString (Bool, ByteString))
    deriving (Typeable)
$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: ByteString -> ByteString -> Update KeyValue ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key (True, value) m))

deleteKey :: ByteString -> Update KeyValue ()
deleteKey key = do
  KeyValue m <- get
  put (KeyValue (Map.insert key (False, key) m))

lookupKey :: ByteString -> Query KeyValue (Maybe (Bool, ByteString))
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)

lookupGT_ :: ByteString -> Query KeyValue (Maybe (ByteString, (Bool, ByteString)))
lookupGT_ key = do
  KeyValue m <- ask
  let pairs = iterate (\mkv -> mkv >>= ((`Map.lookupGT` m) . fst))
        $ Just (key, undefined)
      hasKey (Just (k, (True, v))) = True
      hasKey _ = False
      notDeleted = dropWhile (not . hasKey) $ tail pairs
  pure $ head notDeleted

$(makeAcidic ''KeyValue ['insertKey, 'deleteKey, 'lookupKey, 'lookupGT_ ])

data Db = Db { acidState :: AcidState KeyValue, uid :: Hash }

empty :: IO Db
empty = do
  g <- getStdGen
  let (rHash, newGen) = random g :: (Hash, StdGen)
      fName = Text.unpack $ Hash.base64 rHash
  acidState <- openLocalStateFrom fName $ KeyValue Map.empty
  pure $ Db acidState rHash

-- if acidstore doesn't exist, one is created
load :: Hash -> IO Db
load rHash = do
  let fName = Text.unpack $ Hash.base64 rHash
  acidState <- openLocalStateFrom fName $ KeyValue Map.empty
  pure $ Db acidState rHash

close :: Db -> IO ()
close db = closeAcidState $ acidState db

insert :: ByteString -> ByteString -> Db -> IO ()
insert k v (Db acidState uid) = update acidState $ InsertKey k v

-- TODO garbage collect deleted keys
delete :: ByteString -> Db -> IO ()
delete k (Db acidState uid) = update acidState $ DeleteKey k

lookup :: ByteString -> Db -> IO (Maybe ByteString)
lookup k (Db acidState uid) = do
  result <- query acidState $ LookupKey k
  pure $ case result of
    Just (True, v) -> pure v
    _ -> Nothing

-- | Find next key in the Db whose key is greater than the provided key
lookupGT :: ByteString -> Db -> IO (Maybe (ByteString, ByteString))
lookupGT k (Db acidState uid) = do
  result <- query acidState $ LookupGT_ k
  pure $ case result of
    Just (k, (True, v)) -> pure (k, v)
    _ -> Nothing
