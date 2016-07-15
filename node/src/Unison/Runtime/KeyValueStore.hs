{-# LANGUAGE OverloadedStrings #-}
module Unison.Runtime.KeyValueStore
  (Unison.Runtime.KeyValueStore.lookup
  ,Unison.Runtime.KeyValueStore.delete
  ,Unison.Runtime.KeyValueStore.insert
  ,Unison.Runtime.KeyValueStore.lookupGT
  ,empty
  ,load
  ) where

import Data.ByteString (ByteString)
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Runtime.JournaledMap as JM
import qualified Unison.BlockStore as BS

type KeyHash = ByteString
type Key = ByteString
type Value = ByteString

data Db = Db (JM.JournaledMap KeyHash (Key, Value))

empty :: BS.BlockStore Hash -> IO KeyHash -> IO Db
empty bs hashGen = do
  cpHash <- hashGen
  udHash <- hashGen
  let cp = BS.Series cpHash
      ud = BS.Series udHash
  Db <$> JM.fromSeries bs cp ud

-- if acidstore doesn't exist, one is created
load :: BS.BlockStore Hash -> IO KeyHash -> KeyHash -> IO Db
load bs hashGen rHash = do
  udHash <- hashGen
  let cp = BS.Series rHash
      ud = BS.Series udHash
  Db <$> JM.fromSeries bs cp ud

insert :: KeyHash -> (Key, Value) -> Db -> IO ()
insert kh kv (Db journaledMap) = JM.insert kh kv journaledMap

delete :: KeyHash -> Db -> IO ()
delete kh (Db journaledMap) = JM.delete kh journaledMap

lookup :: KeyHash -> Db -> IO (Maybe (Key, Value))
lookup kh (Db journaledMap) = JM.lookup kh journaledMap

-- | Find next key in the Db whose key is greater than the provided key
lookupGT :: KeyHash -> Db -> IO (Maybe (KeyHash, (Key, Value)))
lookupGT kh (Db journaledMap) = JM.lookupGT kh journaledMap
