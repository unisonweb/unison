{-# LANGUAGE OverloadedStrings #-}
module Unison.Runtime.KeyValueStore
  (Unison.Runtime.KeyValueStore.lookup
  ,Unison.Runtime.KeyValueStore.delete
  ,Unison.Runtime.KeyValueStore.insert
  ,Unison.Runtime.KeyValueStore.lookupGT
  ,load
  ,dump
  ) where

import Data.ByteString.Char8 (unpack)
import Data.ByteString (ByteString)
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Runtime.JournaledMap as JM
import qualified Unison.BlockStore as BS
import qualified Unison.Runtime.Block as B

type KeyHash = ByteString
type Key = ByteString
type Value = ByteString
type Address = (BS.Series, BS.Series)

data Db = Db
  { journaledMap :: JM.JournaledMap KeyHash (Key, Value)
  , address :: Address
  }

load :: BS.BlockStore Hash -> Address -> IO Db
load bs (cp, ud) = do
  jm <- JM.fromSeries bs cp ud
  pure $ Db jm (cp, ud)

insert :: KeyHash -> (Key, Value) -> Db -> IO ()
insert kh kv (Db journaledMap _) = JM.insert kh kv journaledMap

delete :: KeyHash -> Db -> IO ()
delete kh (Db journaledMap _) = JM.delete kh journaledMap

lookup :: KeyHash -> Db -> IO (Maybe (Key, Value))
lookup kh (Db journaledMap _) = JM.lookup kh journaledMap

-- | Find next key in the Db whose key is greater than the provided key
lookupGT :: KeyHash -> Db -> IO (Maybe (KeyHash, (Key, Value)))
lookupGT kh (Db journaledMap _) = JM.lookupGT kh journaledMap
