{-# LANGUAGE OverloadedStrings #-}
module Unison.Runtime.KeyValueStore
  (Unison.Runtime.KeyValueStore.lookup
  ,Unison.Runtime.KeyValueStore.delete
  ,Unison.Runtime.KeyValueStore.insert
  ,Unison.Runtime.KeyValueStore.lookupGT
  ,load
  ,idToText
  ,textToId
  ) where

import Data.ByteString (ByteString, append)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Unison.Runtime.Address
import Unison.Runtime.JournaledMap as JM
import qualified Unison.BlockStore as BS
import qualified Data.ByteString.Base64.URL as Base64

type KeyHash = ByteString
type Key = ByteString
type Value = ByteString
type Identifier = (BS.Series, BS.Series)

data Db = Db (JM.JournaledMap KeyHash (Key, Value)) Identifier

idToText :: Identifier -> Text
idToText (BS.Series a, BS.Series b) = decodeUtf8 $ append (Base64.encode a)
  (Base64.encode b)

textToId :: Text -> Identifier
textToId = undefined encodeUtf8 -- TODO

load :: BS.BlockStore Address -> Identifier -> IO Db
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
