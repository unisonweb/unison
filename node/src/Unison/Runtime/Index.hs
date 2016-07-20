{-# LANGUAGE OverloadedStrings #-}
module Unison.Runtime.Index
  (Unison.Runtime.Index.lookup
  ,Unison.Runtime.Index.delete
  ,Unison.Runtime.Index.insert
  ,Unison.Runtime.Index.lookupGT
  ,load
  ,idToText
  ,textToId
  ,Identifier
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Unison.Runtime.Address
import Unison.Runtime.JournaledMap as JM
import qualified Unison.BlockStore as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64

type KeyHash = ByteString
type Key = ByteString
type Value = ByteString
type Identifier = (BS.Series, BS.Series)

data Db = Db (JM.JournaledMap KeyHash (Key, Value)) Identifier

idToText :: Identifier -> Text
idToText (BS.Series a, BS.Series b) = decodeUtf8 $ B.concat
  [Base64.encode a, B.cons 20 $ Base64.encode b] -- delineated by a space

textToId :: Text -> Identifier
textToId t =
  let [a, b] = B.split 20 $ encodeUtf8 t
      decode = BS.Series . Base64.decodeLenient
  in (decode a, decode b)

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
