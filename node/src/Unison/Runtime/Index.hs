{-# LANGUAGE OverloadedStrings #-}
module Unison.Runtime.Index
  (Unison.Runtime.Index.lookup
  ,Unison.Runtime.Index.delete
  ,Unison.Runtime.Index.insert
  ,Unison.Runtime.Index.lookupGT
  ,Unison.Runtime.Index.flush
  ,idToText
  ,load
  ,loadEncrypted
  ,textToId
  ,Identifier
  ) where

import Control.Concurrent.STM (STM)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Unison.Cryptography
import Unison.Runtime.Journal as J
import Unison.Runtime.JournaledMap as JM
import qualified Unison.BlockStore as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Map as Map

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

load :: (Eq a) => BS.BlockStore a -> Identifier -> IO Db
load bs (cp, ud) = do
  jm <- JM.fromSeries bs cp ud
  pure $ Db jm (cp, ud)

loadEncrypted :: (Eq a) => BS.BlockStore a -> Cryptography t1 t2 t3 t4 t5 t6 ByteString
  -> Identifier -> IO Db
loadEncrypted bs crypto (cp, ud) = do
  jm <- JM.fromEncryptedSeries crypto bs cp ud
  pure $ Db jm (cp, ud)

flush :: Db -> IO ()
flush (Db journaledMap _) = J.record journaledMap

insert :: KeyHash -> (Key, Value) -> Db -> STM (STM ())
insert kh kv (Db journaledMap _) = J.updateNowAsyncFlush (JM.Insert kh kv) journaledMap

delete :: KeyHash -> Db -> STM (STM ())
delete kh (Db journaledMap _) = J.updateNowAsyncFlush (JM.Delete kh) journaledMap

lookup :: KeyHash -> Db -> STM (Maybe (Key, Value))
lookup kh (Db journaledMap _) = Map.lookup kh <$> J.get journaledMap

-- | Find next key in the Db whose key is greater than the provided key
lookupGT :: KeyHash -> Db -> STM (Maybe (KeyHash, (Key, Value)))
lookupGT kh (Db journaledMap _) = Map.lookupGT kh <$> J.get journaledMap

