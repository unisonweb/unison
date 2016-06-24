module Unison.Runtime.Block where

import Data.Maybe
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.ByteString (ByteString)
import Unison.BlockStore (BlockStore, Series)
import qualified Unison.BlockStore as BlockStore
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put

-- | A `BlockStore.Series` along with some logic for serialization
data Block a = Block Series (Maybe ByteString -> IO a) (a -> IO (Maybe ByteString))

-- | Obtain the current value of the `Block`
get :: BlockStore h -> Block a -> IO a
get bs b = snd <$> get' bs b

-- | Obtain the value of a block along with its hash
get' :: BlockStore h -> Block a -> IO (h,a)
get' bs (Block series decode _) = do
  h <- BlockStore.declareSeries bs series
  bytes <- BlockStore.lookup bs h
  (,) h <$> decode bytes

-- | Attempt a compare and swap on the value of the block. Returns `Nothing` if the
-- current value of the block does not have the specified hash.
trySet :: Eq h => BlockStore h -> Block a -> h -> a -> IO (Maybe h)
trySet bs (Block series _ encode) h0 a = do
  h <- BlockStore.declareSeries bs series
  case h == h0 of
    True -> encode a >>= \o -> case o of
      Nothing -> pure Nothing
      Just bytes -> BlockStore.update bs series h bytes
    False -> pure Nothing

-- | Atomically modify a `Block` using a pure function
modify :: Eq h => BlockStore h -> Block a -> (a -> a) -> IO h
modify bs b f = modify' bs b (pure . f)

-- | Atomically modify a `Block` using an effectful function
modify' :: Eq h => BlockStore h -> Block a -> (a -> IO a) -> IO h
modify' bs b f = do
  (h,a) <- get' bs b
  a <- f a
  o <- trySet bs b h a
  case o of
    Nothing -> modify' bs b f
    Just h -> pure h

-- | Create a `Block` from the current bytes pointed to by a `Series`
fromSeries :: Series -> Block (Maybe ByteString)
fromSeries series = Block series pure pure

-- | Provide a default value for this `Block`
or :: Block (Maybe a) -> a -> Block a
or (Block series get set) a = Block series (fmap (fromMaybe a) . get) (set . Just)

-- | Serialize/deserialize a block of bytes
serial' :: Serial a => Block ByteString -> Block a
serial' b = xmap' decode encode b where
  decode bs = either fail pure $ Get.runGetS deserialize bs
  encode a = pure $ Put.runPutS (serialize a)

-- | Serialize/deserialize a block of bytes, given a default
serial :: Serial a => a -> Block (Maybe ByteString) -> Block a
serial a b = xmap' decode encode b where
  decode Nothing = pure a
  decode (Just bs) = either fail pure $ Get.runGetS deserialize bs
  encode a = pure . Just $ Put.runPutS (serialize a)

xmap' :: (a -> IO b) -> (b -> IO a) -> Block a -> Block b
xmap' to from (Block series get set) = Block series get' set' where
  get' bs = get bs >>= to
  set' b = from b >>= set

xmap :: (a -> b) -> (b -> a) -> Block a -> Block b
xmap to from = xmap' (pure . to) (pure . from)

-- todo: encryption, signatures, timestamping, caching, write-buffering, etc
