{-# Language TupleSections #-}

module Unison.Runtime.Block where

import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial, serialize, deserialize)
import Data.IORef
import Data.Maybe
import Unison.BlockStore (BlockStore, Series)
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Unison.BlockStore as BlockStore
import qualified Unison.Cryptography as C

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

-- | Obtain the stream of values in the `Block` series
gets :: BlockStore h -> Block a -> IO [IO a]
gets bs b = fmap (map (fmap snd)) (gets' bs b)

-- | Obtain the stream of values in the `Block` series, along with their hashes
gets' :: BlockStore h -> Block a -> IO [IO (h, a)]
gets' bs (Block series decode _) = do
  hs <- BlockStore.resolves bs series
  pure [ (,) h <$> (decode =<< BlockStore.lookup bs h) | h <- hs ]

-- | Attempt a compare and swap on the value of the block. Returns `Nothing` if the
-- current value of the block does not have the specified hash.
tryEdit :: Eq h
        => (BlockStore h -> Series -> h -> ByteString -> IO (Maybe h))
        -> BlockStore h -> Block a -> h -> a -> IO (Maybe h)
tryEdit edit bs (Block series _ encode) h0 a = do
  h <- BlockStore.declareSeries bs series
  case h == h0 of
    True -> encode a >>= \o -> case o of
      Nothing -> pure Nothing
      Just bytes -> edit bs series h bytes
    False -> pure Nothing

-- | Attempt a compare and swap on the value of the block. Returns `Nothing` if the
-- current value of the block does not have the specified hash.
tryUpdate :: Eq h => BlockStore h -> Block a -> h -> a -> IO (Maybe h)
tryUpdate = tryEdit BlockStore.update

-- | Try appending a value to this block series.
tryAppend :: Eq h => BlockStore h -> Block a -> h -> a -> IO (Maybe h)
tryAppend = tryEdit BlockStore.append

-- | Append a value to this block series.
append :: Eq h => BlockStore h -> Block a -> a -> IO h
append bs b@(Block series _ _) a = do
  h <- BlockStore.declareSeries bs series
  o <- tryAppend bs b h a
  case o of
    Nothing -> append bs b a
    Just h -> pure h

-- | Atomically modify a `Block` using a pure function
modify :: Eq h => BlockStore h -> Block a -> (a -> (a,b)) -> IO (h,b)
modify bs b f = modifyEffectful bs b (pure . f)

-- | Atomically modify a `Block` using a pure function
modify' :: Eq h => BlockStore h -> Block a -> (a -> a) -> IO h
modify' bs b f = fst <$> modify bs b (\a -> (f a, ()))

-- | Atomically modify a `Block` using an effectful function
modifyEffectful :: Eq h => BlockStore h -> Block a -> (a -> IO (a,b)) -> IO (h,b)
modifyEffectful bs b f = do
  (h,a) <- get' bs b
  (a,out) <- f a
  o <- tryUpdate bs b h a
  case o of
    Nothing -> modifyEffectful bs b f
    Just h -> pure (h,out)

modifyEffectful' :: Eq h => BlockStore h -> Block a -> (a -> IO a) -> IO h
modifyEffectful' bs b f = fst <$> modifyEffectful bs b (\a -> (,()) <$> f a)

-- | Create a `Block` from the current bytes pointed to by a `Series`
fromSeries :: Series -> Block (Maybe ByteString)
fromSeries series = Block series pure pure

-- | Provide a default value for this `Block`
or :: Block (Maybe a) -> a -> Block a
or (Block series get set) a = Block series (fmap (fromMaybe a) . get) (set . Just)

encrypted :: C.Cryptography t1 t2 t3 t4 t5 t6 ByteString
          -> Block (Maybe ByteString)
          -> Block (Maybe ByteString)
encrypted crypto b = xmap' decrypt encrypt b where
  decrypt Nothing = pure Nothing
  decrypt (Just bs) = either fail (pure.Just) $ C.decryptAsymmetric crypto bs
  encrypt Nothing = pure Nothing
  encrypt (Just bs) = Just <$> C.encryptAsymmetric crypto (C.publicKey crypto) bs

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

serialM :: Serial a => IO a -> Block (Maybe ByteString) -> Block a
serialM gen b = xmap' decode encode b where
  decode Nothing = gen
  decode (Just bs) = either fail pure $ Get.runGetS deserialize bs
  encode a = pure . Just $ Put.runPutS (serialize a)

cache :: Block a -> IO (Block a)
cache (Block series get set) = cached <$> newIORef Nothing where
  cached cache = Block series get' set' where
    get' mbs = do
      c <- readIORef cache
      case c of
        Nothing -> do a <- get mbs; writeIORef cache (Just a); pure a
        Just a -> pure a
    set' a = do
      writeIORef cache Nothing
      set a

xmap' :: (a -> IO b) -> (b -> IO a) -> Block a -> Block b
xmap' to from (Block series get set) = Block series get' set' where
  get' bs = get bs >>= to
  set' b = from b >>= set

xmap :: (a -> b) -> (b -> a) -> Block a -> Block b
xmap to from = xmap' (pure . to) (pure . from)


-- buffer :: Block a -> IO (Block a)
-- todo: encryption, signatures, timestamping, caching, write-buffering, etc
-- Blocks as a separate type
