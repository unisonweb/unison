module Unison.Runtime.Serialize.Get
  ( Get (..),
    GetExn (..),
    getExnMsg,
    PrimBase (..),
    Ix (..),
    evaluated,
    getByteString,
    getWord8,
    getVarInt,
    getInt64be,
    getWord16be,
    getWord32be,
    getWord64be,
    getWord64le,
    getDoublebe,
    getFloatbe,
    getAccumulating,
    getAccumulatingRevList,
    getArray,
    getList,
    getSeq,
    getPrimArray,
    remaining,
    runGet,
    runGetCatch,
    runGetCatchIO,
  )
where

import Control.Exception
import Control.Monad (replicateM)
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Int
import Data.Primitive.Array
import Data.Primitive.PrimArray
import Data.Primitive.PrimVar
import Data.Primitive.Types
import Data.Sequence qualified as Seq
import Data.Word

-- TODO: replace with GHC builtins after upgrading to GHC 9.10
foreign import ccall unisonWord32ToFloat :: Word32 -> Float

foreign import ccall unisonWord64ToDouble :: Word64 -> Double

newtype Ix m = Ix (PrimVar (PrimState m) Int)

newtype Get m a = Get {unGet :: ByteString -> Ix m -> m a}

runGet :: (PrimBase m) => Get m a -> ByteString -> m a
runGet (Get k) bs = newPrimVar 0 >>= k bs . Ix
{-# SPECIALIZE runGet :: Get IO a -> ByteString -> IO a #-}
{-# SPECIALIZE runGet :: Get (ST s) a -> ByteString -> ST s a #-}

runGetCatchIO :: Get IO a -> ByteString -> IO (Either String a)
runGetCatchIO g bs = fmap (first getExnMsg) . try $ runGet g bs

-- This might be somewhat unsafe. It uses IO facilities to catch
-- exceptions in an ST-like monad. When in doubt, prefer `runGetCatchIO`.
runGetCatch ::
  (PrimBase m) => Get m a -> ByteString -> m (Either String a)
runGetCatch g bs =
  unsafeIOToPrim
    . fmap (first getExnMsg)
    . try
    . unsafePrimToIO
    $ runGet g bs
-- runGetCatch @IO should just be safer to run as runGetCatchIO
{-# NOINLINE [1] runGetCatch #-}

{-# RULES "runGetCatch/IO" runGetCatch = runGetCatchIO #-}

evaluated :: (PrimBase m) => a -> Get m a
evaluated x = Get \_ _ -> evalPrim x
{-# INLINE evaluated #-}

data GetExn
  = InsufficientBytes String
  | UserExn String
  deriving (Show)

getExnMsg :: GetExn -> String
getExnMsg (InsufficientBytes name) = "insufficient bytes in " ++ name
getExnMsg (UserExn msg) = msg

instance Exception GetExn

instance (Functor m) => Functor (Get m) where
  fmap f (Get k) = Get \b i -> fmap f (k b i)
  {-# INLINE fmap #-}
  x <$ Get k = Get \b i -> x <$ k b i
  {-# INLINE (<$) #-}

instance (Applicative m) => Applicative (Get m) where
  pure x = Get \_ _ -> pure x
  {-# INLINE pure #-}
  Get kf <*> Get kx = Get \b i -> kf b i <*> kx b i
  {-# INLINE (<*>) #-}
  liftA2 f (Get kx) (Get ky) = Get \b i -> liftA2 f (kx b i) (ky b i)
  {-# INLINE liftA2 #-}
  Get kx *> Get ky = Get \b i -> kx b i *> ky b i
  {-# INLINE (*>) #-}
  Get kx <* Get ky = Get \b i -> kx b i <* ky b i
  {-# INLINE (<*) #-}

instance (Monad m) => Monad (Get m) where
  Get k >>= f = Get \b i -> k b i >>= \x -> unGet (f x) b i
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance (Monad m) => MonadFail (Get m) where
  fail s = throw $ UserExn s

instance MonadTrans Get where
  lift m = Get \_ _ -> m
  {-# INLINE lift #-}

remaining :: (PrimBase m) => Get m Int
remaining = Get \bs (Ix ix) -> f bs <$> readPrimVar ix
  where
    f bs i = BS.length bs - i
{-# INLINEABLE remaining #-}

getWord8 :: (PrimBase m) => Get m Word8
getWord8 = Get \bs (Ix ix) ->
  readPrimVar ix >>= \case
    i
      | i < BS.length bs ->
          BS.unsafeIndex bs i <$ writePrimVar ix (i + 1)
      | otherwise -> throw $ InsufficientBytes "getWord8"
{-# INLINE getWord8 #-}

getVarInt :: (Bits int, Num int, Ord int, PrimBase m) => Get m int
getVarInt = Get \bs (Ix ix) -> readPrimVar ix >>= buildVarInt bs ix
{-# INLINEABLE getVarInt #-}

buildVarInt ::
  (Bits int, Num int, Ord int, PrimBase m) =>
  ByteString ->
  PrimVar (PrimState m) Int ->
  Int ->
  m int
buildVarInt bs ix i0
  | i0 < sz = eat (i0 + 1) 0 0 $ grab i0
  | otherwise = throw $ InsufficientBytes "getVarInt"
  where
    sz = BS.length bs
    grab j = fromIntegral $ BS.unsafeIndex bs j

    eat !i !acc !sh !m
      | not $ testBit m 7,
        acc <- acc .|. (m !<<. sh) =
          acc <$ writePrimVar ix i
      | i < sz,
        acc <- acc .|. (clearBit m 7 !<<. sh) =
          eat (i + 1) acc (sh + 7) $ grab i
      | otherwise =
          throw $ InsufficientBytes "getVarInt"
{-# INLINEABLE buildVarInt #-}

getInt64be :: (PrimBase m) => Get m Int64
getInt64be = Get \bs (Ix ix) ->
  readPrimVar ix >>= \case
    i
      | i + 7 < BS.length bs -> build bs i <$ writePrimVar ix (i + 8)
      | otherwise -> throw $ InsufficientBytes "getInt64be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs i) !<<. 56)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 48)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 40)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 3)) !<<. 32)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 4)) !<<. 24)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 5)) !<<. 16)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 6)) !<<. 8)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 7)))
{-# SPECIALIZE getInt64be :: Get IO Int64 #-}
{-# SPECIALIZE getInt64be :: Get (ST s) Int64 #-}

getWord16be :: (PrimBase m) => Get m Word16
getWord16be = Get \bs (Ix ix) ->
  readPrimVar ix >>= \case
    i
      | i + 1 < BS.length bs -> build bs i <$ writePrimVar ix (i + 2)
      | otherwise -> throw $ InsufficientBytes "getWord16be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs i) !<<. 8)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 1)))
{-# SPECIALIZE getWord16be :: Get IO Word16 #-}
{-# SPECIALIZE getWord16be :: Get (ST s) Word16 #-}

getWord32be :: (PrimBase m) => Get m Word32
getWord32be = Get \bs (Ix ix) ->
  readPrimVar ix >>= \case
    i
      | i + 3 < BS.length bs -> build bs i <$ writePrimVar ix (i + 4)
      | otherwise -> throw $ InsufficientBytes "getWord32be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs i) !<<. 24)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 16)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 8)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 3)))
{-# SPECIALIZE getWord32be :: Get IO Word32 #-}
{-# SPECIALIZE getWord32be :: Get (ST s) Word32 #-}

getWord64be :: (PrimBase m) => Get m Word64
getWord64be = Get \bs (Ix ix) ->
  readPrimVar ix >>= \case
    i
      | i + 7 < BS.length bs -> build bs i <$ writePrimVar ix (i + 8)
      | otherwise -> throw $ InsufficientBytes "getWord64be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs i) !<<. 56)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 48)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 40)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 3)) !<<. 32)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 4)) !<<. 24)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 5)) !<<. 16)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 6)) !<<. 8)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 7)))
{-# SPECIALIZE getWord64be :: Get IO Word64 #-}
{-# SPECIALIZE getWord64be :: Get (ST s) Word64 #-}

getWord64le :: (PrimBase m) => Get m Word64
getWord64le = Get \bs (Ix ix) ->
  readPrimVar ix >>= \case
    i
      | i + 7 < BS.length bs -> build bs i <$ writePrimVar ix (i + 8)
      | otherwise -> throw $ InsufficientBytes "getWord64be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs (i + 7)) !<<. 56)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 6)) !<<. 48)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 5)) !<<. 40)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 4)) !<<. 32)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 3)) !<<. 24)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 16)
        .|. (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 8)
        .|. (fromIntegral (BS.unsafeIndex bs i))
{-# SPECIALIZE getWord64le :: Get IO Word64 #-}
{-# SPECIALIZE getWord64le :: Get (ST s) Word64 #-}

getFloatbe :: (PrimBase m) => Get m Float
getFloatbe = unisonWord32ToFloat <$> getWord32be
{-# INLINEABLE getFloatbe #-}

getDoublebe :: (PrimBase m) => Get m Double
getDoublebe = unisonWord64ToDouble <$> getWord64be
{-# INLINEABLE getDoublebe #-}

getByteString :: (PrimBase m) => Int -> Get m ByteString
getByteString n = Get \bs (Ix ix) -> do
  i <- readPrimVar ix
  if i + n <= BS.length bs
    then BS.unsafeTake n (BS.unsafeDrop i bs) <$ writePrimVar ix (i + n)
    else throw $ InsufficientBytes "getBytes"
{-# INLINEABLE getByteString #-}

getList :: (PrimBase m) => Get m a -> Get m [a]
getList ga = getVarInt >>= (`replicateM` ga)
{-# INLINE getList #-}

-- Builds a result by repeated snoc in an efficient loop. Should only be
-- used when the snoc is efficient.
getAccumulating ::
  (PrimBase m) =>
  s ->
  (s -> a -> s) ->
  (s -> r) ->
  Get m a ->
  Get m r
getAccumulating nil snoc finish = \ga -> Get \bs ix ->
  let loop !as (n :: Int)
        | n <= 0 = evalPrim $ finish as
        | otherwise = unGet ga bs ix >>= \a -> loop (snoc as a) (n - 1)
   in unGet getVarInt bs ix >>= loop nil
{-# INLINE getAccumulating #-}

getAccumulatingRevList ::
  (PrimBase m) =>
  ([a] -> r) ->
  Get m a ->
  Get m r
getAccumulatingRevList finish = \ga -> Get \bs ix ->
  let loop as (n :: Int)
        | n <= 0 = evalPrim $ finish as
        | otherwise = unGet ga bs ix >>= \a -> loop (a : as) (n - 1)
   in unGet getVarInt bs ix >>= loop []
{-# INLINE getAccumulatingRevList #-}

getSeq :: (PrimBase m) => Get m a -> Get m (Seq.Seq a)
getSeq ga = getAccumulating mempty (Seq.|>) id ga
{-# INLINEABLE getSeq #-}

getArray :: (PrimBase m) => Get m a -> Get m (Array a)
getArray ga = Get \bs ix -> do
  sz <- unGet getVarInt bs ix
  dst <- newArray sz (error "getArray: bad element")
  let fill i
        | i < sz = unGet ga bs ix >>= writeArray dst i >> fill (i + 1)
        | otherwise = unsafeFreezeArray dst
  fill 0
{-# INLINE getArray #-}

getPrimArray :: (PrimBase m, Prim a) => Get m a -> Get m (PrimArray a)
getPrimArray ga = Get \bs ix -> do
  sz <- unGet getVarInt bs ix
  dst <- newPrimArray sz
  let fill i
        | i < sz = unGet ga bs ix >>= writePrimArray dst i >> fill (i + 1)
        | otherwise = unsafeFreezePrimArray dst
  fill 0
{-# INLINE getPrimArray #-}
