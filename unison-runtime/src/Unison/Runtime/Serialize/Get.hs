
module Unison.Runtime.Serialize.Get
  ( Get (..)
  , GetExn (..)
  , getExnMsg
  , PrimBase (..)
  , Ix (..)
  , evaluated
  , getByteString
  , getWord8
  , getVarInt
  , getInt64be
  , getWord16be
  , getWord32be
  , getWord64be
  , getWord64le
  , getDoublebe
  , getFloatbe
  , remaining
  , runGet
  , runGetCatchIO
  ) where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (first)
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Primitive.PrimVar
import Data.Int
import Data.Word

-- TODO: replace with GHC builtins after upgrading to GHC 9.10
foreign import ccall unisonWord32ToFloat :: Word32 -> Float
foreign import ccall unisonWord64ToDouble :: Word64 -> Double

newtype Ix m = Ix (PrimVar (PrimState m) Int)

newtype Get m a = Get { unGet :: ByteString -> Ix m -> m a }

runGet :: PrimBase m => Get m a -> ByteString -> m a
runGet (Get k) bs = newPrimVar 0 >>= k bs . Ix
{-# specialize runGet :: Get IO a -> ByteString -> IO a #-}
{-# specialize runGet :: Get (ST s) a -> ByteString -> ST s a #-}

runGetCatchIO :: Get IO a -> ByteString -> IO (Either String a)
runGetCatchIO g bs = fmap (first getExnMsg) . try $ runGet g bs

evaluated :: PrimBase m => a -> Get m a
evaluated x = Get \_ _ -> evalPrim x
{-# inline evaluated #-}

data GetExn
  = InsufficientBytes String
  | UserExn String
  deriving (Show)

getExnMsg :: GetExn -> String
getExnMsg (InsufficientBytes name) = "insufficient bytes in " ++ name
getExnMsg (UserExn msg) = msg

instance Exception GetExn

instance Functor m => Functor (Get m) where
  fmap f (Get k) = Get \b i -> fmap f (k b i)
  {-# inline fmap #-}
  x <$ Get k = Get \b i -> x <$ k b i
  {-# inline (<$) #-}

instance Applicative m => Applicative (Get m) where
  pure x = Get \_ _ -> pure x
  {-# inline pure #-}
  Get kf <*> Get kx = Get \b i -> kf b i <*> kx b i
  {-# inline (<*>) #-}
  liftA2 f (Get kx) (Get ky) = Get \b i -> liftA2 f (kx b i) (ky b i)
  {-# inline liftA2 #-}
  Get kx *> Get ky = Get \b i -> kx b i *> ky b i
  {-# inline (*>) #-}
  Get kx <* Get ky = Get \b i -> kx b i <* ky b i
  {-# inline (<*) #-}

instance Monad m => Monad (Get m) where
  Get k >>= f = Get \b i -> k b i >>= \x -> unGet (f x) b i
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

instance Monad m => MonadFail (Get m) where
  fail s = throw $ UserExn s

instance MonadTrans Get where
  lift m = Get \_ _ -> m
  {-# inline lift #-}

remaining :: PrimBase m => Get m Int
remaining = Get \bs (Ix ix) -> f bs <$> readPrimVar ix
  where
    f bs i = BS.length bs - i
{-# inlinable remaining #-}

getWord8 :: PrimBase m => Get m Word8
getWord8 = Get \bs (Ix ix) -> readPrimVar ix >>= \case
  i | i < BS.length bs ->
        BS.unsafeIndex bs i <$ writePrimVar ix (i+1)
    | otherwise -> throw $ InsufficientBytes "getWord8"
{-# inline getWord8 #-}

getVarInt :: (Bits int, Num int, Ord int, PrimBase m) => Get m int
getVarInt = Get \bs (Ix ix) -> readPrimVar ix >>= buildVarInt bs ix
{-# inlinable getVarInt #-}

buildVarInt ::
  (Bits int, Num int, Ord int, PrimBase m) =>
  ByteString ->
  PrimVar (PrimState m) Int ->
  Int ->
  m int
buildVarInt bs ix i0
  | i0 < sz = eat (i0+1) 0 0 $ grab i0
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
        eat (i+1) acc (sh+7) $ grab i
    | otherwise =
        throw $ InsufficientBytes "getVarInt"
{-# inlinable buildVarInt #-}

getInt64be :: PrimBase m => Get m Int64
getInt64be = Get \bs (Ix ix) -> readPrimVar ix >>= \case
  i | i + 7 < BS.length bs -> build bs i <$ writePrimVar ix (i+8)
    | otherwise -> throw $ InsufficientBytes "getInt64be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs  i     ) !<<. 56) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 48) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 40) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 3)) !<<. 32) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 4)) !<<. 24) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 5)) !<<. 16) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 6)) !<<.  8) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 7)))
{-# specialize getInt64be :: Get IO Int64 #-}
{-# specialize getInt64be :: Get (ST s) Int64 #-}

getWord16be :: PrimBase m => Get m Word16
getWord16be = Get \bs (Ix ix) -> readPrimVar ix >>= \case
  i | i + 1 < BS.length bs -> build bs i <$ writePrimVar ix (i+2)
    | otherwise -> throw $ InsufficientBytes "getWord16be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs  i) !<<. 8) .|.
      (fromIntegral (BS.unsafeIndex bs (i+1)))
{-# specialize getWord16be :: Get IO Word16 #-}
{-# specialize getWord16be :: Get (ST s) Word16 #-}

getWord32be :: PrimBase m => Get m Word32
getWord32be = Get \bs (Ix ix) -> readPrimVar ix >>= \case
  i | i + 3 < BS.length bs -> build bs i <$ writePrimVar ix (i+4)
    | otherwise -> throw $ InsufficientBytes "getWord32be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs  i     ) !<<. 24) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 16) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<.  8) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 3)))
{-# specialize getWord32be :: Get IO Word32 #-}
{-# specialize getWord32be :: Get (ST s) Word32 #-}

getWord64be :: PrimBase m => Get m Word64
getWord64be = Get \bs (Ix ix) -> readPrimVar ix >>= \case
  i | i + 7 < BS.length bs -> build bs i <$ writePrimVar ix (i+8)
    | otherwise -> throw $ InsufficientBytes "getWord64be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs  i     ) !<<. 56) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<. 48) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 40) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 3)) !<<. 32) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 4)) !<<. 24) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 5)) !<<. 16) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 6)) !<<.  8) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 7)))
{-# specialize getWord64be :: Get IO Word64 #-}
{-# specialize getWord64be :: Get (ST s) Word64 #-}

getWord64le :: PrimBase m => Get m Word64
getWord64le = Get \bs (Ix ix) -> readPrimVar ix >>= \case
  i | i + 7 < BS.length bs -> build bs i <$ writePrimVar ix (i+8)
    | otherwise -> throw $ InsufficientBytes "getWord64be"
  where
    build bs !i =
      (fromIntegral (BS.unsafeIndex bs (i + 7)) !<<. 56) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 6)) !<<. 48) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 5)) !<<. 40) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 4)) !<<. 32) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 3)) !<<. 24) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 2)) !<<. 16) .|.
      (fromIntegral (BS.unsafeIndex bs (i + 1)) !<<.  8) .|.
      (fromIntegral (BS.unsafeIndex bs  i     ))
{-# specialize getWord64le :: Get IO Word64 #-}
{-# specialize getWord64le :: Get (ST s) Word64 #-}

getFloatbe :: PrimBase m => Get m Float
getFloatbe = unisonWord32ToFloat <$> getWord32be
{-# inlinable getFloatbe #-}

getDoublebe :: PrimBase m => Get m Double
getDoublebe = unisonWord64ToDouble <$> getWord64be
{-# inlinable getDoublebe #-}

getByteString :: PrimBase m => Int -> Get m ByteString
getByteString n = Get \bs (Ix ix) -> do
  i <- readPrimVar ix
  if i + n <= BS.length bs
  then BS.unsafeTake n (BS.unsafeDrop i bs) <$ writePrimVar ix (i+n)
  else throw $ InsufficientBytes "getBytes"
{-# inlinable getByteString #-}
