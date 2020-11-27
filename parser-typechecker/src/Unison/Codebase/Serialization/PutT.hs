module Unison.Codebase.Serialization.PutT where

import Data.Bytes.Put
import Data.Serialize.Put
  ( PutM,
    runPutM,
  )
import qualified Data.Serialize.Put as Ser

newtype PutT m a = PutT {unPutT :: m (PutM a)}

instance Monad m => MonadPut (PutT m) where
  putWord8 = PutT . pure . putWord8
  {-# INLINE putWord8 #-}
  putByteString = PutT . pure . putByteString
  {-# INLINE putByteString #-}
  putLazyByteString = PutT . pure . putLazyByteString
  {-# INLINE putLazyByteString #-}
  flush = PutT $ pure flush
  {-# INLINE flush #-}
  putWord16le = PutT . pure . putWord16le
  {-# INLINE putWord16le #-}
  putWord16be = PutT . pure . putWord16be
  {-# INLINE putWord16be #-}
  putWord16host = PutT . pure . putWord16host
  {-# INLINE putWord16host #-}
  putWord32le = PutT . pure . putWord32le
  {-# INLINE putWord32le #-}
  putWord32be = PutT . pure . putWord32be
  {-# INLINE putWord32be #-}
  putWord32host = PutT . pure . putWord32host
  {-# INLINE putWord32host #-}
  putWord64le = PutT . pure . putWord64le
  {-# INLINE putWord64le #-}
  putWord64be = PutT . pure . putWord64be
  {-# INLINE putWord64be #-}
  putWord64host = PutT . pure . putWord64host
  {-# INLINE putWord64host #-}
  putWordhost = PutT . pure . putWordhost
  {-# INLINE putWordhost #-}

instance Functor m => Functor (PutT m) where
  fmap f (PutT m) = PutT $ fmap (fmap f) m

instance Applicative m => Applicative (PutT m) where
  pure = PutT . pure . pure
  (PutT f) <*> (PutT a) = PutT $ (<*>) <$> f <*> a

instance Monad m => Monad (PutT m) where
  (PutT m) >>= f = PutT $ do
    putm <- m
    let (a, bs) = runPutM putm
    putm' <- unPutT $ f a
    let (b, bs') = runPutM putm'
    pure $ do
      Ser.putByteString bs
      Ser.putByteString bs'
      pure b
