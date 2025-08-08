{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Array utilities for Unison with writeability tracking
module Unison.Util.Array
  ( MutableArrayWithWriteability (..),
    MutableByteArrayWithWriteability (..),
    newArray,
    newArrayWith,
    readArray,
    writeArray,
    indexArray,
    indexArrayM,
    cloneArray,
    copyArray,
    copyMutableArray,
    freezeArray,
    unsafeFreezeArray,
    freezeAndPoisonArray,
    thawArray,
    unsafeThawArray,
    sizeofArray,
    sizeofMutableArray,
    sameMutableArray,
    newByteArray,
    newPinnedByteArray,
    newAlignedPinnedByteArray,
    byteArrayFromList,
    byteArrayFromListN,
    readByteArray,
    writeByteArray,
    indexByteArray,
    cloneByteArray,
    copyByteArray,
    copyMutableByteArray,
    moveByteArray,
    setByteArray,
    fillByteArray,
    freezeByteArray,
    unsafeFreezeByteArray,
    thawByteArray,
    unsafeThawByteArray,
    sizeofByteArray,
    sizeofMutableByteArray,
    sameMutableByteArray,
    byteArrayContents,
    mutableByteArrayContents,
    isByteArrayPinned,
    isMutableByteArrayPinned,
    freezeAndPoisonByteArray,
    isWriteable,
    setWriteable,
    setWriteableByteArray,
    isWriteableByteArray,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.Array qualified as PA
import Data.Primitive.ByteArray qualified as PBA
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Primitive.Ptr (Ptr (..))
import Data.Primitive.Types (Prim)
import Data.Word (Word8)
import GHC.ST (ST (..))

-- | A mutable array with writeability tracking
data MutableArrayWithWriteability s a = MutableArrayWithWriteability
  { array :: PA.MutableArray s a,
    writeable :: MutVar s Bool
  }

-- | A mutable byte array with writeability tracking
data MutableByteArrayWithWriteability s = MutableByteArrayWithWriteability
  { byteArray :: PBA.MutableByteArray s,
    writeable :: MutVar s Bool
  }

-- | Check if an array is writeable and throw an error if not
checkWriteable :: String -> MutVar s Bool -> ST s ()
checkWriteable op writeable = do
  isWriteable <- readMutVar writeable
  if isWriteable
    then return ()
    else error $ op ++ ": array is not writeable"

-- | Set the writeability flag
setWriteable :: MutableArrayWithWriteability s a -> Bool -> ST s ()
setWriteable (MutableArrayWithWriteability _ writeable) writeable' = writeMutVar writeable writeable'

-- | Check if an array is writeable
isWriteable :: MutableArrayWithWriteability s a -> ST s Bool
isWriteable (MutableArrayWithWriteability _ writeable) = readMutVar writeable

-- | Set the writeability flag for byte arrays
setWriteableByteArray :: MutableByteArrayWithWriteability s -> Bool -> ST s ()
setWriteableByteArray (MutableByteArrayWithWriteability _ writeable) writeable' = writeMutVar writeable writeable'

-- | Check if a byte array is writeable
isWriteableByteArray :: MutableByteArrayWithWriteability s -> ST s Bool
isWriteableByteArray (MutableByteArrayWithWriteability _ writeable) = readMutVar writeable

-- | Create a new array with writeability tracking
newArray :: Int -> a -> ST s (MutableArrayWithWriteability s a)
newArray n a = do
  arr <- PA.newArray n a
  writeable <- newMutVar True
  return $ MutableArrayWithWriteability arr writeable

-- | Create a new array with a function
newArrayWith :: Int -> (Int -> a) -> ST s (MutableArrayWithWriteability s a)
newArrayWith n f = do
  arr <- PA.newArray n (f 0)
  writeable <- newMutVar True
  let result = MutableArrayWithWriteability arr writeable
  -- Fill the array
  let fill i
        | i >= n = return result
        | otherwise = do
            PA.writeArray arr i (f i)
            fill (i + 1)
  fill 1
  return result

-- | Read from an array
readArray :: MutableArrayWithWriteability s a -> Int -> ST s a
readArray arr i = PA.readArray (array arr) i

-- | Write to an array (checks writeability)
writeArray :: MutableArrayWithWriteability s a -> Int -> a -> ST s ()
writeArray arr@(MutableArrayWithWriteability _ writeable) i a = do
  checkWriteable "writeArray" writeable
  PA.writeArray (array arr) i a

-- | Index into an immutable array
indexArray :: PA.Array a -> Int -> a
indexArray = PA.indexArray

-- | Index into an immutable array in a monadic context
indexArrayM :: PA.Array a -> Int -> ST s a
indexArrayM = PA.indexArrayM

-- | Clone a mutable array
cloneArray :: MutableArrayWithWriteability s a -> ST s (MutableArrayWithWriteability s a)
cloneArray arr = do
  cloned <- PA.cloneMutableArray (array arr) 0 (PA.sizeofMutableArray (array arr))
  writeable <- newMutVar True
  return $ MutableArrayWithWriteability cloned writeable

-- | Copy from an immutable array to a mutable array
copyArray :: MutableArrayWithWriteability s a -> Int -> PA.Array a -> Int -> Int -> ST s ()
copyArray dst@(MutableArrayWithWriteability _ writeable) d src s l = do
  checkWriteable "copyArray" writeable
  PA.copyArray (array dst) d src s l

-- | Copy from a mutable array to another mutable array
copyMutableArray :: MutableArrayWithWriteability s a -> Int -> MutableArrayWithWriteability s a -> Int -> Int -> ST s ()
copyMutableArray dst@(MutableArrayWithWriteability _ writeable) d src s l = do
  checkWriteable "copyMutableArray" writeable
  PA.copyMutableArray (array dst) d (array src) s l

-- | Freeze a mutable array
freezeArray :: MutableArrayWithWriteability s a -> Int -> Int -> ST s (PA.Array a)
freezeArray arr off len = do
  PA.freezeArray (array arr) off len

-- | Unsafe freeze a mutable array
unsafeFreezeArray :: MutableArrayWithWriteability s a -> ST s (PA.Array a)
unsafeFreezeArray arr = PA.unsafeFreezeArray (array arr)

-- | Freeze and poison a mutable array (sets writeability to False)
freezeAndPoisonArray :: MutableArrayWithWriteability s a -> ST s (PA.Array a)
freezeAndPoisonArray arr@(MutableArrayWithWriteability _ writeable) = do
  result <- PA.unsafeFreezeArray (array arr)
  writeMutVar writeable False
  return result

-- | Thaw an immutable array
thawArray :: PA.Array a -> Int -> Int -> ST s (MutableArrayWithWriteability s a)
thawArray arr off len = do
  mutable <- PA.thawArray arr off len
  writeable <- newMutVar True
  return $ MutableArrayWithWriteability mutable writeable

-- | Unsafe thaw an immutable array
unsafeThawArray :: PA.Array a -> ST s (MutableArrayWithWriteability s a)
unsafeThawArray arr = do
  mutable <- PA.unsafeThawArray arr
  writeable <- newMutVar True
  return $ MutableArrayWithWriteability mutable writeable

-- | Get the size of an immutable array
sizeofArray :: PA.Array a -> Int
sizeofArray = PA.sizeofArray

-- | Get the size of a mutable array
sizeofMutableArray :: MutableArrayWithWriteability s a -> Int
sizeofMutableArray arr = PA.sizeofMutableArray (array arr)

-- | Check if two mutable arrays are the same
sameMutableArray :: MutableArrayWithWriteability s a -> MutableArrayWithWriteability s a -> Bool
sameMutableArray arr1 arr2 = PA.sameMutableArray (array arr1) (array arr2)

-- | Create a new byte array with writeability tracking
newByteArray :: Int -> ST s (MutableByteArrayWithWriteability s)
newByteArray n = do
  arr <- PBA.newByteArray n
  writeable <- newMutVar True
  return $ MutableByteArrayWithWriteability arr writeable

-- | Create a new pinned byte array
newPinnedByteArray :: Int -> ST s (MutableByteArrayWithWriteability s)
newPinnedByteArray n = do
  arr <- PBA.newPinnedByteArray n
  writeable <- newMutVar True
  return $ MutableByteArrayWithWriteability arr writeable

-- | Create a new aligned pinned byte array
newAlignedPinnedByteArray :: Int -> Int -> ST s (MutableByteArrayWithWriteability s)
newAlignedPinnedByteArray n alignment = do
  arr <- PBA.newAlignedPinnedByteArray n alignment
  writeable <- newMutVar True
  return $ MutableByteArrayWithWriteability arr writeable

-- | Create a byte array from a list
byteArrayFromList :: [Word8] -> PBA.ByteArray
byteArrayFromList = PBA.byteArrayFromList

-- | Create a byte array from a list with a specific size
byteArrayFromListN :: Int -> [Word8] -> PBA.ByteArray
byteArrayFromListN = PBA.byteArrayFromListN

-- | Read from a byte array
readByteArray :: forall a m. (Prim a, PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> m a
readByteArray arr i = PBA.readByteArray @a (byteArray arr) i

-- | Write to a byte array (checks writeability)
writeByteArray :: forall a m. (Prim a, PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> a -> m ()
writeByteArray arr i a = do
  checkWriteableByteArray "writeByteArray" arr
  PBA.writeByteArray @a (byteArray arr) i a

-- | Index into an immutable byte array
indexByteArray :: forall a. (Prim a) => PBA.ByteArray -> Int -> a
indexByteArray = PBA.indexByteArray @a

-- | Clone a byte array
cloneByteArray :: MutableByteArrayWithWriteability s -> ST s (MutableByteArrayWithWriteability s)
cloneByteArray arr = do
  cloned <- PBA.cloneMutableByteArray (byteArray arr) 0 (PBA.sizeofMutableByteArray (byteArray arr))
  writeable <- newMutVar True
  return $ MutableByteArrayWithWriteability cloned writeable

-- | Copy from an immutable byte array to a mutable byte array
copyByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> PBA.ByteArray -> Int -> Int -> m ()
copyByteArray dst d src s l = do
  checkWriteableByteArray "copyByteArray" dst
  PBA.copyByteArray (byteArray dst) d src s l

-- | Copy from a mutable byte array to another mutable byte array
copyMutableByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> MutableByteArrayWithWriteability (PrimState m) -> Int -> Int -> m ()
copyMutableByteArray dst d src s l = do
  checkWriteableByteArray "copyMutableByteArray" dst
  PBA.copyMutableByteArray (byteArray dst) d (byteArray src) s l

-- | Move data between mutable byte arrays
moveByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> MutableByteArrayWithWriteability (PrimState m) -> Int -> Int -> m ()
moveByteArray dst d src s l = do
  checkWriteableByteArray "moveByteArray" dst
  PBA.moveByteArray (byteArray dst) d (byteArray src) s l

-- | Set a byte array to a value
setByteArray :: forall a m. (Prim a, PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> Int -> a -> m ()
setByteArray arr off len a = do
  checkWriteableByteArray "setByteArray" arr
  PBA.setByteArray @a (byteArray arr) off len a

-- | Fill a byte array with a value
fillByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> Int -> Word8 -> m ()
fillByteArray arr off len a = do
  checkWriteableByteArray "fillByteArray" arr
  PBA.fillByteArray (byteArray arr) off len a

-- | Freeze a mutable byte array
freezeByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> Int -> Int -> m PBA.ByteArray
freezeByteArray arr off len = do
  PBA.freezeByteArray (byteArray arr) off len

-- | Unsafe freeze a mutable byte array
unsafeFreezeByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> m PBA.ByteArray
unsafeFreezeByteArray arr = PBA.unsafeFreezeByteArray (byteArray arr)

-- | Freeze and poison a mutable byte array (sets writeability to False)
freezeAndPoisonByteArray :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> m PBA.ByteArray
freezeAndPoisonByteArray arr@(MutableByteArrayWithWriteability _ writeable) = do
  result <- PBA.unsafeFreezeByteArray (byteArray arr)
  writeMutVar writeable False
  return result

-- | Thaw an immutable byte array
thawByteArray :: PBA.ByteArray -> Int -> Int -> ST s (MutableByteArrayWithWriteability s)
thawByteArray arr off len = do
  mutable <- PBA.thawByteArray arr off len
  writeable <- newMutVar True
  return $ MutableByteArrayWithWriteability mutable writeable

-- | Unsafe thaw an immutable byte array
unsafeThawByteArray :: PBA.ByteArray -> ST s (MutableByteArrayWithWriteability s)
unsafeThawByteArray arr = do
  mutable <- PBA.unsafeThawByteArray arr
  writeable <- newMutVar True
  return $ MutableByteArrayWithWriteability mutable writeable

-- | Get the size of an immutable byte array
sizeofByteArray :: PBA.ByteArray -> Int
sizeofByteArray = PBA.sizeofByteArray

-- | Get the size of a mutable byte array
sizeofMutableByteArray :: MutableByteArrayWithWriteability s -> Int
sizeofMutableByteArray arr = PBA.sizeofMutableByteArray (byteArray arr)

-- | Check if two mutable byte arrays are the same
sameMutableByteArray :: MutableByteArrayWithWriteability s -> MutableByteArrayWithWriteability s -> Bool
sameMutableByteArray arr1 arr2 = PBA.sameMutableByteArray (byteArray arr1) (byteArray arr2)

-- | Get the contents of a byte array
byteArrayContents :: PBA.ByteArray -> Ptr Word8
byteArrayContents = PBA.byteArrayContents

-- | Get the contents of a mutable byte array
mutableByteArrayContents :: forall m. (PrimMonad m) => MutableByteArrayWithWriteability (PrimState m) -> m (Ptr Word8)
mutableByteArrayContents arr = return $ PBA.mutableByteArrayContents (byteArray arr)

-- | Check if a byte array is pinned
isByteArrayPinned :: PBA.ByteArray -> Bool
isByteArrayPinned = PBA.isByteArrayPinned

-- | Check if a mutable byte array is pinned
isMutableByteArrayPinned :: MutableByteArrayWithWriteability s -> Bool
isMutableByteArrayPinned arr = PBA.isMutableByteArrayPinned (byteArray arr)

-- | Helper function to check writeability for byte arrays
checkWriteableByteArray :: forall m. (PrimMonad m) => String -> MutableByteArrayWithWriteability (PrimState m) -> m ()
checkWriteableByteArray op (MutableByteArrayWithWriteability _ writeable) = do
  isWriteable <- readMutVar writeable
  if isWriteable
    then return ()
    else error $ op ++ ": byte array is not writeable"
