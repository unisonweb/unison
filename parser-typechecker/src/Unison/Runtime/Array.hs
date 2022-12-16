{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- This module wraps the operations in the primitive package so that
-- bounds checks can be toggled on during the build for debugging
-- purposes. It exports the entire API for the three array types
-- needed, and adds wrappers for the operations that are unchecked in
-- the base library.
--
-- Checking is toggled using the `arraychecks` flag.
module Unison.Runtime.Array
  ( module EPA,
    readArray,
    writeArray,
    copyArray,
    copyMutableArray,
    cloneMutableArray,
    readByteArray,
    writeByteArray,
    indexByteArray,
    copyByteArray,
    copyMutableByteArray,
    moveByteArray,
    readPrimArray,
    writePrimArray,
    indexPrimArray,
  )
where

import Control.Monad.Primitive
import Data.Kind (Constraint)
import Data.Primitive.Array as EPA hiding
  ( cloneMutableArray,
    copyArray,
    copyMutableArray,
    readArray,
    writeArray,
  )
import qualified Data.Primitive.Array as PA
import Data.Primitive.ByteArray as EPA hiding
  ( copyByteArray,
    copyMutableByteArray,
    indexByteArray,
    moveByteArray,
    readByteArray,
    writeByteArray,
  )
import qualified Data.Primitive.ByteArray as PA
import Data.Primitive.PrimArray as EPA hiding
  ( indexPrimArray,
    readPrimArray,
    writePrimArray,
  )
import qualified Data.Primitive.PrimArray as PA
import Data.Primitive.Types

#ifdef ARRAY_CHECK
import GHC.Stack

type CheckCtx :: Constraint
type CheckCtx = HasCallStack

type MA = MutableArray
type MBA = MutableByteArray
type A = Array
type BA = ByteArray

-- check index mutable array
checkIMArray
  :: CheckCtx
  => String
  -> (MA s a -> Int -> r)
  -> MA s a -> Int -> r
checkIMArray name f arr i
  | i < 0 || sizeofMutableArray arr <= i
  = error $ name ++ " unsafe check out of bounds: " ++ show i
  | otherwise = f arr i
{-# inline checkIMArray #-}

-- check copy array
checkCArray
  :: CheckCtx
  => String
  -> (MA s a -> Int -> A a -> Int -> Int -> r)
  -> MA s a -> Int -> A a -> Int -> Int -> r
checkCArray name f dst d src s l
  | d < 0
  || s < 0
  || sizeofMutableArray dst < d + l
  || sizeofArray src < s + l
  = error $ name ++ " unsafe check out of bounds: " ++ show (d, s, l)
  | otherwise = f dst d src s l
{-# inline checkCArray #-}

-- check copy mutable array
checkCMArray
  :: CheckCtx
  => String
  -> (MA s a -> Int -> MA s a -> Int -> Int -> r)
  -> MA s a -> Int -> MA s a -> Int -> Int -> r
checkCMArray name f dst d src s l
  | d < 0
  || s < 0
  || sizeofMutableArray dst < d + l
  || sizeofMutableArray src < s + l
  = error $ name ++ " unsafe check out of bounds: " ++ show (d, s, l)
  | otherwise = f dst d src s l
{-# inline checkCMArray #-}

-- check range mutable array
checkRMArray
  :: CheckCtx
  => String
  -> (MA s a -> Int -> Int -> r)
  -> MA s a -> Int -> Int -> r
checkRMArray name f arr o l
  | o < 0 || sizeofMutableArray arr < o+l
  = error $ name ++ "unsafe check out of bounds: " ++ show (o, l)
  | otherwise = f arr o l
{-# inline checkRMArray #-}

-- check index byte array
checkIBArray
  :: CheckCtx
  => Prim a
  => String
  -> a
  -> (ByteArray -> Int -> r)
  -> ByteArray -> Int -> r
checkIBArray name a f arr i
  | i < 0 || sizeofByteArray arr `quot` sizeOf a <= i
  = error $ name ++ " unsafe check out of bounds: " ++ show i
  | otherwise = f arr i
{-# inline checkIBArray #-}

-- check index mutable byte array
checkIMBArray
  :: CheckCtx
  => Prim a
  => String
  -> a
  -> (MutableByteArray s -> Int -> r)
  -> MutableByteArray s -> Int -> r
checkIMBArray name a f arr i
  | i < 0 || sizeofMutableByteArray arr `quot` sizeOf a <= i
  = error $ name ++ " unsafe check out of bounds: " ++ show i
  | otherwise = f arr i
{-# inline checkIMBArray #-}

-- check copy byte array
checkCBArray
  :: CheckCtx
  => String
  -> (MBA s -> Int -> BA -> Int -> Int -> r)
  -> MBA s -> Int -> BA -> Int -> Int -> r
checkCBArray name f dst d src s l
  | d < 0
  || s < 0
  || sizeofMutableByteArray dst < d + l
  || sizeofByteArray src < s + l
  = error $ name ++ " unsafe check out of bounds: " ++ show (d, s, l)
  | otherwise = f dst d src s l
{-# inline checkCBArray #-}

-- check copy mutable byte array
checkCMBArray
  :: CheckCtx
  => String
  -> (MBA s -> Int -> MBA s -> Int -> Int -> r)
  -> MBA s -> Int -> MBA s -> Int -> Int -> r
checkCMBArray name f dst d src s l
  | d < 0
  || s < 0
  || sizeofMutableByteArray dst < d + l
  || sizeofMutableByteArray src < s + l
  = error $ name ++ " unsafe check out of bounds: " ++ show (d, s, l)
  | otherwise = f dst d src s l
{-# inline checkCMBArray #-}

-- check index prim array
checkIPArray
  :: CheckCtx
  => Prim a
  => String
  -> (PrimArray a -> Int -> r)
  -> PrimArray a -> Int -> r
checkIPArray name f arr i
  | i < 0 || sizeofPrimArray arr <= i
  = error $ name ++ " unsafe check out of bounds: " ++ show i
  | otherwise = f arr i
{-# inline checkIPArray #-}

-- check index mutable prim array
checkIMPArray
  :: CheckCtx
  => Prim a
  => String
  -> (MutablePrimArray s a -> Int -> r)
  -> MutablePrimArray s a -> Int -> r
checkIMPArray name f arr i
  | i < 0 || sizeofMutablePrimArray arr <= i
  = error $ name ++ " unsafe check out of bounds: " ++ show i
  | otherwise = f arr i
{-# inline checkIMPArray #-}

#else
type CheckCtx :: Constraint
type CheckCtx = ()

checkIMArray, checkIMPArray, checkIPArray :: String -> r -> r
checkCArray, checkCMArray, checkRMArray :: String -> r -> r
checkIMArray _ = id
checkIMPArray _ = id
checkCArray _ = id
checkCMArray _ = id
checkRMArray _ = id
checkIPArray _ = id

checkIBArray, checkIMBArray :: String -> a -> r -> r
checkCBArray, checkCMBArray :: String -> r -> r
checkIBArray _ _ = id
checkIMBArray _ _ = id
checkCBArray _ = id
checkCMBArray _ = id
#endif

readArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableArray (PrimState m) a ->
  Int ->
  m a
readArray = checkIMArray "readArray" PA.readArray
{-# INLINE readArray #-}

writeArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableArray (PrimState m) a ->
  Int ->
  a ->
  m ()
writeArray = checkIMArray "writeArray" PA.writeArray
{-# INLINE writeArray #-}

copyArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableArray (PrimState m) a ->
  Int ->
  Array a ->
  Int ->
  Int ->
  m ()
copyArray = checkCArray "copyArray" PA.copyArray
{-# INLINE copyArray #-}

cloneMutableArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableArray (PrimState m) a ->
  Int ->
  Int ->
  m (MutableArray (PrimState m) a)
cloneMutableArray = checkRMArray "cloneMutableArray" PA.cloneMutableArray
{-# INLINE cloneMutableArray #-}

copyMutableArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableArray (PrimState m) a ->
  Int ->
  MutableArray (PrimState m) a ->
  Int ->
  Int ->
  m ()
copyMutableArray = checkCMArray "copyMutableArray" PA.copyMutableArray
{-# INLINE copyMutableArray #-}

readByteArray ::
  forall a m.
  CheckCtx =>
  PrimMonad m =>
  Prim a =>
  MutableByteArray (PrimState m) ->
  Int ->
  m a
readByteArray = checkIMBArray @a "readByteArray" undefined PA.readByteArray
{-# INLINE readByteArray #-}

writeByteArray ::
  forall a m.
  CheckCtx =>
  PrimMonad m =>
  Prim a =>
  MutableByteArray (PrimState m) ->
  Int ->
  a ->
  m ()
writeByteArray = checkIMBArray @a "writeByteArray" undefined PA.writeByteArray
{-# INLINE writeByteArray #-}

indexByteArray ::
  forall a.
  CheckCtx =>
  Prim a =>
  ByteArray ->
  Int ->
  a
indexByteArray = checkIBArray @a "indexByteArray" undefined PA.indexByteArray
{-# INLINE indexByteArray #-}

copyByteArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  ByteArray ->
  Int ->
  Int ->
  m ()
copyByteArray = checkCBArray "copyByteArray" PA.copyByteArray
{-# INLINE copyByteArray #-}

copyMutableByteArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  MutableByteArray (PrimState m) ->
  Int ->
  Int ->
  m ()
copyMutableByteArray = checkCMBArray "copyMutableByteArray" PA.copyMutableByteArray
{-# INLINE copyMutableByteArray #-}

moveByteArray ::
  CheckCtx =>
  PrimMonad m =>
  MutableByteArray (PrimState m) ->
  Int ->
  MutableByteArray (PrimState m) ->
  Int ->
  Int ->
  m ()
moveByteArray = checkCMBArray "moveByteArray" PA.moveByteArray
{-# INLINE moveByteArray #-}

readPrimArray ::
  CheckCtx =>
  PrimMonad m =>
  Prim a =>
  MutablePrimArray (PrimState m) a ->
  Int ->
  m a
readPrimArray = checkIMPArray "readPrimArray" PA.readPrimArray
{-# INLINE readPrimArray #-}

writePrimArray ::
  CheckCtx =>
  PrimMonad m =>
  Prim a =>
  MutablePrimArray (PrimState m) a ->
  Int ->
  a ->
  m ()
writePrimArray = checkIMPArray "writePrimArray" PA.writePrimArray
{-# INLINE writePrimArray #-}

indexPrimArray ::
  CheckCtx =>
  Prim a =>
  PrimArray a ->
  Int ->
  a
indexPrimArray = checkIPArray "indexPrimArray" PA.indexPrimArray
{-# INLINE indexPrimArray #-}
