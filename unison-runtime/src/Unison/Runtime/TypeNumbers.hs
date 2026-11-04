{-# LANGUAGE BangPatterns #-}

-- | The process-wide numbering of types.
--
-- A data constructor closure carries a packed tag: the number of its type
-- in the high bits, the constructor's tag in the low ('Unison.Runtime.TypeTags').
-- The number stands for the type's 'Reference' wherever that is needed: to
-- order values of different types ('Unison.Runtime.Stack.compareClosure'),
-- to write a value out ('Unison.Runtime.Machine.reflectValue') or to show
-- it ('Unison.Runtime.Decompile'). The comparison runs in pure code
-- (@Ord Val@, the key order of a replaced @Map@) and has no cache in hand,
-- and a value built by one code cache can meet a value built by another
-- (ucm runs an ordinary and a sandboxed runtime), so the numbering is one
-- per process, here, and not a field of the cache.
--
-- Numbers are never reused and an entry never changes once written, which
-- is what makes the pure read ('typeReference') sound: a closure carrying
-- number @n@ exists only after @n@ was published, which is after its slot
-- was written. A number reaches another thread inside a closure or a
-- compiled group, through whatever synchronization handed those over.
--
-- Writes happen when code is loaded, rarely, and never block a reader:
-- the number-to-reference direction is a directory of fixed chunks that
-- are allocated on demand and never moved, read with two loads; the
-- reference-to-number direction is an immutable map swapped atomically.
--
-- The builtin types have fixed numbers ('builtinTypeNumbering'), the same
-- in every process; the compiled code of a program carries the numbering
-- it was compiled against, and the process that runs it seeds the registry
-- from that ('seedTypeNumbers') before loading anything.
module Unison.Runtime.TypeNumbers
  ( typeNumbers,
    typeNumber,
    typeReference,
    typeReferences,
    typeNumberMap,
    seedTypeNumbers,
  )
where

import Control.Exception (evaluate)
import Control.Monad.ST (RealWorld)
import Data.Bits (shiftR, (.&.))
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Primitive.Array
import Data.Primitive.SmallArray
import Data.Set qualified as Set
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.Builtin.Types (builtinTypeNumbering)
import Unison.Runtime.InternalError (internalBug)
import Unison.Util.EnumContainers (EnumMap)
import Unison.Util.EnumContainers qualified as EC

-- | Both directions, swapped together so that a snapshot is consistent.
data Maps = Maps !(Map Reference Word64) !(EnumMap Word64 Reference)

data Registry = Registry
  { -- | the next number to hand out
    next :: !(IORef Word64),
    -- | number to reference, in chunks of 'chunkSize'; an entry is
    -- 'Nothing' until the first number in its chunk is assigned
    directory :: !(SmallArray (IORef (Maybe (MutableArray RealWorld Reference)))),
    maps :: !(IORef Maps)
  }

chunkBits :: Int
chunkBits = 12

chunkSize :: Int
chunkSize = 4096

-- | 1024 chunks of 4096: four million types, far beyond what a process
-- could load.
directorySize :: Int
directorySize = 1024

registry :: Registry
registry = unsafePerformIO do
  next <- newIORef (1 + maximum builtinTypeNumbering)
  directory <- smallArrayFromListN directorySize <$> replicateM directorySize (newIORef Nothing)
  maps <- newIORef (Maps Map.empty mempty)
  let reg = Registry next directory maps
  for_ (Map.toList builtinTypeNumbering) \(r, n) -> assign reg n r
  pure reg
{-# NOINLINE registry #-}

-- | The chunk that holds number @n@, allocated if it was not.
chunkFor :: Registry -> Word64 -> IO (MutableArray RealWorld Reference)
chunkFor reg n
  | ci >= directorySize = internalBug [] ("TypeNumbers: number out of range: " ++ show n)
  | otherwise =
      readIORef ref >>= \case
        Just c -> pure c
        Nothing -> do
          c <- newArray chunkSize (internalBug [] ("TypeNumbers: unassigned type number near " ++ show n))
          atomicModifyIORef' ref \case
            Just c' -> (Just c', c')
            Nothing -> (Just c, c)
  where
    ci = fromIntegral (n `shiftR` chunkBits)
    ref = indexSmallArray (directory reg) ci

-- | Writes slot @n@, then publishes the entry. Returns the number the
-- reference has afterwards: @n@, or the one another thread published
-- first, in which case slot @n@ stays a hole. Dies if @n@ was already
-- published for another reference, which only a seed can attempt.
assign :: Registry -> Word64 -> Reference -> IO Word64
assign reg n r = do
  c <- chunkFor reg n
  writeArray c (fromIntegral n .&. (chunkSize - 1)) r
  outcome <- atomicModifyIORef' (maps reg) \ms@(Maps fwd bwd) -> case Map.lookup r fwd of
    Just n' -> (ms, Right n')
    Nothing -> case EC.lookup n bwd of
      Just r' -> (ms, Left r')
      Nothing -> (Maps (Map.insert r n fwd) (EC.mapInsert n r bwd), Right n)
  case outcome of
    Right n' -> pure n'
    Left r' -> internalBug [] ("TypeNumbers: number " ++ show n ++ " is " ++ show r' ++ ", cannot seed it as " ++ show r)

-- | Numbers the references that have none yet; the numbers of them all.
typeNumbers :: Set Reference -> IO (Map Reference Word64)
typeNumbers rs = do
  Maps fwd _ <- readIORef (maps registry)
  let new = Set.filter (`Map.notMember` fwd) rs
  assigned <- for (Set.toList new) \r -> do
    n <- atomicModifyIORef' (next registry) \i -> (i + 1, i)
    (r,) <$> assign registry n r
  pure (Map.fromList assigned <> Map.restrictKeys fwd rs)

-- | The number of one reference, assigned if it had none.
typeNumber :: Reference -> IO Word64
typeNumber r = do
  Maps fwd _ <- readIORef (maps registry)
  case Map.lookup r fwd of
    Just n -> pure n
    Nothing -> (Map.! r) <$> typeNumbers (Set.singleton r)

-- | The reference a number stands for. Pure: an entry never changes once
-- written, and a number is only ever read out of something built after
-- the entry was published (see the module comment). A number never
-- assigned is an internal error.
typeReference :: Word64 -> Reference
typeReference n = unsafeDupablePerformIO do
  readIORef (indexSmallArray (directory registry) (fromIntegral (n `shiftR` chunkBits))) >>= \case
    Just c -> readArray c (fromIntegral n .&. (chunkSize - 1)) >>= evaluate
    Nothing -> internalBug [] ("TypeNumbers: unassigned type number " ++ show n)
{-# NOINLINE typeReference #-}

-- | Every number assigned so far, with its reference.
typeReferences :: IO (EnumMap Word64 Reference)
typeReferences = (\(Maps _ bwd) -> bwd) <$> readIORef (maps registry)

-- | Every reference numbered so far, with its number.
typeNumberMap :: IO (Map Reference Word64)
typeNumberMap = (\(Maps fwd _) -> fwd) <$> readIORef (maps registry)

-- | Takes on a numbering made elsewhere: a compiled program's. Each entry
-- must agree with what this process has, if it has it; the counter moves
-- past the largest number.
seedTypeNumbers :: EnumMap Word64 Reference -> IO ()
seedTypeNumbers m = do
  for_ (EC.mapToList m) \(n, _) ->
    atomicModifyIORef' (next registry) \i -> (max i (n + 1), ())
  for_ (EC.mapToList m) \(n, r) -> do
    n' <- assign registry n r
    when (n' /= n) $
      internalBug [] ("TypeNumbers: " ++ show r ++ " is number " ++ show n' ++ " here, cannot seed it as " ++ show n)
