{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The global exits table. Native code returns an index into it to say
-- what the interpreter should do next. See unison-runtime/src/Unison/Runtime/JIT/design.md.
module Unison.Runtime.JIT.Exits
  ( Exit (..),
    ExitIndex,
    registerExits,
    replaceExits,
    lookupExit,
    countExit,
    exitCounts,
    resetExitCounts,
    countEntry,
    nameEntry,
    entryCounts,
  )
where

import Control.Monad (forM, forM_)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Primitive.PrimArray (MutablePrimArray (..), newPrimArray, readPrimArray, setPrimArray)
import Data.Primitive.SmallArray (SmallMutableArray, copySmallMutableArray, getSizeofSmallMutableArray, newSmallArray, readSmallArray, writeSmallArray)
import Foreign.Ptr (Ptr, WordPtr, ptrToWordPtr)
import GHC.Exts (Int (..), RealWorld, fetchAddIntArray#)
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafePerformIO)
import Unison.Runtime.JIT.Config (config, stats)
import Unison.Runtime.MCode (CombIx, NativeCell)
import Unison.Runtime.Machine.Types (MInstr, MSection)

data Exit
  = -- | interpret from this section of this combinator
    Resume !CombIx !MSection
  | -- | grow the Unison stack to fit a frame of this size, then call the
    -- function whose cell this is again (it may not be the one the
    -- trampoline entered: tail calls change functions)
    GrowStack !Int !(Ptr NativeCell)
  | -- | the runtime asked the thread to stop; call the function whose
    -- cell this is again when the thread next runs
    Reenter !(Ptr NativeCell)
  | -- | run this instruction with the interpreter (it pushes the given
    -- number of values), then call the function in this cell, which
    -- continues with the section after it. If the instruction pushed a
    -- different number of values, interpret that section instead.
    CallOut !CombIx !MInstr !MSection !Int !(Ptr NativeCell)
  | -- | a description, for statistics
    Named String Exit

-- | 1-based; 0 is OK and negative values are errors.
type ExitIndex = Int

-- | The exits, by index, in an array that grows by copying. Looking one up
-- is on the path of every exit, so it is two loads. Only the thread that
-- compiles writes; an entry is in place before the code that returns its
-- index is installed, and a reader that still holds an older array finds
-- every index it can be handed there.
data Table = Table !Int !(SmallMutableArray RealWorld Exit) -- next index, exits

table :: IORef Table
table = unsafePerformIO (newIORef . Table 1 =<< newSmallArray 1024 unknown)
{-# NOINLINE table #-}

unknown :: Exit
unknown = error "JIT: unknown exit index"

-- | Adds a module's exits and returns the index of the first one. The
-- rest follow consecutively, in the order given.
registerExits :: [Exit] -> IO ExitIndex
registerExits exits = do
  Table next arr <- readIORef table
  size <- getSizeofSmallMutableArray arr
  let n = length exits
  arr' <-
    if next + n <= size
      then pure arr
      else do
        bigger <- newSmallArray (max (2 * size) (next + n)) unknown
        copySmallMutableArray bigger 0 arr 0 next
        pure bigger
  forM_ (zip [next ..] exits) $ \(i, e) -> writeSmallArray arr' i $! e
  writeIORef table (Table (next + n) arr')
  pure next

-- | Overwrites exits registered earlier, starting at the given index: the
-- second code generation pass produces the final ones (with the cells of
-- the auxiliary functions), the first only their number.
replaceExits :: ExitIndex -> [Exit] -> IO ()
replaceExits base exits = do
  Table _ arr <- readIORef table
  forM_ (zip [base ..] exits) $ \(i, e) -> writeSmallArray arr i $! e

lookupExit :: ExitIndex -> IO Exit
lookupExit i = do
  Table _ arr <- readIORef table
  readSmallArray arr i
{-# INLINE lookupExit #-}

-- | How often each exit was taken, for @UNISON_JIT_STATS@: one counter per
-- exit index, with the total in slot 0. Fixed in size so that counting is
-- one atomic add; exits past the end are counted together in the last
-- slot. Only allocated when statistics are on.
counters :: MutablePrimArray RealWorld Int
counters = unsafePerformIO $ do
  let n = if stats config then counterSlots else 1
  arr <- newPrimArray n
  setPrimArray arr 0 n 0
  pure arr
{-# NOINLINE counters #-}

counterSlots :: Int
counterSlots = 1024 * 1024

-- | Counts an exit; gives the total taken so far.
countExit :: ExitIndex -> IO Int
countExit i = do
  _ <- fetchAddInt counters (min i (counterSlots - 1)) 1
  (+ 1) <$> fetchAddInt counters 0 1

-- | Every exit that has been taken, with its count.
exitCounts :: IO [(ExitIndex, Exit, Int)]
exitCounts = do
  Table next arr <- readIORef table
  counts <- forM [1 .. min next counterSlots - 1] $ \i -> (,) i <$> readPrimArray counters i
  forM [(i, n) | (i, n) <- counts, n > 0] $ \(i, n) -> (\e -> (i, e, n)) <$> readSmallArray arr i

-- | Starts counting again from zero.
resetExitCounts :: IO ()
resetExitCounts = do
  Table next _ <- readIORef table
  setPrimArray counters 0 (min next counterSlots) 0

fetchAddInt :: MutablePrimArray RealWorld Int -> Int -> Int -> IO Int
fetchAddInt (MutablePrimArray arr) (I# i) (I# n) = IO $ \s -> case fetchAddIntArray# arr i n s of
  (# s', old #) -> (# s', I# old #)

-- | For @UNISON_JIT_STATS@: how often the trampoline entered each native
-- function (by code address), and what each is called. Slow (a map in an
-- IORef); statistics runs only.
entries :: IORef (M.Map WordPtr Int)
entries = unsafePerformIO (newIORef M.empty)
{-# NOINLINE entries #-}

entryNames :: IORef (M.Map WordPtr String)
entryNames = unsafePerformIO (newIORef M.empty)
{-# NOINLINE entryNames #-}

countEntry :: Ptr a -> IO ()
countEntry fn = atomicModifyIORef' entries (\m -> (M.insertWith (+) (ptrToWordPtr fn) 1 m, ()))

-- | Records the name of the function at a code address.
nameEntry :: Ptr a -> String -> IO ()
nameEntry fn name = atomicModifyIORef' entryNames (\m -> (M.insert (ptrToWordPtr fn) name m, ()))

-- | The entries counted since the last call, by function name.
entryCounts :: IO [(String, Int)]
entryCounts = do
  counts <- atomicModifyIORef' entries (\m -> (M.empty, m))
  names <- readIORef entryNames
  pure [(M.findWithDefault (show a) a names, n) | (a, n) <- M.toList counts]
