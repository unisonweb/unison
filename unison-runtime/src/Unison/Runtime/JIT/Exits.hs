{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The exits table, one per JIT (which is one per process, see JIT.hs). Native code returns an index into it to say
-- what the interpreter should do next. See unison-runtime/src/Unison/Runtime/JIT/design.md.
module Unison.Runtime.JIT.Exits
  ( Exit (..),
    ExitIndex,
    Exits,
    newExits,
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
import Control.Monad.Primitive (PrimState)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Primitive.PrimArray (MutablePrimArray (..), newPrimArray, readPrimArray, setPrimArray)
import Data.Primitive.SmallArray (SmallMutableArray, copySmallMutableArray, getSizeofSmallMutableArray, newSmallArray, readSmallArray, writeSmallArray)
import Foreign.Ptr (Ptr, WordPtr, ptrToWordPtr)
import GHC.Exts (Int (..), fetchAddIntArray#)
import GHC.IO (IO (..))
import Unison.Runtime.MCode (CombIx, NativeCell)
import Unison.Runtime.Stack (MInstr, MSection)

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
data Table = Table !Int !(SmallMutableArray (PrimState IO) Exit) -- next index, exits

-- | The table, the exit counters, and the entry counts (by code address)
-- with the names of the functions entered.
data Exits = Exits
  { exTable :: !(IORef Table),
    exCounters :: !(MutablePrimArray (PrimState IO) Int),
    exEntries :: !(IORef (M.Map WordPtr Int)),
    exEntryNames :: !(IORef (M.Map WordPtr String))
  }

-- | An empty table. With statistics on, the counters have a slot per exit
-- index (up to 'counterSlots'); otherwise one, since nothing counts.
newExits :: Bool -> IO Exits
newExits stats = do
  t <- newIORef . Table 1 =<< newSmallArray 1024 unknown
  let n = if stats then counterSlots else 1
  arr <- newPrimArray n
  setPrimArray arr 0 n 0
  Exits t arr <$> newIORef M.empty <*> newIORef M.empty

unknown :: Exit
unknown = error "JIT: unknown exit index"

-- | Adds a module's exits and returns the index of the first one. The
-- rest follow consecutively, in the order given.
registerExits :: Exits -> [Exit] -> IO ExitIndex
registerExits ex exits = do
  Table next arr <- readIORef (exTable ex)
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
  writeIORef (exTable ex) (Table (next + n) arr')
  pure next

-- | Overwrites exits registered earlier, starting at the given index: the
-- second code generation pass produces the final ones (with the cells of
-- the auxiliary functions), the first only their number.
replaceExits :: Exits -> ExitIndex -> [Exit] -> IO ()
replaceExits ex base exits = do
  Table _ arr <- readIORef (exTable ex)
  forM_ (zip [base ..] exits) $ \(i, e) -> writeSmallArray arr i $! e

lookupExit :: Exits -> ExitIndex -> IO Exit
lookupExit ex i = do
  Table _ arr <- readIORef (exTable ex)
  readSmallArray arr i
{-# INLINE lookupExit #-}

-- | How often each exit was taken, for @UNISON_JIT_STATS@: one counter per
-- exit index, with the total in slot 0. Fixed in size so that counting is
-- one atomic add; exits past the end are counted together in the last
-- slot. Only allocated when statistics are on.
counterSlots :: Int
counterSlots = 1024 * 1024

-- | Counts an exit; gives the total taken so far.
countExit :: Exits -> ExitIndex -> IO Int
countExit ex i = do
  _ <- fetchAddInt (exCounters ex) (min i (counterSlots - 1)) 1
  (+ 1) <$> fetchAddInt (exCounters ex) 0 1

-- | Every exit that has been taken, with its count.
exitCounts :: Exits -> IO [(ExitIndex, Exit, Int)]
exitCounts ex = do
  Table next arr <- readIORef (exTable ex)
  counts <- forM [1 .. min next counterSlots - 1] $ \i -> (,) i <$> readPrimArray (exCounters ex) i
  forM [(i, n) | (i, n) <- counts, n > 0] $ \(i, n) -> (\e -> (i, e, n)) <$> readSmallArray arr i

-- | Starts counting again from zero.
resetExitCounts :: Exits -> IO ()
resetExitCounts ex = do
  Table next _ <- readIORef (exTable ex)
  setPrimArray (exCounters ex) 0 (min next counterSlots) 0

fetchAddInt :: MutablePrimArray (PrimState IO) Int -> Int -> Int -> IO Int
fetchAddInt (MutablePrimArray arr) (I# i) (I# n) = IO $ \s -> case fetchAddIntArray# arr i n s of
  (# s', old #) -> (# s', I# old #)

-- | For @UNISON_JIT_STATS@: how often the trampoline entered each native
-- function (by code address), and what each is called. Slow (a map in an
-- IORef); statistics runs only.
countEntry :: Exits -> Ptr a -> IO ()
countEntry ex fn = atomicModifyIORef' (exEntries ex) (\m -> (M.insertWith (+) (ptrToWordPtr fn) 1 m, ()))

-- | Records the name of the function at a code address.
nameEntry :: Exits -> Ptr a -> String -> IO ()
nameEntry ex fn name = atomicModifyIORef' (exEntryNames ex) (\m -> (M.insert (ptrToWordPtr fn) name m, ()))

-- | The entries counted since the last call, by function name.
entryCounts :: Exits -> IO [(String, Int)]
entryCounts ex = do
  counts <- atomicModifyIORef' (exEntries ex) (\m -> (M.empty, m))
  names <- readIORef (exEntryNames ex)
  pure [(M.findWithDefault (show a) a names, n) | (a, n) <- M.toList counts]
