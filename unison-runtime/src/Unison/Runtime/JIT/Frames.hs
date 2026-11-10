-- | The frame table, one per JIT. A native caller whose callee exits writes a
-- frame record naming an entry here; the trampoline turns the record into
-- the @Push@ frame the interpreter would have pushed. See unison-runtime/src/Unison/Runtime/JIT/design.md,
-- "Frames: native code records, Haskell builds".
module Unison.Runtime.JIT.Frames
  ( Frame (..),
    FrameIndex,
    Frames,
    newFrames,
    registerFrames,
    replaceFrames,
    lookupFrame,
  )
where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimState)
import Data.IORef
import Data.Primitive.SmallArray (SmallMutableArray, copySmallMutableArray, getSizeofSmallMutableArray, newSmallArray, readSmallArray, writeSmallArray)
import Foreign.Ptr (Ptr)
import Unison.Runtime.MCode (CombIx, NativeCell)
import Unison.Runtime.Stack (MSection)

-- | The fixed parts of a @Push@ frame for one @Let@: the body's @CombIx@,
-- stack guard and section, and the cell holding the body's native code.
data Frame = Frame !CombIx !Int !MSection !(Ptr NativeCell)

-- | 1-based.
type FrameIndex = Int

-- | Like the exits table: an array that grows by copying, written only by
-- the thread that compiles.
data Table = Table !Int !(SmallMutableArray (PrimState IO) Frame)

newtype Frames = Frames (IORef Table)

newFrames :: IO Frames
newFrames = Frames <$> (newIORef . Table 1 =<< newSmallArray 1024 unknown)

unknown :: Frame
unknown = error "JIT: unknown frame index"

-- | Adds a module's frames and returns the index of the first one. The
-- rest follow consecutively, in the order given.
registerFrames :: Frames -> [Frame] -> IO FrameIndex
registerFrames (Frames table) frames = do
  Table next arr <- readIORef table
  size <- getSizeofSmallMutableArray arr
  let n = length frames
  arr' <-
    if next + n <= size
      then pure arr
      else do
        bigger <- newSmallArray (max (2 * size) (next + n)) unknown
        copySmallMutableArray bigger 0 arr 0 next
        pure bigger
  forM_ (zip [next ..] frames) $ \(i, f) -> writeSmallArray arr' i $! f
  writeIORef table (Table (next + n) arr')
  pure next

-- | Overwrites frames registered earlier, starting at the given index
-- (see replaceExits).
replaceFrames :: Frames -> FrameIndex -> [Frame] -> IO ()
replaceFrames (Frames table) base frames = do
  Table _ arr <- readIORef table
  forM_ (zip [base ..] frames) $ \(i, f) -> writeSmallArray arr i $! f

lookupFrame :: Frames -> FrameIndex -> IO Frame
lookupFrame (Frames table) i = do
  Table _ arr <- readIORef table
  readSmallArray arr i
