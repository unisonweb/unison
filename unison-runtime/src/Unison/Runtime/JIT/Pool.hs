-- | The constant pool: Haskell heap objects that native code needs, in an
-- array whose address is passed on every entry. See unison-runtime/src/Unison/Runtime/JIT/design.md,
-- "Appendix: the constant pool", which also says why there is one pool per
-- process rather than one per module.
--
-- Indices are assigned when a module is compiled ('poolIndices') and are
-- constants in the generated code. The array grows by copying; the old
-- arrays are kept, and new entries are written to them too where they
-- fit, so an address read at entry stays valid for as long as that native
-- run lasts. A run can still meet code that was installed after it began
-- and uses an index past the end of the array the run holds: a function
-- that uses an index of 'poolStableSize' or more checks the array's size
-- at entry, and exits to be entered again with the current array.
module Unison.Runtime.JIT.Pool
  ( Pool,
    newPool,
    PoolKey (..),
    poolIndices,
    currentPool,
    poolStableSize,
    poolIndexCharTag,
    poolIndexFloatTag,
    poolIndexIntTag,
    poolIndexNatTag,
    poolIndexTrue,
    poolIndexFalse,
  )
where

import Control.Monad (forM, forM_, when)
import Control.Monad.Primitive (PrimState)
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Primitive.Array (MutableArray, copyMutableArray, newArray, sizeofMutableArray, writeArray)
import Data.Vector qualified as V
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag)
import Unison.Runtime.MCode (CombIx, GCombInfo, MLit (..), RecordShape (..))
import Unison.Runtime.Stack (Closure, MComb, Val (..), charTypeTag, falseVal, floatTypeTag, intTypeTag, natTypeTag, nullSeg, trueVal, pattern Enum, pattern Foreign, pattern PAp, pattern RecordG)
import Unison.Runtime.Stack qualified as Stack

-- The order matches unboxedTypeTagToInt in Stack.hs.
poolIndexCharTag, poolIndexFloatTag, poolIndexIntTag, poolIndexNatTag :: Int
poolIndexCharTag = 0
poolIndexFloatTag = 1
poolIndexIntTag = 2
poolIndexNatTag = 3

-- | The boolean closures, for comparison results.
poolIndexTrue, poolIndexFalse :: Int
poolIndexTrue = 4
poolIndexFalse = 5

fixedEntries :: Int
fixedEntries = 6

-- | The size of the first array: every array ever passed to native code
-- has at least this many entries.
-- | A pool, with the size its first array has: a function that uses only
-- indices below it never has to check the array it was given (the stress
-- mode @pool=N@ makes it tiny, so that growing is exercised).
data Pool = Pool !(IORef PoolState) !Int

poolStableSize :: Pool -> Int
poolStableSize (Pool _ n) = n

-- | What a pool entry is. Entries are interned by key.
data PoolKey
  = -- | a constructor with no fields. Also how a 'Reference' is kept: the
    -- entry for @KeyEnum r 0@ is an 'Enum' whose reference field native
    -- code reads when it builds a constructor of that type.
    KeyEnum !Reference !PackedTag
  | -- | a boxed literal (text, term link, type link)
    KeyLit !MLit
  | -- | a known combinator as a value: a @PAp@ with nothing captured, the
    -- same closure the interpreter builds for @App (Env cix) ZArgs@.
    -- Interned by the CombIx alone (the combinator has no Ord).
    KeyComb !CombIx !(GCombInfo MComb)
  | -- | the boxed part of a top-level value that was evaluated when its
    -- definition was loaded (a @CachedVal@), by its CombIx
    KeyCached !CombIx !Closure
  | -- | a record shape, kept as an empty record of that shape: native code
    -- copies the shape's words out of it when it builds a record
    -- (@RecPack@). Interned by the field names, which determine the shape.
    KeyRecord !RecordShape

instance Eq PoolKey where
  a == b = compare a b == EQ

instance Ord PoolKey where
  compare = \cases
    (KeyEnum r t) (KeyEnum r' t') -> compare (r, t) (r', t')
    (KeyEnum {}) _ -> LT
    _ (KeyEnum {}) -> GT
    (KeyLit l) (KeyLit l') -> compare l l'
    (KeyLit {}) _ -> LT
    _ (KeyLit {}) -> GT
    (KeyComb c _) (KeyComb c' _) -> compare c c'
    (KeyComb {}) _ -> LT
    _ (KeyComb {}) -> GT
    (KeyCached c _) (KeyCached c' _) -> compare c c'
    (KeyCached {}) _ -> LT
    _ (KeyCached {}) -> GT
    (KeyRecord s) (KeyRecord s') -> compare s s'

instance Show PoolKey where
  show = \case
    KeyEnum r t -> "KeyEnum " ++ show r ++ " " ++ show t
    KeyLit l -> "KeyLit " ++ show l
    KeyComb c _ -> "KeyComb " ++ show c
    KeyCached c _ -> "KeyCached " ++ show c
    KeyRecord s -> "KeyRecord " ++ show (shapeFields s)

data PoolState = PoolState
  { poolArray :: !(MutableArray (PrimState IO) Closure),
    poolNext :: !Int,
    poolKeys :: !(Map.Map PoolKey Int),
    -- | superseded arrays, kept alive on purpose
    poolOld :: [MutableArray (PrimState IO) Closure]
  }

-- | A pool holding the fixed entries. The argument is the stress mode's
-- size for the first array, if any.
newPool :: Maybe Int -> IO Pool
newPool stress = do
  let stable = max fixedEntries (fromMaybe 4096 stress)
  -- Everything is forced before it goes in: native code reads these
  -- objects' fields directly, so they must be values, not thunks.
  arr <- newArray stable $! natTypeTag
  let put i c = writeArray arr i $! c
  put poolIndexCharTag charTypeTag
  put poolIndexFloatTag floatTypeTag
  put poolIndexIntTag intTypeTag
  put poolIndexNatTag natTypeTag
  put poolIndexTrue (getBoxedVal trueVal)
  put poolIndexFalse (getBoxedVal falseVal)
  ref <- newIORef (PoolState arr fixedEntries Map.empty [])
  pure (Pool ref stable)

-- | The array to pass to native code now.
currentPool :: Pool -> IO (MutableArray (PrimState IO) Closure)
currentPool (Pool ref _) = poolArray <$> readIORef ref

-- | The index of each key, adding the ones not seen before. Called when
-- a module is compiled, before its code is generated.
poolIndices :: Pool -> [PoolKey] -> IO (Map.Map PoolKey Int)
poolIndices (Pool ref _) keys = do
  ixs <- forM keys $ \k -> do
    p <- readIORef ref
    case Map.lookup k (poolKeys p) of
      Just i -> pure (k, i)
      Nothing -> do
        p <- grow p
        let i = poolNext p
            !c = closureFor k
        writeArray (poolArray p) i c
        -- a native run that began before the array last grew holds an old one
        forM_ (poolOld p) $ \old -> when (i < sizeofMutableArray old) (writeArray old i c)
        writeIORef ref p {poolNext = i + 1, poolKeys = Map.insert k i (poolKeys p)}
        pure (k, i)
  pure (Map.fromList ixs)
  where
    grow p
      | poolNext p < sizeofMutableArray (poolArray p) = pure p
      | otherwise = do
          let old = poolArray p
              n = sizeofMutableArray old
          new <- newArray (2 * n) $! natTypeTag
          copyMutableArray new 0 old 0 n
          pure p {poolArray = new, poolOld = old : poolOld p}

closureFor :: PoolKey -> Closure
closureFor = \case
  KeyEnum r t -> Enum r t
  -- the segment's boxes must be evaluated (native code reads through
  -- them); nullSeg's components are CAFs, so force them
  KeyComb cix comb -> let (u, b) = nullSeg in u `seq` b `seq` PAp cix comb (u, b)
  KeyCached _ c -> c
  KeyRecord s -> RecordG s V.empty
  KeyLit l -> case l of
    MT t -> Stack.TextC t
    MM r -> Foreign (Stack.WrapReferent r)
    MY r -> Foreign (Stack.WrapReference r)
    _ -> error "closureFor: unboxed literal"
