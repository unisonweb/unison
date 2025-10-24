{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UnboxedTuples #-}

module Unison.Runtime.Stack
  ( K (..),
    GClosure (..),
    Closure
      ( ..,
        DataC,
        PApV,
        CapV,
        PAp,
        Enum,
        Data1,
        Data2,
        DataG,
        Captured,
        Foreign,
        Affine,
        BlackHole,
        UnboxedTypeTag
      ),
    AffineRef (..),
    AEnv,
    DEnv,
    HEnv (..),
    closureTag,
    formDataReplaced,
    unitClosure,
    UnboxedTypeTag (..),
    unboxedTypeTagToInt,
    unboxedTypeTagFromInt,
    IxClosure,
    Callback (..),
    Augment (..),
    Dump (..),
    Stack (..),
    XStack,
    pattern XStack,
    packXStack,
    unpackXStack,
    xStackIOToIO,
    stackIOToIOX,
    estackIOToIOX,
    exStackIOToIO,
    IOXStack,
    IOEXStack,
    apX,
    fpX,
    spX,
    ustkX,
    bstkX,
    Off,
    SZ,
    FP,
    Seg,
    USeg,
    BSeg,
    SegList,
    Val
      ( ..,
        CharVal,
        NatVal,
        DoubleVal,
        IntVal,
        BoolVal,
        UnboxedVal,
        BoxedVal
      ),
    emptyVal,
    falseVal,
    trueVal,
    boxedVal,
    USeq,
    traceK,
    frameDataSize,
    RuntimePanic (..),
    marshalToForeign,
    marshalUnwrapForeignIO,
    unull,
    bnull,
    nullSeg,
    peekD,
    peekOffD,
    peekC,
    peekOffC,
    poke,
    pokeD,
    pokeOffD,
    pokeC,
    pokeOffC,
    pokeBool,
    pokeTag,
    peekTag,
    peekTagOff,
    peekI,
    peekOffI,
    peekN,
    peekOffN,
    pokeN,
    pokeOffN,
    pokeI,
    pokeOffI,
    pokeByte,
    peekBi,
    peekOffBi,
    pokeBi,
    pokeOffBi,
    peekBool,
    peekOffBool,
    peekOffS,
    pokeS,
    pokeOffS,
    frameView,
    scount,
    closureTermRefs,
    dumpAP,
    dumpFP,
    alloc,
    peek,
    upeek,
    bpeek,
    peekOff,
    upeekOff,
    bpeekOff,
    bpoke,
    bpokeOff,
    pokeOff,
    upokeT,
    upokeOffT,
    unsafePokeIasN,
    bump,
    bumpn,
    grabSeg,
    truncateSeg,
    ensure,
    duplicate,
    discardFrame,
    saveFrame,
    saveArgs,
    restoreFrame,
    prepareArgs,
    acceptArgs,
    frameArgs,
    augSeg,
    dumpSeg,
    adjustArgs,
    fsize,
    asize,
    useg,
    bseg,
    segFromList,
    traverseListToSeg,
    traverseAccumSegToList,

    -- * Unboxed type tags
    natTypeTag,
    intTypeTag,
    charTypeTag,
    floatTypeTag,
    hasNoAllocations,
    universalEq,
    universalCompare,
    -- pseudo data stuff
    inflateMap,
    deflateMap,
  )
where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate, throw, throwIO)
import Control.Monad.Primitive
import Control.Monad.State.Strict (StateT (..))
import Data.Atomics qualified as Atomic
import Data.Bits (clearBit)
import Data.Char qualified as Char
import Data.Functor.Classes (Eq1 (..), Ord1 (..))
import Data.IORef (IORef)
import Data.Map.Strict.Internal (Map (..))
import Data.Ord (comparing)
import Data.Primitive (sizeOf)
import Data.Primitive.ByteArray qualified as BA
import Data.Sequence qualified as Sq
import Data.Tagged (Tagged (..))
import Data.Word
import GHC.Base
import GHC.Exts as L (IsList (..))
import Language.Haskell.TH qualified as TH
import Test.Inspection qualified as TI
import Unison.Builtin.Decls as Ty
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.ANF (PackedTag, maskTags)
import Unison.Runtime.Array as PA
import Unison.Runtime.Foreign
import Unison.Runtime.MCode
import Unison.Runtime.TypeTags qualified as TT
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.RefPromise (Promise)
import Prelude hiding (words)

#ifdef STACK_CHECK
type DebugCallStack = (HasCallStack :: Constraint)

unboxedSentinel :: Int
unboxedSentinel = -99

boxedSentinel :: Closure
boxedSentinel = (Closure GUnboxedSentinel)

assertBumped :: HasCallStack => Stack -> Off -> IO ()
assertBumped (Stack _ _ sp ustk bstk) i = do
  u <- readByteArray ustk (sp - i)
  b :: BVal <- readArray bstk (sp - i)
  when (u /= unboxedSentinel || not (isBoxedSentinel b)) do
            error $ "Expected stack slot to have been bumped, but it was:" <> show (Val u b)
  where
    isBoxedSentinel :: Closure -> Bool
    isBoxedSentinel (Closure GUnboxedSentinel) = True
    isBoxedSentinel _ = False

assertUnboxed :: HasCallStack => Stack -> Off -> IO ()
assertUnboxed (Stack _ _ sp ustk bstk) i = do
  (u :: Int) <- readByteArray ustk (sp - i)
  b <- readArray bstk (sp - i)
  case b of
    UnboxedTypeTag _ -> pure ()
    _ -> error $ "Expected stack val to be unboxed, but it was:" <> show (Val u b)

pokeSentinelOff :: Stack -> Off -> IO ()
pokeSentinelOff (Stack _ _ sp ustk bstk) off = do
  writeByteArray ustk (sp - off) unboxedSentinel
  writeArray bstk (sp - off) boxedSentinel
#else
-- Don't track callstacks in production, it's expensive
type DebugCallStack = (() :: Constraint)
#endif

newtype Callback = Hook (XStack -> IO ())

instance Eq Callback where _ == _ = True

instance Ord Callback where compare _ _ = EQ

-- Evaluation stack
data K
  = KE
  | -- callback hook
    CB Callback
  | -- mark continuation with affine prompt
    AMark
      !Int -- pending args
      AEnv -- saved handler environment; intentionally lazy
      !AffineRef -- updateable reference for handler
      !K
  | -- mark continuation with a prompt
    Mark
      !Int -- pending args
      !(EnumSet Word64)
      DEnv -- saved shadowed handlers; intentionally lazy
      !K
  | -- save information about a frame for later resumption
    Push
      !Int -- frame size
      !Int -- pending args
      !CombIx -- resumption section reference
      !Int -- stack guard
      !(RSection Val) -- resumption section
      !K
  | -- saved context during affine handler
    Local
      HEnv -- stored environment; intentionally lazy
      !Int -- pending args
      !K

newtype Closure = Closure {unClosure :: (GClosure (RComb Val))}
  deriving stock (Show)

-- A handler is 'affine' if its action does not change the structure
-- of the stack except possibly by truncation to that handler. The two
-- scenarios that satisfy this are:
--
--   1. Exception-like handlers that never resume the continuation
--      (this is the truncation case).
--   2. Handlers that call the continuation _in tail position_ and
--      also _with an (affine) handler for the same abilities_. The
--      simplest case is when a handler calls itself recursively to
--      implement a "deep" handler.
--
-- The advantage of affine handlers is that they do not need to be
-- implemented by continuation capture. Case 1 can be implemented by
-- simply _discarding_ the continuation. For case 2, as long as all
-- handlers are affine, it is sufficient to simply keep track of the
-- current state of each handler, and the local environment the
-- handler executes in. The restrictions ensure that these don't
-- change in an arbitrary way—just by stateful updates.
--
-- Non-affine handlers spoil this when they are higher in the stack,
-- because they could change the dynamic environment of handlers below
-- them, and it is no longer simple to properly update the state in
-- place. Possibly this could be handled by modifying affine handler
-- state when reinstating copied continuations in the future.
--
-- If we arrange things such that we use affine versions of handlers
-- until a non-affine one is installed, then we can avoid affine
-- handlers ever being captured in a continuation. This lets us avoid
-- issues with equality of mutable references for efficient affine
-- implementation.
--
-- The calling convention for affine handlers takes an extra argument
-- which enables using associated operations.
type AEnv = EnumMap Word64 AffineRef

-- dynamic environment
type DEnv = EnumMap Word64 Val

-- Handler environment.
--
-- Note: the fields are intentionally not strict. This seems to yield
-- better performance. At a guess, strict fields and being strict in
-- the HEnv requires GHC to emit forcing instructions that cause
-- overhead.
--
-- Instead, components are passed `evaluate` locally when built, or
-- similar.
data HEnv = HEnv {aenv :: AEnv, denv :: DEnv}

instance Semigroup HEnv where
  HEnv la ld <> HEnv ra rd = HEnv (la <> ra) (ld <> rd)

instance Monoid HEnv where
  mempty = HEnv mempty mempty
  mappend = (<>)

-- | Implementation for Unison sequences.
type USeq = Seq Val

type IxClosure = GClosure CombIx

-- Don't re-order these, the ord instance affects Universal.compare
data UnboxedTypeTag
  = CharTag
  | FloatTag
  | IntTag
  | NatTag
  deriving stock (Show, Eq, Ord)

unboxedTypeTagToInt :: UnboxedTypeTag -> Int
unboxedTypeTagToInt = \case
  CharTag -> 0
  FloatTag -> 1
  IntTag -> 2
  NatTag -> 3

unboxedTypeTagFromInt :: (HasCallStack) => Int -> UnboxedTypeTag
unboxedTypeTagFromInt = \case
  0 -> CharTag
  1 -> FloatTag
  2 -> IntTag
  3 -> NatTag
  _ -> error "intToUnboxedTypeTag: invalid tag"

data GClosure comb
  = GPAp
      !CombIx
      {-# UNPACK #-} !(GCombInfo comb)
      -- | args
      {-# UNPACK #-} !Seg
  | GEnum !Reference !PackedTag
  | GData1 !Reference !PackedTag !Val
  | GData2 !Reference !PackedTag !Val !Val
  | GDataG !Reference !PackedTag {-# UNPACK #-} !Seg
  | GCaptured
      -- | code cont
      !K
      -- | arg size
      !Int
      -- | u/b data stacks
      {-# UNPACK #-} !Seg
  | GForeign !Foreign
  | -- | The type tag for the value in the corresponding unboxed stack slot.
    --
    --   We should consider adding separate constructors for common builtin type tags.
    --   GHC will optimize nullary constructors into singletons.
    GUnboxedTypeTag !UnboxedTypeTag
  | GAffine
      -- | associated ability numbers
      !(EnumSet Word64)
      -- | original handler environment
      !AEnv
      -- | updateable reference
      !AffineRef
  | GBlackHole
#ifdef STACK_CHECK
  | GUnboxedSentinel
#endif

-- These derived instances are standalone to avoid needing to disable Ormolu for the data declaration above.

deriving stock instance (Show comb) => Show (GClosure comb)

deriving stock instance Functor GClosure

deriving stock instance Foldable GClosure

deriving stock instance Traversable GClosure

-- Wrap IORef to get a trivial `Show` instance
newtype AffineRef = ARef (IORef Closure) deriving (Eq)

instance Show AffineRef where
  show _ = "<AffineRef>"

-- Singleton black hole value to avoid allocation.
blackHole :: Closure
blackHole = Closure GBlackHole
{-# NOINLINE blackHole #-}

pattern PAp :: CombIx -> GCombInfo (RComb Val) -> Seg -> Closure
pattern PAp cix comb seg = Closure (GPAp cix comb seg)

pattern Enum :: Reference -> PackedTag -> Closure
pattern Enum r t = Closure (GEnum r t)

pattern Data1 r t i = Closure (GData1 r t i)

pattern Data2 r t i j = Closure (GData2 r t i j)

pattern DataG r t seg = Closure (GDataG r t seg)

pattern Captured k a seg = Closure (GCaptured k a seg)

pattern Foreign x = Closure (GForeign x)

pattern Affine ps aenv r = Closure (GAffine ps aenv r)

pattern BlackHole <- Closure GBlackHole
  where
    BlackHole = blackHole

pattern UnboxedTypeTag t <- Closure (GUnboxedTypeTag t)
  where
    UnboxedTypeTag t = case t of
      CharTag -> charTypeTag
      FloatTag -> floatTypeTag
      IntTag -> intTypeTag
      NatTag -> natTypeTag

{-# COMPLETE PAp, Enum, Data1, Data2, DataG, Captured, Foreign, UnboxedTypeTag, BlackHole, Affine #-}

{-# COMPLETE DataC, PAp, Captured, Foreign, BlackHole, UnboxedTypeTag, Affine #-}

{-# COMPLETE DataC, PApV, Captured, Foreign, BlackHole, UnboxedTypeTag, Affine #-}

{-# COMPLETE DataC, PApV, CapV, Foreign, BlackHole, UnboxedTypeTag, Affine #-}

-- We can avoid allocating a closure for common type tags on each poke by having shared top-level closures for them.
natTypeTag :: Closure
natTypeTag = (Closure (GUnboxedTypeTag NatTag))
{-# NOINLINE natTypeTag #-}

intTypeTag :: Closure
intTypeTag = (Closure (GUnboxedTypeTag IntTag))
{-# NOINLINE intTypeTag #-}

charTypeTag :: Closure
charTypeTag = (Closure (GUnboxedTypeTag CharTag))
{-# NOINLINE charTypeTag #-}

floatTypeTag :: Closure
floatTypeTag = (Closure (GUnboxedTypeTag FloatTag))
{-# NOINLINE floatTypeTag #-}

traceK :: Reference -> K -> [(Reference, Int)]
traceK begin = dedup (begin, 1)
  where
    dedup p (Mark _ _ _ k) = dedup p k
    dedup p@(cur, n) (Push _ _ (CIx r _ _) _ _ k)
      | cur == r = dedup (cur, 1 + n) k
      | otherwise = p : dedup (r, 1) k
    dedup p _ = [p]

splitData :: Closure -> Maybe (Reference, PackedTag, SegList)
splitData = \case
  (Enum r t) -> Just (r, t, [])
  (Data1 r t u) -> Just (r, t, [u])
  (Data2 r t i j) -> Just (r, t, [i, j])
  (DataG r t seg) -> Just (r, t, segToList seg)
  _ -> Nothing

closureTag :: Closure -> PackedTag
closureTag (Enum _ t) = t
closureTag (Data1 _ t _) = t
closureTag (Data2 _ t _ _) = t
closureTag (DataG _ t _) = t
closureTag c =
  throw $ Panic "closureTag: unexpected closure" (Just $ BoxedVal c)
{-# INLINE closureTag #-}

-- | Converts a list of integers representing an unboxed segment back into the
-- appropriate segment. Segments are stored backwards in the runtime, so this
-- reverses the list.
useg :: [Int] -> USeg
useg ws = case L.fromList $ reverse ws of
  PrimArray ba -> ByteArray ba

-- | Converts a boxed segment to a list of closures. The segments are stored
-- backwards, so this reverses the contents.
bsegToList :: BSeg -> [Closure]
bsegToList = reverse . L.toList

-- | Converts a list of closures back to a boxed segment. Segments are stored
-- backwards, so this reverses the contents.
bseg :: [Closure] -> BSeg
bseg = L.fromList . reverse

formData :: Reference -> PackedTag -> SegList -> Closure
formData r t [] = Enum r t
formData r t [v1] = Data1 r t v1
formData r t [v1, v2] = Data2 r t v1 v2
formData r t segList = DataG r t (segFromList segList)

formDataSeg :: Reference -> PackedTag -> Seg -> Closure
formDataSeg r t (usg, bsg) = case sizeofArray bsg of
  0 -> Enum r t
  1 -> Data1 r t (Val (indexByteArray usg 0) (indexArray bsg 0))
  2 ->
    Data2
      r
      t
      (Val (indexByteArray usg 1) (indexArray bsg 1))
      (Val (indexByteArray usg 0) (indexArray bsg 0))
  _ -> DataG r t (usg, bsg)
{-# INLINE formDataSeg #-}

-- Build a data type, but apply replacements
formDataReplaced :: Reference -> PackedTag -> Seg -> Closure
formDataReplaced r t sg@(usg, bsg)
  | t == TT.mapTipTag = case sizeofArray bsg of
      0 -> tipClosure
      _ -> error "formDataReplaced: bad `Map`"
  | t == TT.mapBinTag = case sizeofArray bsg of
      5
        | !bk <- indexArray bsg 3,
          !uk <- indexByteArray usg 3,
          !bv <- indexArray bsg 2,
          !uv <- indexByteArray usg 2,
          Foreign l <- indexArray bsg 1,
          Just (ul :: Map Val Val) <- maybeUnwrapBuiltin l,
          Foreign r <- indexArray bsg 0,
          Just (ur :: Map Val Val) <- maybeUnwrapBuiltin r,
          sz <- indexByteArray usg 4,
          Closure (GUnboxedTypeTag NatTag) <- indexArray bsg 4 ->
            Foreign . Wrap Ty.hmapRef $
              Bin sz (Val uk bk) (Val uv bv) ul ur
      _ -> error "formDataReplaced: bad `Map`"
  | otherwise = formDataSeg r t sg

tipClosure :: Closure
tipClosure = Foreign $ Wrap Ty.hmapRef Tip

frameDataSize :: K -> Int
frameDataSize = go 0
  where
    go sz KE = sz
    go sz (CB _) = sz
    go sz (Mark a _ _ k) = go (sz + a) k
    go sz (Push f a _ _ _ k) =
      go (sz + f + a) k
    go _ (Local {}) =
      error "frameDataSize: captured Local frame"
    go _ (AMark {}) =
      error "frameDataSize: captured AMark frame"

pattern DataC :: Reference -> PackedTag -> SegList -> Closure
pattern DataC rf ct segs <-
  (splitData -> Just (rf, ct, segs))
  where
    DataC rf ct segs = formData rf ct segs

matchCharVal :: Val -> Maybe Char
matchCharVal = \case
  (UnboxedVal u CharTag) -> Just (Char.chr u)
  _ -> Nothing

pattern CharVal :: Char -> Val
pattern CharVal c <- (matchCharVal -> Just c)
  where
    CharVal c = Val (Char.ord c) charTypeTag

matchNatVal :: Val -> Maybe Word64
matchNatVal = \case
  (UnboxedVal u NatTag) -> Just (fromIntegral u)
  _ -> Nothing

pattern NatVal :: Word64 -> Val
pattern NatVal n <- (matchNatVal -> Just n)
  where
    NatVal n = Val (fromIntegral n) natTypeTag

matchDoubleVal :: Val -> Maybe Double
matchDoubleVal = \case
  (UnboxedVal u FloatTag) -> Just (intToDouble u)
  _ -> Nothing

pattern DoubleVal :: Double -> Val
pattern DoubleVal d <- (matchDoubleVal -> Just d)
  where
    DoubleVal d = Val (doubleToInt d) floatTypeTag

matchIntVal :: Val -> Maybe Int
matchIntVal = \case
  (UnboxedVal u IntTag) -> Just u
  _ -> Nothing

pattern IntVal :: Int -> Val
pattern IntVal i <- (matchIntVal -> Just i)
  where
    IntVal i = Val i intTypeTag

matchBoolVal :: Val -> Maybe Bool
matchBoolVal = \case
  (BoxedVal (Enum r t)) | r == Ty.booleanRef -> Just (t == TT.trueTag)
  _ -> Nothing

pattern BoolVal :: Bool -> Val
pattern BoolVal b <- (matchBoolVal -> Just b)
  where
    BoolVal b = if b then trueVal else falseVal

-- Define singletons we can use for the bools to prevent allocation where possible.
falseVal :: Val
falseVal = BoxedVal (Enum Ty.booleanRef TT.falseTag)
{-# NOINLINE falseVal #-}

trueVal :: Val
trueVal = BoxedVal (Enum Ty.booleanRef TT.trueTag)
{-# NOINLINE trueVal #-}

doubleToInt :: Double -> Int
doubleToInt d = indexByteArray (BA.byteArrayFromList [d]) 0
{-# INLINE doubleToInt #-}

intToDouble :: Int -> Double
intToDouble w = indexByteArray (BA.byteArrayFromList [w]) 0
{-# INLINE intToDouble #-}

type SegList = [Val]

pattern PApV :: CombIx -> RCombInfo Val -> SegList -> Closure
pattern PApV cix rcomb segs <-
  PAp cix rcomb (segToList -> segs)
  where
    PApV cix rcomb segs = PAp cix rcomb (segFromList segs)

pattern CapV :: K -> Int -> SegList -> Closure
pattern CapV k a segs <- Captured k a (segToList -> segs)
  where
    CapV k a segList = Captured k a (segFromList segList)

-- | Converts from the efficient stack form of a segment to the list representation. Segments are stored backwards,
-- so this reverses the contents
segToList :: Seg -> SegList
segToList (u, b) =
  zipWith Val (ints u) (bsegToList b)

-- | Converts an unboxed segment to a list of integers for a more interchangeable
-- representation. The segments are stored in backwards order, so this reverses
-- the contents.
ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [n - 1, n - 2 .. 0]
  where
    n = sizeofByteArray ba `div` 8

-- | Converts from the list representation of a segment to the efficient stack form. Segments are stored backwards,
-- so this reverses the contents.
segFromList :: SegList -> Seg
segFromList xs =
  xs
    & foldMap
      ( \(Val unboxed boxed) -> ([unboxed], [boxed])
      )
    & \(us, bs) -> (useg us, bseg bs)

traverseListToSeg :: (a -> IO Val) -> [a] -> IO Seg
traverseListToSeg f src = do
  udst <- newByteArray $ bytes sz
  bdst <- newArray sz BlackHole
  let fill _ [] = do
        udst <- unsafeFreezeByteArray udst
        bdst <- unsafeFreezeArray bdst
        pure (udst, bdst)
      fill i (x : xs) = do
        Val un bx <- f x
        writeByteArray udst i un
        writeArray bdst i bx
        fill (i - 1) xs
  fill (sz - 1) src
  where
    sz = length src
{-# INLINE traverseListToSeg #-}

-- Note: this traverses the Seg left-to-right, which is backwards in
-- element terms. This is more efficient for building the correct
-- (reversed) list, but it means the effects happen in the opposite
-- order of the resulting values. This is not a problem for intended
-- uses, though, since the effect orders do not really matter.
traverseAccumSegToList ::
  (Val -> StateT s IO a) -> Seg -> StateT s IO [a]
traverseAccumSegToList f (usrc, bsrc) = StateT \s -> go s [] 0
  where
    sz = sizeofArray bsrc

    go s xs i
      | i < sz = do
          let !un = indexByteArray usrc i
          bx <- indexArrayM bsrc i
          (x, s) <- runStateT (f (Val un bx)) s
          x <- evaluate x
          go s (x : xs) (i + 1)
      | otherwise = pure (xs, s)
{-# INLINE traverseAccumSegToList #-}

marshalToForeign :: (HasCallStack) => Closure -> Foreign
marshalToForeign (Foreign x) = x
marshalToForeign c =
  error $ "marshalToForeign: unhandled closure: " ++ show c

data RuntimePanic = Panic String (Maybe Val)
  deriving (Show)

instance Exception RuntimePanic

marshalUnwrapForeignIO :: (HasCallStack) => Closure -> IO a
marshalUnwrapForeignIO (Foreign x) = pure $ unwrapForeign x
marshalUnwrapForeignIO c =
  throwIO $ Panic "marshalUnwrapForeignIO: unhandled closure" (Just v)
  where
    v = BoxedVal c

type Off = Int

type SZ = Int

type FP = Int

type UA = MutableByteArray (PrimState IO)

type BA = MutableArray (PrimState IO) Closure

intSize :: Int
intSize = sizeOf (0 :: Int)

words :: Int -> Int
words n = n `div` intSize

bytes :: Int -> Int
bytes n = n * intSize

type Arrs = (UA, BA)

argOnto :: Arrs -> Off -> Arrs -> Off -> Args' -> IO Int
argOnto (srcUstk, srcBstk) srcSp (dstUstk, dstBstk) dstSp args = do
  -- Both new cp's should be the same, so we can just return one.
  _cp <- uargOnto srcUstk srcSp dstUstk dstSp args
  cp <- bargOnto srcBstk srcSp dstBstk dstSp args
  pure cp

-- The Caller must ensure that when setting the unboxed stack, the equivalent
-- boxed stack is zeroed out to BlackHole where necessary.
uargOnto :: UA -> Off -> UA -> Off -> Args' -> IO Int
uargOnto stk sp cop cp0 (Arg1 i) = do
  (x :: Int) <- readByteArray stk (sp - i)
  writeByteArray cop cp x
  pure cp
  where
    cp = cp0 + 1
uargOnto stk sp cop cp0 (Arg2 i j) = do
  (x :: Int) <- readByteArray stk (sp - i)
  (y :: Int) <- readByteArray stk (sp - j)
  writeByteArray cop cp x
  writeByteArray cop (cp - 1) y
  pure cp
  where
    cp = cp0 + 2
uargOnto stk sp cop cp0 (ArgN v) = do
  buf <-
    if overwrite
      then newByteArray $ bytes sz
      else pure cop
  let loop i
        | i < 0 = return ()
        | otherwise = do
            (x :: Int) <- readByteArray stk (sp - indexPrimArray v i)
            writeByteArray buf (boff - i) x
            loop $ i - 1
  loop $ sz - 1
  when overwrite $
    copyMutableByteArray cop (bytes $ cp0 + 1) buf 0 (bytes sz)
  pure cp
  where
    cp = cp0 + sz
    sz = sizeofPrimArray v
    overwrite = sameMutableByteArray stk cop
    boff | overwrite = sz - 1 | otherwise = cp0 + sz
uargOnto stk sp cop cp0 (ArgR i l) = do
  moveByteArray cop cbp stk sbp (bytes l)
  pure $ cp0 + l
  where
    cbp = bytes $ cp0 + 1
    sbp = bytes $ sp - i - l + 1

bargOnto :: BA -> Off -> BA -> Off -> Args' -> IO Int
bargOnto stk sp cop cp0 (Arg1 i) = do
  x <- readArray stk (sp - i)
  writeArray cop cp x
  pure cp
  where
    cp = cp0 + 1
bargOnto stk sp cop cp0 (Arg2 i j) = do
  x <- readArray stk (sp - i)
  y <- readArray stk (sp - j)
  writeArray cop cp x
  writeArray cop (cp - 1) y
  pure cp
  where
    cp = cp0 + 2
bargOnto stk sp cop cp0 (ArgN v) = do
  buf <-
    if overwrite
      then newArray sz $ BlackHole
      else pure cop
  let loop i
        | i < 0 = return ()
        | otherwise = do
            x <- readArray stk $ sp - indexPrimArray v i
            writeArray buf (boff - i) x
            loop $ i - 1
  loop $ sz - 1

  when overwrite $
    copyMutableArray cop (cp0 + 1) buf 0 sz
  pure cp
  where
    cp = cp0 + sz
    sz = sizeofPrimArray v
    overwrite = stk == cop
    boff | overwrite = sz - 1 | otherwise = cp0 + sz
bargOnto stk sp cop cp0 (ArgR i l) = do
  copyMutableArray cop (cp0 + 1) stk (sp - i - l + 1) l
  pure $ cp0 + l

data Dump = A | F Int Int | S

dumpAP :: Int -> Int -> Int -> Dump -> Int
dumpAP _ fp sz d@(F _ a) = dumpFP fp sz d - a
dumpAP ap _ _ _ = ap

dumpFP :: Int -> Int -> Dump -> Int
dumpFP fp _ S = fp
dumpFP fp sz A = fp + sz
dumpFP fp sz (F n _) = fp + sz - n

-- closure augmentation mode
-- instruction, kontinuation, call
data Augment = I | K | C

data Stack = Stack
  { ap :: !Int, -- arg pointer
    fp :: !Int, -- frame pointer
    sp :: !Int, -- stack pointer
    ustk :: {-# UNPACK #-} !(MutableByteArray (PrimState IO)),
    bstk :: {-# UNPACK #-} !(MutableArray (PrimState IO) Closure)
  }

-- Unboxed representation of the Stack, used to force GHC optimizations in a few spots.
type XStack = (# Int#, Int#, Int#, MutableByteArray# (PrimState IO), MutableArray# (PrimState IO) Closure #)

type IOXStack = State# RealWorld -> (# State# RealWorld, XStack #)

type IOEXStack =
  State# RealWorld -> (# State# RealWorld, Bool, XStack #)

pattern XStack :: Int# -> Int# -> Int# -> MutableByteArray# RealWorld -> MutableArray# RealWorld Closure -> Stack
pattern XStack {apX, fpX, spX, ustkX, bstkX} = Stack (I# apX) (I# fpX) (I# spX) (MutableByteArray ustkX) (MutableArray bstkX)

{-# COMPLETE XStack #-}

{-# INLINE XStack #-}

packXStack :: XStack -> Stack
packXStack (# ap, fp, sp, ustk, bstk #) = Stack {ap = I# ap, fp = I# fp, sp = I# sp, ustk = MutableByteArray ustk, bstk = MutableArray bstk}
{-# INLINE packXStack #-}

unpackXStack :: Stack -> XStack
unpackXStack (Stack (I# ap) (I# fp) (I# sp) (MutableByteArray ustk) (MutableArray bstk)) = (# ap, fp, sp, ustk, bstk #)
{-# INLINE unpackXStack #-}

xStackIOToIO :: IOXStack -> IO Stack
xStackIOToIO f = IO $ \s -> case f s of (# s', x #) -> (# s', packXStack x #)
{-# INLINE xStackIOToIO #-}

stackIOToIOX :: IO Stack -> IOXStack
stackIOToIOX (IO f) = \s -> case f s of (# s', x #) -> (# s', unpackXStack x #)
{-# INLINE stackIOToIOX #-}

estackIOToIOX :: IO (Bool, Stack) -> IOEXStack
estackIOToIOX (IO f) = \s -> case f s of
  (# s', (b, x) #) -> (# s', b, unpackXStack x #)

exStackIOToIO :: IOEXStack -> IO (Bool, Stack)
exStackIOToIO f = IO $ \s -> case f s of
  (# s, b, x #) -> (# s, (b, packXStack x) #)

instance Show Stack where
  show (Stack ap fp sp _ _) =
    "Stack " ++ show ap ++ " " ++ show fp ++ " " ++ show sp

type UVal = Int

-- | A runtime value, which is either a boxed or unboxed value, but we may not know which.
--
--   When it represents a boxed value, `getUnboxedVal` is meaningless, but when it represents an unboxed value,
--   `getBoxedVal` tells us its type.
data Val = Val {getUnboxedVal :: !UVal, getBoxedVal :: !BVal}
  deriving (Show)

-- | The `Eq` instance for `Val` can’t be derived because you need to take into account the fact that if a `Val` is
--   boxed, the unboxed side is garbage and should not be compared.
instance Eq Val where
  (==) = universalEq (==)

instance Ord Val where
  compare = universalCompare compare

instance BuiltinForeign (Map Val Val) where
  foreignName = Tagged "Map"
  foreignRef = Tagged Ty.hmapRef

instance BuiltinForeign (IORef Val) where
  foreignName = Tagged "IORef"
  foreignRef = Tagged Ty.refRef

instance BuiltinForeign (Atomic.Ticket Val) where
  foreignName = Tagged "Ticket"
  foreignRef = Tagged Ty.ticketRef

instance BuiltinForeign (MVar Val) where
  foreignName = Tagged "MVar"
  foreignRef = Tagged Ty.mvarRef

instance BuiltinForeign (TVar Val) where
  foreignName = Tagged "TVar"
  foreignRef = Tagged Ty.tvarRef

instance BuiltinForeign (Promise Val) where
  foreignName = Tagged "Promise"
  foreignRef = Tagged Ty.promiseRef

instance BuiltinForeign (MutableArray s Val) where
  foreignName = Tagged "MutableArray"
  foreignRef = Tagged Ty.marrayRef

instance BuiltinForeign (Array Val) where
  foreignName = Tagged "Array"
  foreignRef = Tagged Ty.iarrayRef

-- | A nulled out value you can use when filling empty arrays, etc.
emptyVal :: Val
emptyVal = Val (-1) BlackHole

pattern UnboxedVal :: Int -> UnboxedTypeTag -> Val
pattern UnboxedVal v t = (Val v (UnboxedTypeTag t))

valToBoxed :: Val -> Maybe Closure
valToBoxed UnboxedVal {} = Nothing
valToBoxed (Val _ b) = Just b

-- | Matches a Val which is known to be boxed, and returns the closure portion.
pattern BoxedVal :: Closure -> Val
pattern BoxedVal b <- (valToBoxed -> Just b)
  where
    BoxedVal b = Val (-1) b

{-# COMPLETE UnboxedVal, BoxedVal #-}

-- | Lift a boxed val into an Val
boxedVal :: BVal -> Val
boxedVal = Val 0

type USeg = ByteArray

type BVal = Closure

type BSeg = Array Closure

type Seg = (USeg, BSeg)

alloc :: IO Stack
alloc = do
  ustk <- newByteArray 4096
  bstk <- newArray 512 BlackHole
  pure $ Stack {ap = -1, fp = -1, sp = -1, ustk, bstk}
{-# INLINE alloc #-}

{- ORMOLU_DISABLE -}
{- because ormolu-0.7.2.0 can’t handle CPP used within declarations. -}

peek :: (DebugCallStack) => Stack -> IO Val
peek stk@(Stack _ _ sp ustk _) = do
  -- Can't use upeek here because in stack-check mode it will assert that the stack slot is unboxed.
  u <- readByteArray ustk sp
  b <- bpeek stk
  pure (Val u b)
{-# INLINE peek #-}

peekI :: (DebugCallStack) => Stack -> IO Int
peekI _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE peekI #-}

peekOffI :: (DebugCallStack) => Stack -> Off -> IO Int
peekOffI _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE peekOffI #-}

bpeek :: (DebugCallStack) => Stack -> IO BVal
bpeek (Stack _ _ sp _ bstk) = readArray bstk sp
{-# INLINE bpeek #-}

upeek :: (DebugCallStack) => Stack -> IO UVal
upeek _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE upeek #-}

peekOff :: (DebugCallStack) => Stack -> Off -> IO Val
peekOff stk@(Stack _ _ sp ustk _) i = do
  -- Can't use upeekOff here because in stack-check mode it will assert that the stack slot is unboxed.
  u <- readByteArray ustk (sp - i)
  b <- bpeekOff stk i
  pure $ Val u b
{-# INLINE peekOff #-}

bpeekOff :: (DebugCallStack) => Stack -> Off -> IO BVal
bpeekOff (Stack _ _ sp _ bstk) i = readArray bstk (sp - i)
{-# INLINE bpeekOff #-}

upeekOff :: (DebugCallStack) => Stack -> Off -> IO UVal
upeekOff _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE upeekOff #-}

upokeT :: (DebugCallStack) => Stack -> UVal -> BVal -> IO ()
upokeT !stk@(Stack _ _ sp ustk _) !u !t = do
  bpoke stk t
  writeByteArray ustk sp u
{-# INLINE upokeT #-}

poke :: (DebugCallStack) => Stack -> Val -> IO ()
poke _stk@(Stack _ _ sp ustk bstk) (Val u b) = do
#ifdef STACK_CHECK
  assertBumped _stk 0
#endif
  writeByteArray ustk sp u
  writeArray bstk sp b
{-# INLINE poke #-}

-- | Sometimes we get back an int from a foreign call which we want to use as a Nat.
-- If we know it's positive and smaller than 2^63 then we can safely store the Int directly as a Nat without
-- checks.
unsafePokeIasN :: (DebugCallStack) => Stack -> Int -> IO ()
unsafePokeIasN stk n = do
  upokeT stk n natTypeTag
{-# INLINE unsafePokeIasN #-}

-- | Store an unboxed tag to later match on.
-- Often used to indicate the constructor of a data type that's been unpacked onto the stack,
-- or some tag we're about to branch on.
pokeTag :: (DebugCallStack) => Stack -> Int -> IO ()
pokeTag =
  -- For now we just use ints, but maybe should have a separate type for tags so we can detect if we're leaking them.
  pokeI
{-# INLINE pokeTag #-}

peekTag :: (DebugCallStack) => Stack -> IO Int
peekTag = peekI
{-# INLINE peekTag #-}

peekTagOff :: (DebugCallStack) => Stack -> Off -> IO Int
peekTagOff = peekOffI
{-# INLINE peekTagOff #-}

pokeBool :: (DebugCallStack) => Stack -> Bool -> IO ()
pokeBool stk b =
  poke stk $ if b then trueVal else falseVal
{-# INLINE pokeBool #-}

-- | Store a boxed value.
-- We don't bother nulling out the unboxed stack,
-- it's extra work and there's nothing to garbage collect.
bpoke :: (DebugCallStack) => Stack -> BVal -> IO ()
bpoke _stk@(Stack _ _ sp _ bstk) !b = do
#ifdef STACK_CHECK
  assertBumped _stk 0
#endif
  writeArray bstk sp b
{-# INLINE bpoke #-}

pokeOff :: (DebugCallStack) => Stack -> Off -> Val -> IO ()
pokeOff stk i (Val u t) = do
  bpokeOff stk i t
  writeByteArray (ustk stk) (sp stk - i) u
{-# INLINE pokeOff #-}

upokeOffT :: (DebugCallStack) => Stack -> Off -> UVal -> BVal -> IO ()
upokeOffT stk i u t = do
  bpokeOff stk i t
  writeByteArray (ustk stk) (sp stk - i) u
{-# INLINE upokeOffT #-}

bpokeOff :: (DebugCallStack) => Stack -> Off -> BVal -> IO ()
bpokeOff _stk@(Stack _ _ sp _ bstk) i !b = do
#ifdef STACK_CHECK
  assertBumped _stk i
#endif
  writeArray bstk (sp - i) b
{-# INLINE bpokeOff #-}

-- | Eats up arguments
grabSeg :: Stack -> SZ -> IO (Seg, Stack)
grabSeg (Stack _ fp sp ustk bstk) sze = do
  uSeg <- ugrab
  bSeg <- bgrab
  pure $ ((uSeg, bSeg), Stack (fp - sze) (fp - sze) (sp - sze) ustk bstk)
  where
    ugrab = do
      mut <- newByteArray bsz
      copyMutableByteArray mut 0 ustk (bfp - bsz) bsz
      seg <- unsafeFreezeByteArray mut
      moveByteArray ustk (bfp - bsz) ustk bfp fsz
      pure seg
      where
        bsz = bytes sze
        bfp = bytes $ fp + 1
        fsz = bytes $ sp - fp
    bgrab = do
      seg <- unsafeFreezeArray =<< cloneMutableArray bstk (fp + 1 - sze) sze
      copyMutableArray bstk (fp + 1 - sze) bstk (fp + 1) fsz
      pure seg
      where
        fsz = sp - fp
{-# INLINE grabSeg #-}

-- Truncates a portion of a stack, yielding the new stack without the
-- discarded portion. This is analogous to the stack yielded by
-- `grab`, but without doing the work of capturing the discarded
-- portion.
truncateSeg :: Stack -> SZ -> IO Stack
truncateSeg (Stack _ fp sp ustk bstk) sze = do
  moveByteArray ustk (bfp - bsz) ustk bfp fsz
  copyMutableArray bstk (fp + 1 - sze) bstk (fp + 1) fsz
  -- TODO: overwrite stale stack values?
  pure $ Stack (fp - sze) (fp - sze) (sp - sze) ustk bstk
  where
    bfp = bytes $ fp + 1
    bsz = bytes sze
    fsz = bytes $ sp - fp
{-# INLINE truncateSeg #-}

ensure :: Stack -> SZ -> IO Stack
ensure stk@(Stack ap fp sp ustk bstk) sze
  | sze <= 0 = pure stk
  | sp + sze + 1 < bsz = pure stk
  | otherwise = do
      bstk' <- newArray (bsz + bext) BlackHole
      copyMutableArray bstk' 0 bstk 0 (sp + 1)
      ustk' <- resizeMutableByteArray ustk (usz + uext)
      pure $ Stack ap fp sp ustk' bstk'
  where
    usz = sizeofMutableByteArray ustk
    bsz = sizeofMutableArray bstk
    bext
      | sze > 1280 = sze + 512
      | otherwise = 1280
    uext
      | bytes sze > 10240 = bytes sze + 4096
      | otherwise = 10240
{-# INLINE ensure #-}

bump :: Stack -> IO Stack
bump (Stack ap fp sp ustk bstk) = do
  let stk' = Stack ap fp (sp + 1) ustk bstk
#ifdef STACK_CHECK
  pokeSentinelOff stk' 0
#endif
  pure stk'
{-# INLINE bump #-}

bumpn :: Stack -> SZ -> IO Stack
bumpn (Stack ap fp sp ustk bstk) n = do
  let stk' = Stack ap fp (sp + n) ustk bstk
#ifdef STACK_CHECK
  for_ [0..n-1] $ \i ->
    pokeSentinelOff stk' i
#endif
  pure stk'
{-# INLINE bumpn #-}

duplicate :: Stack -> IO Stack
duplicate (Stack ap fp sp ustk bstk) = do
  ustk' <- dupUStk
  bstk' <- dupBStk
  pure $ Stack ap fp sp ustk' bstk'
  where
    dupUStk = do
      let sz = sizeofMutableByteArray ustk
      b <- newByteArray sz
      copyMutableByteArray b 0 ustk 0 sz
      pure b
    dupBStk = do
      cloneMutableArray bstk 0 (sizeofMutableArray bstk)
{-# INLINE duplicate #-}

discardFrame :: Stack -> IO Stack
discardFrame (Stack ap fp _ ustk bstk) = pure $ Stack ap fp fp ustk bstk
{-# INLINE discardFrame #-}

saveFrame :: Stack -> IO (Stack, SZ, SZ)
saveFrame (Stack ap fp sp ustk bstk) = pure (Stack sp sp sp ustk bstk, sp - fp, fp - ap)
{-# INLINE saveFrame #-}

saveArgs :: Stack -> IO (Stack, SZ)
saveArgs (Stack ap fp sp ustk bstk) = pure (Stack fp fp sp ustk bstk, fp - ap)
{-# INLINE saveArgs #-}

restoreFrame :: Stack -> SZ -> SZ -> IO Stack
restoreFrame (Stack _ fp0 sp ustk bstk) fsz asz = pure $ Stack ap fp sp ustk bstk
  where
    fp = fp0 - fsz
    ap = fp - asz
{-# INLINE restoreFrame #-}

prepareArgs :: Stack -> Args' -> IO Stack
prepareArgs (Stack ap fp sp ustk bstk) = \case
  ArgR i l
    | fp + l + i == sp ->
        pure $ Stack ap (sp - i) (sp - i) ustk bstk
  args -> do
    sp <- argOnto (ustk, bstk) sp (ustk, bstk) fp args
    pure $ Stack ap sp sp ustk bstk
{-# INLINE prepareArgs #-}

acceptArgs :: Stack -> Int -> IO Stack
acceptArgs (Stack ap fp sp ustk bstk) n = pure $ Stack ap (fp - n) sp ustk bstk
{-# INLINE acceptArgs #-}

frameArgs :: Stack -> IO Stack
frameArgs (Stack ap _ sp ustk bstk) = pure $ Stack ap ap sp ustk bstk
{-# INLINE frameArgs #-}

augSeg :: Augment -> Stack -> Seg -> Maybe Args' -> IO Seg
augSeg mode (Stack ap fp sp ustk bstk) (useg, bseg) margs = do
  useg' <- unboxedSeg
  bseg' <- boxedSeg
  pure (useg', bseg')
  where
    bpsz
      | I <- mode = 0
      | otherwise = fp - ap
    unboxedSeg = do
      cop <- newByteArray $ ssz + upsz + asz
      copyByteArray cop soff useg 0 ssz
      copyMutableByteArray cop 0 ustk (bytes $ ap + 1) upsz
      for_ margs $ uargOnto ustk sp cop (words poff + bpsz - 1)
      unsafeFreezeByteArray cop
      where
        ssz = sizeofByteArray useg
        (poff, soff)
          | K <- mode = (ssz, 0)
          | otherwise = (0, upsz + asz)
        upsz = bytes bpsz
        asz = case margs of
          Nothing -> bytes 0
          Just (Arg1 _) -> bytes 1
          Just (Arg2 _ _) -> bytes 2
          Just (ArgN v) -> bytes $ sizeofPrimArray v
          Just (ArgR _ l) -> bytes l
    boxedSeg = do
      cop <- newArray (ssz + bpsz + asz) BlackHole
      copyArray cop soff bseg 0 ssz
      copyMutableArray cop poff bstk (ap + 1) bpsz
      for_ margs $ bargOnto bstk sp cop (poff + bpsz - 1)
      unsafeFreezeArray cop
      where
        ssz = sizeofArray bseg
        (poff, soff)
          | K <- mode = (ssz, 0)
          | otherwise = (0, bpsz + asz)
        asz = case margs of
          Nothing -> 0
          Just (Arg1 _) -> 1
          Just (Arg2 _ _) -> 2
          Just (ArgN v) -> sizeofPrimArray v
          Just (ArgR _ l) -> l
{-# INLINE augSeg #-}

dumpSeg :: Stack -> Seg -> Dump -> IO Stack
dumpSeg (Stack ap fp sp ustk bstk) (useg, bseg) mode = do
  dumpUSeg
  dumpBSeg
  pure $ Stack ap' fp' sp' ustk bstk
  where
    sz = sizeofArray bseg
    sp' = sp + sz
    fp' = dumpFP fp sz mode
    ap' = dumpAP ap fp sz mode
    dumpUSeg = do
      let ssz = sizeofByteArray useg
      let bsp = bytes $ sp + 1
      copyByteArray ustk bsp useg 0 ssz
    dumpBSeg = do
      copyArray bstk (sp + 1) bseg 0 sz
{-# INLINE dumpSeg #-}

adjustArgs :: Stack -> SZ -> IO Stack
adjustArgs (Stack ap fp sp ustk bstk) sz = pure $ Stack (ap - sz) fp sp ustk bstk
{-# INLINE adjustArgs #-}

fsize :: Stack -> SZ
fsize (Stack _ fp sp _ _) = sp - fp
{-# INLINE fsize #-}

asize :: Stack -> SZ
asize (Stack ap fp _ _ _) = fp - ap
{-# INLINE asize #-}

peekN :: Stack -> IO Word64
peekN _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE peekN #-}

peekD :: Stack -> IO Double
peekD _stk@(Stack _ _ sp ustk _) = do
#ifdef STACK_CHECK
  assertUnboxed _stk 0
#endif
  readByteArray ustk sp
{-# INLINE peekD #-}

peekC :: Stack -> IO Char
peekC stk = do
  Char.chr <$> peekI stk
{-# INLINE peekC #-}

peekOffN :: Stack -> Int -> IO Word64
peekOffN _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE peekOffN #-}

peekOffD :: Stack -> Int -> IO Double
peekOffD _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  readByteArray ustk (sp - i)
{-# INLINE peekOffD #-}

peekOffC :: Stack -> Int -> IO Char
peekOffC _stk@(Stack _ _ sp ustk _) i = do
#ifdef STACK_CHECK
  assertUnboxed _stk i
#endif
  Char.chr <$> readByteArray ustk (sp - i)
{-# INLINE peekOffC #-}

{- ORMOLU_ENABLE -}

pokeN :: Stack -> Word64 -> IO ()
pokeN stk@(Stack _ _ sp ustk _) n = do
  bpoke stk natTypeTag
  writeByteArray ustk sp n
{-# INLINE pokeN #-}

pokeD :: Stack -> Double -> IO ()
pokeD stk@(Stack _ _ sp ustk _) d = do
  bpoke stk floatTypeTag
  writeByteArray ustk sp d
{-# INLINE pokeD #-}

pokeC :: Stack -> Char -> IO ()
pokeC stk@(Stack _ _ sp ustk _) c = do
  bpoke stk charTypeTag
  writeByteArray ustk sp (Char.ord c)
{-# INLINE pokeC #-}

-- | Note: This is for poking an unboxed value that has the UNISON type 'int', not just any unboxed data.
pokeI :: Stack -> Int -> IO ()
pokeI stk@(Stack _ _ sp ustk _) i = do
  bpoke stk intTypeTag
  writeByteArray ustk sp i
{-# INLINE pokeI #-}

pokeByte :: Stack -> Word8 -> IO ()
pokeByte stk b = do
  -- NOTE: currently we just store bytes as Word64s, but we should have a separate type runtime type tag for them.
  pokeN stk (fromIntegral b)
{-# INLINE pokeByte #-}

pokeOffN :: Stack -> Int -> Word64 -> IO ()
pokeOffN stk@(Stack _ _ sp ustk _) i n = do
  bpokeOff stk i natTypeTag
  writeByteArray ustk (sp - i) n
{-# INLINE pokeOffN #-}

pokeOffD :: Stack -> Int -> Double -> IO ()
pokeOffD stk@(Stack _ _ sp ustk _) i d = do
  bpokeOff stk i floatTypeTag
  writeByteArray ustk (sp - i) d
{-# INLINE pokeOffD #-}

pokeOffI :: Stack -> Int -> Int -> IO ()
pokeOffI stk@(Stack _ _ sp ustk _) i n = do
  bpokeOff stk i intTypeTag
  writeByteArray ustk (sp - i) n
{-# INLINE pokeOffI #-}

pokeOffC :: Stack -> Int -> Char -> IO ()
pokeOffC stk i c = do
  upokeOffT stk i (Char.ord c) charTypeTag
{-# INLINE pokeOffC #-}

pokeBi :: (BuiltinForeign b) => Stack -> b -> IO ()
pokeBi stk x = bpoke stk (Foreign $ wrapBuiltin x)
{-# INLINE pokeBi #-}

pokeOffBi :: (BuiltinForeign b) => Stack -> Int -> b -> IO ()
pokeOffBi stk i x = bpokeOff stk i (Foreign $ wrapBuiltin x)
{-# INLINE pokeOffBi #-}

peekBi :: (BuiltinForeign b) => Stack -> IO b
peekBi stk = unwrapForeign . marshalToForeign <$> bpeek stk
{-# INLINE peekBi #-}

peekOffBi :: (BuiltinForeign b) => Stack -> Int -> IO b
peekOffBi stk i = unwrapForeign . marshalToForeign <$> bpeekOff stk i
{-# INLINE peekOffBi #-}

peekBool :: Stack -> IO Bool
peekBool stk = do
  b <- bpeek stk
  pure $ case b of
    Enum _ t -> t /= TT.falseTag
    _ -> error "peekBool: not a boolean"
{-# INLINE peekBool #-}

peekOffBool :: Stack -> Int -> IO Bool
peekOffBool stk i = do
  b <- bpeekOff stk i
  pure $ case b of
    Enum _ t -> t /= TT.falseTag
    _ -> error "peekOffBool: not a boolean"
{-# INLINE peekOffBool #-}

peekOffS :: Stack -> Int -> IO USeq
peekOffS stk i =
  unwrapForeign . marshalToForeign <$> bpeekOff stk i
{-# INLINE peekOffS #-}

pokeS :: Stack -> USeq -> IO ()
pokeS stk s = bpoke stk (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeS #-}

pokeOffS :: Stack -> Int -> USeq -> IO ()
pokeOffS stk i s = bpokeOff stk i (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeOffS #-}

unull :: USeg
unull = byteArrayFromListN 0 ([] :: [Int])

bnull :: BSeg
bnull = fromListN 0 []

nullSeg :: Seg
nullSeg = (unull, bnull)

instance Show K where
  show k = "[" ++ go "" k
    where
      go _ KE = "]"
      go _ (CB _) = "]"
      go com (Push f a ci _g _rsect k) =
        com ++ show (f, a, ci) ++ go "," k
      go com (Mark a ps _ k) =
        com ++ "M " ++ show a ++ " " ++ show ps ++ go "," k
      go com (Local _ a k) =
        com ++ "L " ++ show a ++ go "," k
      go com (AMark a _ _ k) =
        com ++ "A " ++ show a ++ go "," k

frameView :: Stack -> IO ()
frameView stk = putStr "|" >> gof False 0
  where
    fsz = fsize stk
    asz = asize stk
    gof delim n
      | n >= fsz = putStr "|" >> goa False 0
      | otherwise = do
          when delim $ putStr ","
          putStr . show =<< peekOff stk n
          gof True (n + 1)
    goa delim n
      | n >= asz = putStrLn "|.."
      | otherwise = do
          when delim $ putStr ","
          putStr . show =<< peekOff stk (fsz + n)
          goa True (n + 1)

scount :: Seg -> Int
scount (_, bseg) = bscount bseg
  where
    bscount :: BSeg -> Int
    bscount seg = sizeofArray seg

closureTermRefs :: (Monoid m) => (Reference -> m) -> (Closure -> m)
closureTermRefs f = \case
  PAp (CIx r _ _) _ (_useg, bseg) ->
    f r <> foldMap (closureTermRefs f) bseg
  (DataC _ _ vs) ->
    vs & foldMap \case
      BoxedVal c -> closureTermRefs f c
      UnboxedVal {} -> mempty
  (Captured k _ (_useg, bseg)) ->
    contTermRefs f k <> foldMap (closureTermRefs f) bseg
  (Foreign fo)
    | Just (cs :: USeq) <- maybeUnwrapForeign Ty.listRef fo ->
        foldMap (\(Val _i clos) -> closureTermRefs f clos) cs
  _ -> mempty

contTermRefs :: (Monoid m) => (Reference -> m) -> K -> m
contTermRefs f (Mark _ _ m k) =
  ( m & foldMap \case
      BoxedVal clo -> closureTermRefs f clo
      _ -> mempty
  )
    <> contTermRefs f k
contTermRefs f (Push _ _ (CIx r _ _) _ _ k) =
  f r <> contTermRefs f k
contTermRefs _ _ = mempty

hasNoAllocations :: TH.Name -> TI.Obligation
hasNoAllocations n = TI.mkObligation n TI.NoAllocation

unitClosure :: Closure
unitClosure = Enum Ty.unitRef TT.unitTag
{-# NOINLINE unitClosure #-}

-- Universal comparison functions

closureNum :: Closure -> Int
closureNum PAp {} = 0
closureNum DataC {} = 1
closureNum Captured {} = 2
closureNum Foreign {} = 3
closureNum UnboxedTypeTag {} = 4
closureNum BlackHole {} = 5
closureNum Affine {} = 6

universalEq ::
  (Foreign -> Foreign -> Bool) ->
  Val ->
  Val ->
  Bool
universalEq frn = eqVal
  where
    eql :: (a -> b -> Bool) -> [a] -> [b] -> Bool
    eql cm l r = length l == length r && and (zipWith cm l r)
    eqVal :: Val -> Val -> Bool
    eqVal (UnboxedVal v1 t1) (UnboxedVal v2 t2) = matchUnboxedTypes t1 t2 && v1 == v2
    eqVal (BoxedVal x) (BoxedVal y) = eqc x y
    eqVal _ _ = False
    eqc :: Closure -> Closure -> Bool
    eqc (DataC _ ct1 [w1]) (DataC _ ct2 [w2]) =
      matchTags ct1 ct2 && eqVal w1 w2
    eqc (DataC _ ct1 vs1) (DataC _ ct2 vs2) =
      ct1 == ct2
        && eqValList vs1 vs2
    eqc (PApV cix1 _ segs1) (PApV cix2 _ segs2) =
      cix1 == cix2
        && eqValList segs1 segs2
    eqc (CapV k1 a1 vs1) (CapV k2 a2 vs2) =
      eqK k1 k2
        && a1 == a2
        && eqValList vs1 vs2
    eqc (Foreign fl) (Foreign fr)
      | Just al <- maybeUnwrapForeign @(PA.Array Val) Ty.iarrayRef fl,
        Just ar <- maybeUnwrapForeign @(PA.Array Val) Ty.iarrayRef fr =
          arrayEq eqVal al ar
      | Just sl <- maybeUnwrapForeign @(Seq Val) Ty.listRef fl,
        Just sr <- maybeUnwrapForeign @(Seq Val) Ty.listRef fr =
          length sl == length sr && and (Sq.zipWith eqVal sl sr)
      | Just ml <- maybeUnwrapForeign @(Map Val Val) Ty.hmapRef fl,
        Just mr <- maybeUnwrapForeign @(Map Val Val) Ty.hmapRef fr =
          mapEq eqVal eqVal ml mr
      | otherwise = frn fl fr
    eqc c d = closureNum c == closureNum d

    eqValList :: [Val] -> [Val] -> Bool
    eqValList vs1 vs2 = eql eqVal vs1 vs2

    eqK :: K -> K -> Bool
    eqK KE KE = True
    eqK (CB cb) (CB cb') = cb == cb'
    eqK (Mark a ps m k) (Mark a' ps' m' k') =
      a == a' && ps == ps' && liftEq eqVal m m' && eqK k k'
    eqK (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') =
      f == f' && a == a' && ci == ci' && eqK k k'
    eqK _ _ = False

-- IEEE floating point layout is such that comparison as integers
-- somewhat works. Positive floating values map to positive integers
-- and negatives map to negatives. The corner cases are:
--
--   1. If both numbers are negative, ordering is flipped.
--   2. There is both +0 and -0, with -0 being represented as the
--      minimum signed integer.
--   3. NaN does weird things.
--
-- So, the strategy here is to compare normally if one argument is
-- positive, since positive numbers compare normally to others.
-- Otherwise, the sign bit is cleared and the numbers are compared
-- backwards. Clearing the sign bit maps -0 to +0 and maps a negative
-- number to its absolute value (including infinities). The multiple
-- NaN values are just handled according to bit patterns, rather than
-- IEEE specified behavior.
--
-- Transitivity is somewhat non-obvious for this implementation.
--
--   if i <= j and j <= k
--     if j > 0 then k > 0, so all 3 comparisons use `compare`
--     if k > 0 then k > i, since i <= j <= 0
--     if all 3 are <= 0, all 3 comparisons use the alternate
--       comparison, which is transitive via `compare`
compareAsFloat :: Int -> Int -> Ordering
compareAsFloat i j
  | i > 0 || j > 0 = compare i j
  | otherwise = compare (clear j) (clear i)
  where
    clear k = clearBit k 64

universalCompare ::
  (Foreign -> Foreign -> Ordering) ->
  Val ->
  Val ->
  Ordering
universalCompare frn = cmpVal False
  where
    cmpVal :: Bool -> Val -> Val -> Ordering
    cmpVal tyEq = \cases
      (BoxedVal c1) (BoxedVal c2) -> cmpc tyEq c1 c2
      (UnboxedVal {}) (BoxedVal {}) -> LT
      (BoxedVal {}) (UnboxedVal {}) -> GT
      (NatVal i) (NatVal j) -> compare i j
      (UnboxedVal v1 t1) (UnboxedVal v2 t2) -> cmpUnboxed tyEq (t1, v1) (t2, v2)
    cmpl :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
    cmpl cm l r =
      compare (length l) (length r) <> fold (zipWith cm l r)
    cmpc :: Bool -> Closure -> Closure -> Ordering
    cmpc tyEq = \cases
      (DataC rf1 ct1 vs1) (DataC rf2 ct2 vs2) ->
        (if tyEq && ct1 /= ct2 then compare rf1 rf2 else EQ)
          <> compare (maskTags ct1) (maskTags ct2)
          -- when comparing corresponding `Any` values, which have
          -- existentials inside check that type references match
          <> cmpValList (tyEq || rf1 == Ty.anyRef) vs1 vs2
      (PApV cix1 _ segs1) (PApV cix2 _ segs2) ->
        compare cix1 cix2
          <> cmpValList tyEq segs1 segs2
      (CapV k1 a1 vs1) (CapV k2 a2 vs2) ->
        cmpK tyEq k1 k2
          <> compare a1 a2
          <> cmpValList True vs1 vs2
      (Foreign fl) (Foreign fr)
        | Just sl <- maybeUnwrapForeign @(Seq Val) Ty.listRef fl,
          Just sr <- maybeUnwrapForeign @(Seq Val) Ty.listRef fr ->
            fold (Sq.zipWith (cmpVal tyEq) sl sr)
              <> compare (length sl) (length sr)
        | Just al <- maybeUnwrapForeign @(PA.Array Val) Ty.iarrayRef fl,
          Just ar <- maybeUnwrapForeign @(PA.Array Val) Ty.iarrayRef fr ->
            arrayCmp (cmpVal tyEq) al ar
        | Just ml <- maybeUnwrapForeign @(Map Val Val) Ty.hmapRef fl,
          Just mr <- maybeUnwrapForeign @(Map Val Val) Ty.hmapRef fr ->
            mapCmp (cmpVal tyEq) (cmpVal tyEq) ml mr
        | otherwise -> frn fl fr
      (UnboxedTypeTag t1) (UnboxedTypeTag t2) -> compare t1 t2
      (BlackHole) (BlackHole) -> EQ
      c d -> comparing closureNum c d

    cmpUnboxed :: Bool -> (UnboxedTypeTag, Int) -> (UnboxedTypeTag, Int) -> Ordering
    cmpUnboxed tyEq = \cases
      -- Need to cast to Nat or else maxNat == -1 and it flips comparisons of large Nats.
      -- TODO: Investigate whether bit-twiddling is faster than using Haskell's fromIntegral.
      (IntTag, n1) (IntTag, n2) -> compare n1 n2
      (NatTag, n1) (NatTag, n2) -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (NatTag, n1) (IntTag, n2)
        | n2 < 0 -> GT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (IntTag, n1) (NatTag, n2)
        | n1 < 0 -> LT
        | otherwise -> compare (fromIntegral n1 :: Word64) (fromIntegral n2 :: Word64)
      (FloatTag, n1) (FloatTag, n2) -> compareAsFloat n1 n2
      (t1, v1) (t2, v2) ->
        Monoid.whenM tyEq (compare t1 t2)
          <> compare v1 v2

    cmpValList :: Bool -> [Val] -> [Val] -> Ordering
    cmpValList tyEq vs1 vs2 = cmpl (cmpVal tyEq) vs1 vs2

    cmpK :: Bool -> K -> K -> Ordering
    cmpK tyEq = \cases
      KE KE -> EQ
      (CB cb) (CB cb') -> compare cb cb'
      (Mark a ps m k) (Mark a' ps' m' k') ->
        compare a a'
          <> compare ps ps'
          <> liftCompare (cmpVal tyEq) m m'
          <> cmpK tyEq k k'
      (Push f a ci _ _sect k) (Push f' a' ci' _ _sect' k') ->
        compare f f'
          <> compare a a'
          <> compare ci ci'
          <> cmpK tyEq k k'
      KE _ -> LT
      _ KE -> GT
      (CB {}) _ -> LT
      _ (CB {}) -> GT
      (Mark {}) _ -> LT
      _ (Mark {}) -> GT
      (Local {}) _ -> error "compare K: captured Local frame"
      _ (Local {}) -> error "compare K: captured Local frame"
      (AMark {}) _ -> error "compare K: captured AMark frame"
      _ (AMark {}) -> error "compare K: captured AMark frame"

arrayCmp ::
  (a -> a -> Ordering) ->
  PA.Array a ->
  PA.Array a ->
  Ordering
arrayCmp cmpVal l r =
  comparing PA.sizeofArray l r <> go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = EQ
      | otherwise = cmpVal (PA.indexArray l i) (PA.indexArray r i) <> go (i - 1)

arrayEq :: (a -> a -> Bool) -> PA.Array a -> PA.Array a -> Bool
arrayEq eqc l r
  | PA.sizeofArray l /= PA.sizeofArray r = False
  | otherwise = go (PA.sizeofArray l - 1)
  where
    go i
      | i < 0 = True
      | otherwise = eqc (PA.indexArray l i) (PA.indexArray r i) && go (i - 1)

-- Note: these are not the same as the Data.Map Eq/Ord instances,
-- because the automatic derivations in unison doesn't consider
-- equivalent maps to be the same. It just checks the exact
-- data structure.
mapEq :: (k -> k -> Bool) -> (v -> v -> Bool) -> Map k v -> Map k v -> Bool
mapEq _ _ Tip Tip = True
mapEq ek ev (Bin szl kl vl ll rl) (Bin szr kr vr lr rr) =
  and
    [ szl == szr,
      ek kl kr,
      ev vl vr,
      mapEq ek ev ll lr,
      mapEq ek ev rl rr
    ]
mapEq _ _ _ _ = False

mapCmp ::
  (k -> k -> Ordering) ->
  (v -> v -> Ordering) ->
  Map k v ->
  Map k v ->
  Ordering
mapCmp _ _ Tip Tip = EQ
mapCmp ck cv (Bin szl kl vl ll rl) (Bin szr kr vr lr rr) =
  fold
    [ compare szl szr,
      ck kl kr,
      cv vl vr,
      mapCmp ck cv ll lr,
      mapCmp ck cv rl rr
    ]
mapCmp _ _ Tip Bin {} = compare mapTip mapBin
mapCmp _ _ Bin {} Tip = compare mapBin mapTip

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchTags :: PackedTag -> PackedTag -> Bool
matchTags ct1 ct2 =
  ct1 == ct2
    || (ct1 == TT.intTag && ct2 == TT.natTag)
    || (ct1 == TT.natTag && ct2 == TT.intTag)

-- serialization doesn't necessarily preserve Int tags, so be
-- more accepting for those.
matchUnboxedTypes :: UnboxedTypeTag -> UnboxedTypeTag -> Bool
matchUnboxedTypes ct1 ct2 =
  ct1 == ct2
    || (ct1 == IntTag && ct2 == NatTag)
    || (ct1 == NatTag && ct2 == IntTag)

-- Turn the pseudo data version of maps back into the closure that
-- it represents as a unison type.
inflateMap :: Map Val Val -> Closure
inflateMap Tip = Enum mapRef TT.mapTipTag
inflateMap (Bin sz k v l r) =
  DataC
    mapRef
    TT.mapBinTag
    [NatVal $ fromIntegral sz, k, v, BoxedVal $ inflateMap l, BoxedVal $ inflateMap r]

-- Reverses the above conversion, turning a unison data
-- representation of a map back into a Haskell map.
deflateMap :: Closure -> Maybe (Map Val Val)
deflateMap (Enum _ t)
  | t == TT.mapTipTag = Just Tip
deflateMap (DataC _ t [NatVal sz, k, v, BoxedVal l, BoxedVal r])
  | t == TT.mapBinTag =
      Bin (fromIntegral sz) k v <$> deflateMap l <*> deflateMap r
deflateMap _ = Nothing
