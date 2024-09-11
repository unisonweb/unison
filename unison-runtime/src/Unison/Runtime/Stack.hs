{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Stack
  ( K (..),
    GClosure (.., DataC, PApV, CapV),
    Closure,
    RClosure,
    IxClosure,
    Callback (..),
    Augment (..),
    Dump (..),
    MEM (..),
    Stack (..),
    Off,
    SZ,
    FP,
    traceK,
    frameDataSize,
    marshalToForeign,
    unull,
    bnull,
    peekD,
    peekOffD,
    pokeD,
    pokeOffD,
    peekN,
    peekOffN,
    pokeN,
    pokeOffN,
    peekBi,
    peekOffBi,
    pokeBi,
    pokeOffBi,
    peekOffS,
    pokeS,
    pokeOffS,
    frameView,
    uscount,
    bscount,
    closureTermRefs,
  )
where

import Control.Monad (when)
import Control.Monad.Primitive
import Data.Foldable as F (for_)
import Data.Kind qualified as Kind
import Data.Sequence (Seq)
import Data.Word
import GHC.Exts as L (IsList (..))
import GHC.Stack (HasCallStack)
import Unison.Reference (Reference)
import Unison.Runtime.ANF as ANF (Mem (..))
import Unison.Runtime.Array
import Unison.Runtime.Foreign
import Unison.Runtime.MCode
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Prelude hiding (words)

newtype Callback = Hook (Stack 'UN -> Stack 'BX -> IO ())

instance Eq Callback where _ == _ = True

instance Ord Callback where compare _ _ = EQ

-- Evaluation stack
data K
  = KE
  | -- callback hook
    CB Callback
  | -- mark continuation with a prompt
    Mark
      !Int -- pending unboxed args
      !Int -- pending boxed args
      !(EnumSet Word64)
      !(EnumMap Word64 RClosure)
      !K
  | -- save information about a frame for later resumption
    Push
      !Int -- unboxed frame size
      !Int -- boxed frame size
      !Int -- pending unboxed args
      !Int -- pending boxed args
      !RComb -- local continuation reference
      !K
  deriving (Eq, Ord)

type RClosure = GClosure RComb

type IxClosure = GClosure CombIx

type Closure = GClosure RComb

data GClosure comb
  = PAp
      !comb
      {-# UNPACK #-} !(Seg 'UN) -- unboxed args
      {-  unpack  -}
      !(Seg 'BX) -- boxed args
  | Enum !Reference !Word64
  | DataU1 !Reference !Word64 !Int
  | DataU2 !Reference !Word64 !Int !Int
  | DataB1 !Reference !Word64 !(GClosure comb)
  | DataB2 !Reference !Word64 !(GClosure comb) !(GClosure comb)
  | DataUB !Reference !Word64 !Int !(GClosure comb)
  | DataG !Reference !Word64 !(Seg 'UN) !(Seg 'BX)
  | -- code cont, u/b arg size, u/b data stacks
    Captured !K !Int !Int {-# UNPACK #-} !(Seg 'UN) !(Seg 'BX)
  | Foreign !Foreign
  | BlackHole
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

traceK :: Reference -> K -> [(Reference, Int)]
traceK begin = dedup (begin, 1)
  where
    dedup p (Mark _ _ _ _ k) = dedup p k
    dedup p@(cur, n) (Push _ _ _ _ (RComb (CIx r _ _) _) k)
      | cur == r = dedup (cur, 1 + n) k
      | otherwise = p : dedup (r, 1) k
    dedup p _ = [p]

splitData :: RClosure -> Maybe (Reference, Word64, [Int], [RClosure])
splitData (Enum r t) = Just (r, t, [], [])
splitData (DataU1 r t i) = Just (r, t, [i], [])
splitData (DataU2 r t i j) = Just (r, t, [i, j], [])
splitData (DataB1 r t x) = Just (r, t, [], [x])
splitData (DataB2 r t x y) = Just (r, t, [], [x, y])
splitData (DataUB r t i y) = Just (r, t, [i], [y])
splitData (DataG r t us bs) = Just (r, t, ints us, bsegToList bs)
splitData _ = Nothing

-- | Converts an unboxed segment to a list of integers for a more interchangeable
-- representation. The segments are stored in backwards order, so this reverses
-- the contents.
ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [n - 1, n - 2 .. 0]
  where
    n = sizeofByteArray ba `div` 8

-- | Converts a list of integers representing an unboxed segment back into the
-- appropriate segment. Segments are stored backwards in the runtime, so this
-- reverses the list.
useg :: [Int] -> Seg 'UN
useg ws = case L.fromList $ reverse ws of
  PrimArray ba -> ByteArray ba

-- | Converts a boxed segment to a list of closures. The segments are stored
-- backwards, so this reverses the contents.
bsegToList :: Seg 'BX -> [RClosure]
bsegToList = reverse . L.toList

-- | Converts a list of closures back to a boxed segment. Segments are stored
-- backwards, so this reverses the contents.
bseg :: [RClosure] -> Seg 'BX
bseg = L.fromList . reverse

formData :: Reference -> Word64 -> [Int] -> [RClosure] -> RClosure
formData r t [] [] = Enum r t
formData r t [i] [] = DataU1 r t i
formData r t [i, j] [] = DataU2 r t i j
formData r t [] [x] = DataB1 r t x
formData r t [] [x, y] = DataB2 r t x y
formData r t [i] [x] = DataUB r t i x
formData r t us bs = DataG r t (useg us) (bseg bs)

frameDataSize :: K -> (Int, Int)
frameDataSize = go 0 0
  where
    go usz bsz KE = (usz, bsz)
    go usz bsz (CB _) = (usz, bsz)
    go usz bsz (Mark ua ba _ _ k) = go (usz + ua) (bsz + ba) k
    go usz bsz (Push uf bf ua ba _ k) = go (usz + uf + ua) (bsz + bf + ba) k

pattern DataC :: Reference -> Word64 -> [Int] -> [RClosure] -> RClosure
pattern DataC rf ct us bs <-
  (splitData -> Just (rf, ct, us, bs))
  where
    DataC rf ct us bs = formData rf ct us bs

pattern PApV :: RComb -> [Int] -> [RClosure] -> RClosure
pattern PApV ic us bs <-
  PAp ic (ints -> us) (bsegToList -> bs)
  where
    PApV ic us bs = PAp ic (useg us) (bseg bs)

pattern CapV :: K -> Int -> Int -> [Int] -> [RClosure] -> RClosure
pattern CapV k ua ba us bs <-
  Captured k ua ba (ints -> us) (bsegToList -> bs)
  where
    CapV k ua ba us bs = Captured k ua ba (useg us) (bseg bs)

{-# COMPLETE DataC, PAp, Captured, Foreign, BlackHole #-}

{-# COMPLETE DataC, PApV, Captured, Foreign, BlackHole #-}

{-# COMPLETE DataC, PApV, CapV, Foreign, BlackHole #-}

marshalToForeign :: (HasCallStack) => RClosure -> Foreign
marshalToForeign (Foreign x) = x
marshalToForeign c =
  error $ "marshalToForeign: unhandled closure: " ++ show c

type Off = Int

type SZ = Int

type FP = Int

type UA = MutableByteArray (PrimState IO)

type BA = MutableArray (PrimState IO) RClosure

words :: Int -> Int
words n = n `div` 8

bytes :: Int -> Int
bytes n = n * 8

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
    copyMutableByteArray cop (bytes $ cp + 1) buf 0 (bytes sz)
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
      then newArray sz BlackHole
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

class MEM (b :: Mem) where
  data Stack b :: Kind.Type
  type Elem b :: Kind.Type
  type Seg b :: Kind.Type
  alloc :: IO (Stack b)
  peek :: Stack b -> IO (Elem b)
  peekOff :: Stack b -> Off -> IO (Elem b)
  poke :: Stack b -> Elem b -> IO ()
  pokeOff :: Stack b -> Off -> Elem b -> IO ()
  grab :: Stack b -> SZ -> IO (Seg b, Stack b)
  ensure :: Stack b -> SZ -> IO (Stack b)
  bump :: Stack b -> IO (Stack b)
  bumpn :: Stack b -> SZ -> IO (Stack b)
  duplicate :: Stack b -> IO (Stack b)
  discardFrame :: Stack b -> IO (Stack b)
  saveFrame :: Stack b -> IO (Stack b, SZ, SZ)
  saveArgs :: Stack b -> IO (Stack b, SZ)
  restoreFrame :: Stack b -> SZ -> SZ -> IO (Stack b)
  prepareArgs :: Stack b -> Args' -> IO (Stack b)
  acceptArgs :: Stack b -> Int -> IO (Stack b)
  frameArgs :: Stack b -> IO (Stack b)
  augSeg :: Augment -> Stack b -> Seg b -> Maybe Args' -> IO (Seg b)
  dumpSeg :: Stack b -> Seg b -> Dump -> IO (Stack b)
  adjustArgs :: Stack b -> SZ -> IO (Stack b)
  fsize :: Stack b -> SZ
  asize :: Stack b -> SZ

instance MEM 'UN where
  data Stack 'UN =
    -- Note: uap <= ufp <= usp
    US
    { uap :: !Int, -- arg pointer
      ufp :: !Int, -- frame pointer
      usp :: !Int, -- stack pointer
      ustk :: {-# UNPACK #-} !(MutableByteArray (PrimState IO))
    }
  type Elem 'UN = Int
  type Seg 'UN = ByteArray
  alloc = US (-1) (-1) (-1) <$> newByteArray 4096
  {-# INLINE alloc #-}
  peek (US _ _ sp stk) = readByteArray stk sp
  {-# INLINE peek #-}
  peekOff (US _ _ sp stk) i = readByteArray stk (sp - i)
  {-# INLINE peekOff #-}
  poke (US _ _ sp stk) n = writeByteArray stk sp n
  {-# INLINE poke #-}
  pokeOff (US _ _ sp stk) i n = writeByteArray stk (sp - i) n
  {-# INLINE pokeOff #-}

  -- Eats up arguments
  grab (US _ fp sp stk) sze = do
    mut <- newByteArray sz
    copyMutableByteArray mut 0 stk (bfp - sz) sz
    seg <- unsafeFreezeByteArray mut
    moveByteArray stk (bfp - sz) stk bfp fsz
    pure (seg, US (fp - sze) (fp - sze) (sp - sze) stk)
    where
      sz = bytes sze
      bfp = bytes $ fp + 1
      fsz = bytes $ sp - fp
  {-# INLINE grab #-}

  ensure stki@(US ap fp sp stk) sze
    | sze <= 0 || bytes (sp + sze + 1) < ssz = pure stki
    | otherwise = do
        stk' <- resizeMutableByteArray stk (ssz + ext)
        pure $ US ap fp sp stk'
    where
      ssz = sizeofMutableByteArray stk
      ext
        | bytes sze > 10240 = bytes sze + 4096
        | otherwise = 10240
  {-# INLINE ensure #-}

  bump (US ap fp sp stk) = pure $ US ap fp (sp + 1) stk
  {-# INLINE bump #-}

  bumpn (US ap fp sp stk) n = pure $ US ap fp (sp + n) stk
  {-# INLINE bumpn #-}

  duplicate (US ap fp sp stk) =
    US ap fp sp <$> do
      b <- newByteArray sz
      copyMutableByteArray b 0 stk 0 sz
      pure b
    where
      sz = sizeofMutableByteArray stk
  {-# INLINE duplicate #-}

  discardFrame (US ap fp _ stk) = pure $ US ap fp fp stk
  {-# INLINE discardFrame #-}

  saveFrame (US ap fp sp stk) = pure (US sp sp sp stk, sp - fp, fp - ap)
  {-# INLINE saveFrame #-}

  saveArgs (US ap fp sp stk) = pure (US fp fp sp stk, fp - ap)
  {-# INLINE saveArgs #-}

  restoreFrame (US _ fp0 sp stk) fsz asz = pure $ US ap fp sp stk
    where
      fp = fp0 - fsz
      ap = fp - asz
  {-# INLINE restoreFrame #-}

  prepareArgs (US ap fp sp stk) (ArgR i l)
    | fp + l + i == sp = pure $ US ap (sp - i) (sp - i) stk
  prepareArgs (US ap fp sp stk) args = do
    sp <- uargOnto stk sp stk fp args
    pure $ US ap sp sp stk
  {-# INLINE prepareArgs #-}

  acceptArgs (US ap fp sp stk) n = pure $ US ap (fp - n) sp stk
  {-# INLINE acceptArgs #-}

  frameArgs (US ap _ sp stk) = pure $ US ap ap sp stk
  {-# INLINE frameArgs #-}

  augSeg mode (US ap fp sp stk) seg margs = do
    cop <- newByteArray $ ssz + psz + asz
    copyByteArray cop soff seg 0 ssz
    copyMutableByteArray cop 0 stk (bytes $ ap + 1) psz
    for_ margs $ uargOnto stk sp cop (words poff + pix - 1)
    unsafeFreezeByteArray cop
    where
      ssz = sizeofByteArray seg
      pix | I <- mode = 0 | otherwise = fp - ap
      (poff, soff)
        | K <- mode = (ssz, 0)
        | otherwise = (0, psz + asz)
      psz = bytes pix
      asz = case margs of
        Nothing -> 0
        Just (Arg1 _) -> 8
        Just (Arg2 _ _) -> 16
        Just (ArgN v) -> bytes $ sizeofPrimArray v
        Just (ArgR _ l) -> bytes l
  {-# INLINE augSeg #-}

  dumpSeg (US ap fp sp stk) seg mode = do
    copyByteArray stk bsp seg 0 ssz
    pure $ US ap' fp' sp' stk
    where
      bsp = bytes $ sp + 1
      ssz = sizeofByteArray seg
      sz = words ssz
      sp' = sp + sz
      fp' = dumpFP fp sz mode
      ap' = dumpAP ap fp sz mode
  {-# INLINE dumpSeg #-}

  adjustArgs (US ap fp sp stk) sz = pure $ US (ap - sz) fp sp stk
  {-# INLINE adjustArgs #-}

  fsize (US _ fp sp _) = sp - fp
  {-# INLINE fsize #-}

  asize (US ap fp _ _) = fp - ap
  {-# INLINE asize #-}

peekN :: Stack 'UN -> IO Word64
peekN (US _ _ sp stk) = readByteArray stk sp
{-# INLINE peekN #-}

peekD :: Stack 'UN -> IO Double
peekD (US _ _ sp stk) = readByteArray stk sp
{-# INLINE peekD #-}

peekOffN :: Stack 'UN -> Int -> IO Word64
peekOffN (US _ _ sp stk) i = readByteArray stk (sp - i)
{-# INLINE peekOffN #-}

peekOffD :: Stack 'UN -> Int -> IO Double
peekOffD (US _ _ sp stk) i = readByteArray stk (sp - i)
{-# INLINE peekOffD #-}

pokeN :: Stack 'UN -> Word64 -> IO ()
pokeN (US _ _ sp stk) n = writeByteArray stk sp n
{-# INLINE pokeN #-}

pokeD :: Stack 'UN -> Double -> IO ()
pokeD (US _ _ sp stk) d = writeByteArray stk sp d
{-# INLINE pokeD #-}

pokeOffN :: Stack 'UN -> Int -> Word64 -> IO ()
pokeOffN (US _ _ sp stk) i n = writeByteArray stk (sp - i) n
{-# INLINE pokeOffN #-}

pokeOffD :: Stack 'UN -> Int -> Double -> IO ()
pokeOffD (US _ _ sp stk) i d = writeByteArray stk (sp - i) d
{-# INLINE pokeOffD #-}

pokeBi :: (BuiltinForeign b) => Stack 'BX -> b -> IO ()
pokeBi bstk x = poke bstk (Foreign $ wrapBuiltin x)
{-# INLINE pokeBi #-}

pokeOffBi :: (BuiltinForeign b) => Stack 'BX -> Int -> b -> IO ()
pokeOffBi bstk i x = pokeOff bstk i (Foreign $ wrapBuiltin x)
{-# INLINE pokeOffBi #-}

peekBi :: (BuiltinForeign b) => Stack 'BX -> IO b
peekBi bstk = unwrapForeign . marshalToForeign <$> peek bstk
{-# INLINE peekBi #-}

peekOffBi :: (BuiltinForeign b) => Stack 'BX -> Int -> IO b
peekOffBi bstk i = unwrapForeign . marshalToForeign <$> peekOff bstk i
{-# INLINE peekOffBi #-}

peekOffS :: Stack 'BX -> Int -> IO (Seq RClosure)
peekOffS bstk i =
  unwrapForeign . marshalToForeign <$> peekOff bstk i
{-# INLINE peekOffS #-}

pokeS :: Stack 'BX -> Seq RClosure -> IO ()
pokeS bstk s = poke bstk (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeS #-}

pokeOffS :: Stack 'BX -> Int -> Seq RClosure -> IO ()
pokeOffS bstk i s = pokeOff bstk i (Foreign $ Wrap Ty.listRef s)
{-# INLINE pokeOffS #-}

unull :: Seg 'UN
unull = byteArrayFromListN 0 ([] :: [Int])

bnull :: Seg 'BX
bnull = fromListN 0 []

instance Show (Stack 'BX) where
  show (BS ap fp sp _) =
    "BS " ++ show ap ++ " " ++ show fp ++ " " ++ show sp

instance Show (Stack 'UN) where
  show (US ap fp sp _) =
    "US " ++ show ap ++ " " ++ show fp ++ " " ++ show sp

instance Show K where
  show k = "[" ++ go "" k
    where
      go _ KE = "]"
      go _ (CB _) = "]"
      go com (Push uf bf ua ba ci k) =
        com ++ show (uf, bf, ua, ba, ci) ++ go "," k
      go com (Mark ua ba ps _ k) =
        com ++ "M " ++ show ua ++ " " ++ show ba ++ " " ++ show ps ++ go "," k

instance MEM 'BX where
  data Stack 'BX = BS
    { bap :: !Int,
      bfp :: !Int,
      bsp :: !Int,
      bstk :: {-# UNPACK #-} !(MutableArray (PrimState IO) RClosure)
    }
  type Elem 'BX = RClosure
  type Seg 'BX = Array RClosure

  alloc = BS (-1) (-1) (-1) <$> newArray 512 BlackHole
  {-# INLINE alloc #-}

  peek (BS _ _ sp stk) = readArray stk sp
  {-# INLINE peek #-}

  peekOff (BS _ _ sp stk) i = readArray stk (sp - i)
  {-# INLINE peekOff #-}

  poke (BS _ _ sp stk) x = writeArray stk sp x
  {-# INLINE poke #-}

  pokeOff (BS _ _ sp stk) i x = writeArray stk (sp - i) x
  {-# INLINE pokeOff #-}

  grab (BS _ fp sp stk) sz = do
    seg <- unsafeFreezeArray =<< cloneMutableArray stk (fp + 1 - sz) sz
    copyMutableArray stk (fp + 1 - sz) stk (fp + 1) fsz
    pure (seg, BS (fp - sz) (fp - sz) (sp - sz) stk)
    where
      fsz = sp - fp
  {-# INLINE grab #-}

  ensure stki@(BS ap fp sp stk) sz
    | sz <= 0 = pure stki
    | sp + sz + 1 < ssz = pure stki
    | otherwise = do
        stk' <- newArray (ssz + ext) BlackHole
        copyMutableArray stk' 0 stk 0 (sp + 1)
        pure $ BS ap fp sp stk'
    where
      ssz = sizeofMutableArray stk
      ext
        | sz > 1280 = sz + 512
        | otherwise = 1280
  {-# INLINE ensure #-}

  bump (BS ap fp sp stk) = pure $ BS ap fp (sp + 1) stk
  {-# INLINE bump #-}

  bumpn (BS ap fp sp stk) n = pure $ BS ap fp (sp + n) stk
  {-# INLINE bumpn #-}

  duplicate (BS ap fp sp stk) =
    BS ap fp sp <$> cloneMutableArray stk 0 (sizeofMutableArray stk)
  {-# INLINE duplicate #-}

  discardFrame (BS ap fp _ stk) = pure $ BS ap fp fp stk
  {-# INLINE discardFrame #-}

  saveFrame (BS ap fp sp stk) = pure (BS sp sp sp stk, sp - fp, fp - ap)
  {-# INLINE saveFrame #-}

  saveArgs (BS ap fp sp stk) = pure (BS fp fp sp stk, fp - ap)
  {-# INLINE saveArgs #-}

  restoreFrame (BS _ fp0 sp stk) fsz asz = pure $ BS ap fp sp stk
    where
      fp = fp0 - fsz
      ap = fp - asz
  {-# INLINE restoreFrame #-}

  prepareArgs (BS ap fp sp stk) (ArgR i l)
    | fp + i + l == sp = pure $ BS ap (sp - i) (sp - i) stk
  prepareArgs (BS ap fp sp stk) args = do
    sp <- bargOnto stk sp stk fp args
    pure $ BS ap sp sp stk
  {-# INLINE prepareArgs #-}

  acceptArgs (BS ap fp sp stk) n = pure $ BS ap (fp - n) sp stk
  {-# INLINE acceptArgs #-}

  frameArgs (BS ap _ sp stk) = pure $ BS ap ap sp stk
  {-# INLINE frameArgs #-}

  augSeg mode (BS ap fp sp stk) seg margs = do
    cop <- newArray (ssz + psz + asz) BlackHole
    copyArray cop soff seg 0 ssz
    copyMutableArray cop poff stk (ap + 1) psz
    for_ margs $ bargOnto stk sp cop (poff + psz - 1)
    unsafeFreezeArray cop
    where
      ssz = sizeofArray seg
      psz | I <- mode = 0 | otherwise = fp - ap
      (poff, soff)
        | K <- mode = (ssz, 0)
        | otherwise = (0, psz + asz)
      asz = case margs of
        Nothing -> 0
        Just (Arg1 _) -> 1
        Just (Arg2 _ _) -> 2
        Just (ArgN v) -> sizeofPrimArray v
        Just (ArgR _ l) -> l
  {-# INLINE augSeg #-}

  dumpSeg (BS ap fp sp stk) seg mode = do
    copyArray stk (sp + 1) seg 0 sz
    pure $ BS ap' fp' sp' stk
    where
      sz = sizeofArray seg
      sp' = sp + sz
      fp' = dumpFP fp sz mode
      ap' = dumpAP ap fp sz mode
  {-# INLINE dumpSeg #-}

  adjustArgs (BS ap fp sp stk) sz = pure $ BS (ap - sz) fp sp stk
  {-# INLINE adjustArgs #-}

  fsize (BS _ fp sp _) = sp - fp
  {-# INLINE fsize #-}

  asize (BS ap fp _ _) = fp - ap

frameView :: (MEM b) => (Show (Elem b)) => Stack b -> IO ()
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

uscount :: Seg 'UN -> Int
uscount seg = words $ sizeofByteArray seg

bscount :: Seg 'BX -> Int
bscount seg = sizeofArray seg

closureTermRefs :: (Monoid m) => (Reference -> m) -> (RClosure -> m)
closureTermRefs f (PAp (RComb (CIx r _ _) _) _ cs) =
  f r <> foldMap (closureTermRefs f) cs
closureTermRefs f (DataB1 _ _ c) = closureTermRefs f c
closureTermRefs f (DataB2 _ _ c1 c2) =
  closureTermRefs f c1 <> closureTermRefs f c2
closureTermRefs f (DataUB _ _ _ c) =
  closureTermRefs f c
closureTermRefs f (Captured k _ _ _ cs) =
  contTermRefs f k <> foldMap (closureTermRefs f) cs
closureTermRefs f (Foreign fo)
  | Just (cs :: Seq RClosure) <- maybeUnwrapForeign Ty.listRef fo =
      foldMap (closureTermRefs f) cs
closureTermRefs _ _ = mempty

contTermRefs :: (Monoid m) => (Reference -> m) -> K -> m
contTermRefs f (Mark _ _ _ m k) =
  foldMap (closureTermRefs f) m <> contTermRefs f k
contTermRefs f (Push _ _ _ _ (RComb (CIx r _ _) _) k) =
  f r <> contTermRefs f k
contTermRefs _ _ = mempty
