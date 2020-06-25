{-# language GADTs #-}
{-# language DataKinds #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Stack where

import Prelude hiding (words)

import Control.Monad (when)
import Control.Monad.Primitive

import Data.Foldable (toList, for_)
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Array
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word

import Unison.Runtime.ANF (Mem(..), unpackTags)
import Unison.Runtime.Foreign
import Unison.Runtime.MCode

import qualified Unison.Type as Ty

import Unison.Util.EnumContainers as EC

import GHC.Stack (HasCallStack)

newtype Callback = Hook (Stack 'UN -> Stack 'BX -> IO ())

instance Eq Callback where _ == _ = True
instance Ord Callback where compare _ _ = EQ

-- Evaluation stack
data K
  = KE
  -- callback hook
  | CB Callback
  -- mark continuation with a prompt
  | Mark !(EnumSet Word64)
         !(EnumMap Word64 Closure)
         !K
  -- save information about a frame for later resumption
  | Push !Int -- unboxed frame size
         !Int -- boxed frame size
         !Int -- pending unboxed args
         !Int -- pending boxed args
         !Section -- code
         !K
  deriving (Eq, Ord)

-- Comb with an identifier
data IComb
  = IC !Word64 !Comb
  deriving (Show)

instance Eq IComb where
  IC i _ == IC j _ = i == j

pattern Lam_ ua ba uf bf entry <- IC _ (Lam ua ba uf bf entry)

-- TODO: more reliable ordering for combinators
instance Ord IComb where
  compare (IC i _) (IC j _) = compare i j

data Closure
  = PAp {-# unpack #-} !IComb     -- code
        {-# unpack #-} !(Seg 'UN) -- unboxed args
        {-  unpack  -} !(Seg 'BX) -- boxed args
  | Enum !Word64
  | DataU1 !Word64 !Int
  | DataU2 !Word64 !Int !Int
  | DataB1 !Word64 !Closure
  | DataB2 !Word64 !Closure !Closure
  | DataUB !Word64 !Int !Closure
  | DataG !Word64 !(Seg 'UN) !(Seg 'BX)
  | Captured !K {-# unpack #-} !(Seg 'UN) !(Seg 'BX)
  | Foreign !Foreign
  | BlackHole
  deriving (Show, Eq, Ord)

splitData :: Closure -> Maybe (Word64, [Int], [Closure])
splitData (Enum t) = Just (t, [], [])
splitData (DataU1 t i) = Just (t, [i], [])
splitData (DataU2 t i j) = Just (t, [i,j], [])
splitData (DataB1 t x) = Just (t, [], [x])
splitData (DataB2 t x y) = Just (t, [], [x,y])
splitData (DataUB t i y) = Just (t, [i], [y])
splitData (DataG t us bs) = Just (t, ints us, toList bs)
splitData _ = Nothing

ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [0..n]
  where
  n = sizeofByteArray ba `div` 8

pattern DataC rt ct us bs <-
  (splitData -> Just (unpackTags -> (rt, ct), us, bs))

pattern PApV ic us bs <- PAp ic (ints -> us) (toList -> bs)

{-# complete DataC, PAp, Captured, Foreign, BlackHole #-}
{-# complete DataC, PApV, Captured, Foreign, BlackHole #-}

marshalToForeign :: HasCallStack => Closure -> Foreign
marshalToForeign (Foreign x) = x
marshalToForeign c
  = error $ "marshalToForeign: unhandled closure: " ++ show c

type Off = Int
type SZ = Int
type FP = Int

type UA = MutableByteArray (PrimState IO)
type BA = MutableArray (PrimState IO) Closure

words :: Int -> Int
words n = n `div` 8

bytes :: Int -> Int
bytes n = n * 8

uargOnto :: UA -> Off -> UA -> Off -> Args' -> IO Int
uargOnto stk sp cop cp0 (Arg1 i) = do
  (x :: Int) <- readByteArray stk (sp-i)
  writeByteArray cop cp x
  pure cp
 where cp = cp0+1
uargOnto stk sp cop cp0 (Arg2 i j) = do
  (x :: Int) <- readByteArray stk (sp-i)
  (y :: Int) <- readByteArray stk (sp-j)
  writeByteArray cop cp x
  writeByteArray cop (cp-1) y
  pure cp
 where cp = cp0+2
uargOnto stk sp cop cp0 (ArgN v) = do
  buf <- if overwrite
         then newByteArray $ bytes sz
         else pure cop
  let loop i
        | i < 0     = return ()
        | otherwise = do
            (x :: Int) <- readByteArray stk (sp-indexPrimArray v i)
            writeByteArray buf (sz-1-i) x
            loop $ i-1
  loop $ sz-1
  when overwrite $
    copyMutableByteArray cop (bytes $ cp+1) buf 0 (bytes sz)
  pure cp
 where
 cp = cp0+sz
 sz = sizeofPrimArray v
 overwrite = sameMutableByteArray stk cop
uargOnto stk sp cop cp0 (ArgR i l) = do
  moveByteArray cop cbp stk sbp (bytes l)
  pure $ cp0+l
 where
 cbp = bytes $ cp0+1
 sbp = bytes $ sp-i-l+1

bargOnto :: BA -> Off -> BA -> Off -> Args' -> IO Int
bargOnto stk sp cop cp0 (Arg1 i) = do
  x <- readArray stk (sp-i)
  writeArray cop cp x
  pure cp
 where cp = cp0+1
bargOnto stk sp cop cp0 (Arg2 i j) = do
  x <- readArray stk (sp-i)
  y <- readArray stk (sp-j)
  writeArray cop cp x
  writeArray cop (cp-1) y
  pure cp
 where cp = cp0+2
bargOnto stk sp cop cp0 (ArgN v) = do
  buf <- if overwrite
         then newArray sz BlackHole
         else pure cop
  let loop i
        | i < 0     = return ()
        | otherwise = do
            x <- readArray stk $ sp-indexPrimArray v i
            writeArray buf (sz-1-i) x
            loop $ i-1
  loop $ sz-1
  when overwrite $
    copyMutableArray cop (cp0+1) buf 0 sz
  pure cp
 where
 cp = cp0+sz
 sz = sizeofPrimArray v
 overwrite = stk == cop
bargOnto stk sp cop cp0 (ArgR i l) = do
  copyMutableArray cop (cp0+1) stk (sp-i-l+1) l
  pure $ cp0+l

data Dump = A | F Int | S

dumpAP :: Int -> Int -> Int -> Dump -> Int
dumpAP _  fp sz d@(F _) = dumpFP fp sz d
dumpAP ap _  _  _     = ap

dumpFP :: Int -> Int -> Dump -> Int
dumpFP fp _  S = fp
dumpFP fp sz A = fp+sz
dumpFP fp sz (F n) = fp+sz-n

class MEM (b :: Mem) where
  data Stack b :: *
  type Elem b :: *
  type Seg b :: *
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
  restoreFrame :: Stack b -> SZ -> SZ -> IO (Stack b)
  prepareArgs :: Stack b -> Args' -> IO (Stack b)
  acceptArgs :: Stack b -> Int -> IO (Stack b)
  frameArgs :: Stack b -> IO (Stack b)
  augSeg :: Bool -> Stack b -> Seg b -> Maybe Args' -> IO (Seg b)
  dumpSeg :: Stack b -> Seg b -> Dump -> IO (Stack b)
  fsize :: Stack b -> SZ
  asize :: Stack b -> SZ

instance MEM 'UN where
  data Stack 'UN
    -- Note: uap <= ufp <= usp
    = US { uap  :: !Int -- arg pointer
         , ufp  :: !Int -- frame pointer
         , usp  :: !Int -- stack pointer
         , ustk :: {-# unpack #-} !(MutableByteArray (PrimState IO))
         }
  type Elem 'UN = Int
  type Seg 'UN = ByteArray
  alloc = US (-1) (-1) (-1) <$> newByteArray 4096
  {-# inline alloc #-}
  peek (US _ _ sp stk) = readByteArray stk sp
  {-# inline peek #-}
  peekOff (US _ _ sp stk) i = readByteArray stk (sp-i)
  {-# inline peekOff #-}
  poke (US _ _ sp stk) n = writeByteArray stk sp n
  {-# inline poke #-}
  pokeOff (US _ _ sp stk) i n = writeByteArray stk (sp-i) n
  {-# inline pokeOff #-}

  -- Eats up arguments
  grab (US _ fp sp stk) sze = do
    mut <- newByteArray sz
    copyMutableByteArray mut 0 stk (bfp-sz) sz
    seg <- unsafeFreezeByteArray mut
    moveByteArray stk (bfp-sz) stk bfp fsz
    pure (seg, US (fp-sze) (fp-sze) (sp-sze) stk)
   where
   sz = bytes sze
   bfp = bytes $ fp+1
   fsz = bytes $ sp-fp
  {-# inline grab #-}

  ensure stki@(US ap fp sp stk) sze
    | sze <= 0
    || bytes (sp+sze+1) < ssz = pure stki
    | otherwise = do
      stk' <- resizeMutableByteArray stk (ssz+10240)
      pure $ US ap fp sp stk'
   where
   ssz = sizeofMutableByteArray stk
  {-# inline ensure #-}

  bump (US ap fp sp stk) = pure $ US ap fp (sp+1) stk
  {-# inline bump #-}

  bumpn (US ap fp sp stk) n = pure $ US ap fp (sp+n) stk
  {-# inline bumpn #-}

  duplicate (US ap fp sp stk)
    = US ap fp sp <$> do
        b <- newByteArray sz
        copyMutableByteArray b 0 stk 0 sz
        pure b
    where
    sz = sizeofMutableByteArray stk
  {-# inline duplicate #-}

  discardFrame (US ap fp _ stk) = pure $ US ap fp fp stk
  {-# inline discardFrame #-}

  saveFrame (US ap fp sp stk) = pure (US sp sp sp stk, sp-fp, fp-ap)
  {-# inline saveFrame #-}

  restoreFrame (US _ fp0 sp stk) fsz asz = pure $ US ap fp sp stk
   where fp = fp0-fsz
         ap = fp-asz
  {-# inline restoreFrame #-}

  prepareArgs (US ap fp sp stk) (ArgR i l)
    | fp+l+i == sp = pure $ US ap (sp-i) (sp-i) stk
  prepareArgs (US ap fp sp stk) args = do
    sp <- uargOnto stk sp stk fp args
    pure $ US ap sp sp stk
  {-# inline prepareArgs #-}

  acceptArgs (US ap fp sp stk) n = pure $ US ap (fp-n) sp stk
  {-# inline acceptArgs #-}

  frameArgs (US ap _ sp stk) = pure $ US ap ap sp stk
  {-# inline frameArgs #-}

  augSeg pend (US ap fp sp stk) seg margs = do
    cop <- newByteArray $ ssz+psz+asz
    when pend $ copyByteArray cop (psz+asz) seg 0 ssz
    copyMutableByteArray cop 0 stk ap psz
    for_ margs $ uargOnto stk sp cop (pix-1)
    unsafeFreezeByteArray cop
   where
   ssz = sizeofByteArray seg
   -- pending
   pix | pend = fp-ap | otherwise = 0
   psz = bytes pix
   asz = case margs of
          Nothing         -> 0
          Just (Arg1 _)   -> 8
          Just (Arg2 _ _) -> 16
          Just (ArgN v)   -> bytes $ sizeofPrimArray v
          Just (ArgR _ l) -> bytes l
  {-# inline augSeg #-}

  dumpSeg (US ap fp sp stk) seg mode = do
    copyByteArray stk bsp seg 0 ssz
    pure $ US ap' fp' sp' stk
   where
   bsp = bytes $ sp+1
   ssz = sizeofByteArray seg
   sz = words ssz
   sp' = sp+sz
   fp' = dumpFP fp sz mode
   ap' = dumpAP ap fp sz mode
  {-# inline dumpSeg #-}

  fsize (US _ fp sp _) = sp-fp
  {-# inline fsize #-}

  asize (US ap fp _ _) = fp-ap
  {-# inline asize #-}

peekN :: Stack 'UN -> IO Word64
peekN (US _ _ sp stk) = readByteArray stk sp
{-# inline peekN #-}

peekD :: Stack 'UN -> IO Double
peekD (US _ _ sp stk) = readByteArray stk sp
{-# inline peekD #-}

peekOffN :: Stack 'UN -> Int -> IO Word64
peekOffN (US _ _ sp stk) i = readByteArray stk (sp-i)
{-# inline peekOffN #-}

peekOffD :: Stack 'UN -> Int -> IO Double
peekOffD (US _ _ sp stk) i = readByteArray stk (sp-i)
{-# inline peekOffD #-}

pokeN :: Stack 'UN -> Word64 -> IO ()
pokeN (US _ _ sp stk) n = writeByteArray stk sp n
{-# inline pokeN #-}

pokeD :: Stack 'UN -> Double -> IO ()
pokeD (US _ _ sp stk) d = writeByteArray stk sp d
{-# inline pokeD #-}

pokeOffN :: Stack 'UN -> Int -> Word64 -> IO ()
pokeOffN (US _ _ sp stk) i n = writeByteArray stk (sp-i) n
{-# inline pokeOffN #-}

pokeOffD :: Stack 'UN -> Int -> Double -> IO ()
pokeOffD (US _ _ sp stk) i d = writeByteArray stk (sp-i) d
{-# inline pokeOffD #-}

peekOffT :: Stack 'BX -> Int -> IO Text
peekOffT bstk i =
  unwrapForeign . marshalToForeign <$> peekOff bstk i
{-# inline peekOffT #-}

pokeT :: Stack 'BX -> Text -> IO ()
pokeT bstk t = poke bstk (Foreign $ wrapText t)
{-# inline pokeT #-}

peekOffS :: Stack 'BX -> Int -> IO (Seq Closure)
peekOffS bstk i =
  unwrapForeign . marshalToForeign <$> peekOff bstk i
{-# inline peekOffS #-}

pokeS :: Stack 'BX -> Seq Closure -> IO ()
pokeS bstk s = poke bstk (Foreign $ Wrap Ty.vectorRef s)
{-# inline pokeS #-}

unull :: Seg 'UN
unull = byteArrayFromListN 0 ([] :: [Int])

bnull :: Seg 'BX
bnull = fromListN 0 []

instance Show (Stack 'BX) where
  show (BS ap fp sp _)
    = "BS " ++ show ap ++ " " ++ show fp ++ " " ++ show sp
instance Show (Stack 'UN) where
  show (US ap fp sp _)
    = "US " ++ show ap ++ " " ++ show fp ++ " " ++ show sp
instance Show K where
  show k = "[" ++ go "" k
    where
    go _ KE = "]"
    go _ (CB _) = "]"
    go com (Push uf bf ua ba _ k)
      = com ++ show (uf,bf,ua,ba) ++ go "," k
    go com (Mark ps _ k) = com ++ "M" ++ show ps ++ go "," k

instance MEM 'BX where
  data Stack 'BX
    = BS { bap :: !Int
         , bfp :: !Int
         , bsp :: !Int
         , bstk :: {-# unpack #-} !(MutableArray (PrimState IO) Closure)
         }
  type Elem 'BX = Closure
  type Seg 'BX = Array Closure

  alloc = BS (-1) (-1) (-1) <$> newArray 512 BlackHole
  {-# inline alloc #-}

  peek (BS _ _ sp stk) = readArray stk sp
  {-# inline peek #-}

  peekOff (BS _ _ sp stk) i = readArray stk (sp-i)
  {-# inline peekOff #-}

  poke (BS _ _ sp stk) x = writeArray stk sp x
  {-# inline poke #-}

  pokeOff (BS _ _ sp stk) i x = writeArray stk (sp-i) x
  {-# inline pokeOff #-}

  grab (BS _ fp sp stk) sz = do
    seg <- unsafeFreezeArray =<< cloneMutableArray stk (fp+1-sz) sz
    copyMutableArray stk (fp+1-sz) stk (fp+1) fsz
    pure (seg, BS (fp-sz) (fp-sz) (sp-sz) stk)
   where fsz = sp-fp
  {-# inline grab #-}

  ensure stki@(BS ap fp sp stk) sz
    | sz <= 0 = pure stki
    | sp+sz+1 < ssz = pure stki
    | otherwise = do
      stk' <- newArray (ssz+1280) BlackHole
      copyMutableArray stk' 0 stk 0 (sp+1)
      pure $ BS ap fp sp stk'
    where ssz = sizeofMutableArray stk
  {-# inline ensure #-}

  bump (BS ap fp sp stk) = pure $ BS ap fp (sp+1) stk
  {-# inline bump #-}

  bumpn (BS ap fp sp stk) n = pure $ BS ap fp (sp+n) stk
  {-# inline bumpn #-}

  duplicate (BS ap fp sp stk)
    = BS ap fp sp <$> cloneMutableArray stk 0 (sizeofMutableArray stk)
  {-# inline duplicate #-}

  discardFrame (BS ap fp _ stk) = pure $ BS ap fp fp stk
  {-# inline discardFrame #-}

  saveFrame (BS ap fp sp stk) = pure (BS sp sp sp stk, sp-fp, fp-ap)
  {-# inline saveFrame #-}

  restoreFrame (BS _ fp0 sp stk) fsz asz = pure $ BS ap fp sp stk
   where
   fp = fp0-fsz
   ap = fp-asz
  {-# inline restoreFrame #-}

  prepareArgs (BS ap fp sp stk) (ArgR i l)
    | fp+i+l == sp = pure $ BS ap (sp-i) (sp-i) stk
  prepareArgs (BS ap fp sp stk) args = do
    sp <- bargOnto stk sp stk fp args
    pure $ BS ap sp sp stk
  {-# inline prepareArgs #-}

  acceptArgs (BS ap fp sp stk) n = pure $ BS ap (fp-n) sp stk
  {-# inline acceptArgs #-}

  frameArgs (BS ap _ sp stk) = pure $ BS ap ap sp stk
  {-# inline frameArgs #-}

  augSeg pend (BS ap fp sp stk) seg margs = do
    cop <- newArray (ssz+psz+asz) BlackHole
    copyArray cop (psz+asz) seg 0 ssz
    when pend $ copyMutableArray cop 0 stk ap psz
    for_ margs $ bargOnto stk sp cop (psz-1)
    unsafeFreezeArray cop
   where
   ssz = sizeofArray seg
   psz | pend = fp-ap | otherwise = 0
   asz = case margs of
          Nothing -> 0
          Just (Arg1 _)   -> 1
          Just (Arg2 _ _) -> 2
          Just (ArgN v)   -> sizeofPrimArray v
          Just (ArgR _ l) -> l
  {-# inline augSeg #-}

  dumpSeg (BS ap fp sp stk) seg mode = do
    copyArray stk (sp+1) seg 0 sz
    pure $ BS ap' fp' sp' stk
   where
   sz = sizeofArray seg
   sp' = sp+sz
   fp' = dumpFP fp sz mode
   ap' = dumpAP ap fp sz mode
  {-# inline dumpSeg #-}

  fsize (BS _ fp sp _) = sp-fp
  {-# inline fsize #-}

  asize (BS ap fp _ _) = fp-ap

uscount :: Seg 'UN -> Int
uscount seg = words $ sizeofByteArray seg

bscount :: Seg 'BX -> Int
bscount seg = sizeofArray seg

