{-# language GADTs #-}
{-# language DataKinds #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Stack
  ( K(..)
  , Closure(.., DataC, PApV, CapV)
  , Callback(..)
  , Augment(..)
  , Dump(..)
  , MEM(..)
  , Stack(..)
  , Off
  , SZ
  , FP
  , universalEq
  , universalCompare
  , marshalToForeign
  , unull
  , bnull
  , peekD
  , peekOffD
  , pokeD
  , pokeOffD
  , peekN
  , peekOffN
  , pokeN
  , pokeOffN
  , peekBi
  , peekOffBi
  , pokeBi
  , pokeOffBi
  , peekOffS
  , pokeS
  , pokeOffS
  , frameView
  , uscount
  , bscount
  ) where

import Prelude hiding (words)

import GHC.Exts as L (IsList(..))

import Control.Monad (when)
import Control.Monad.Primitive

import Data.Ord (comparing)
import Data.Foldable (fold)

import Data.Foldable as F (toList, for_)
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Array
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
import Data.Word

import Unison.Reference (Reference)

import Unison.Runtime.ANF as ANF (Mem(..))
import Unison.Runtime.MCode
import Unison.Runtime.Foreign

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
         !CombIx -- local continuation reference
         !K
  deriving (Eq, Ord)

data Closure
  = PAp {-# unpack #-} !CombIx    -- reference
        {-# unpack #-} !(Seg 'UN) -- unboxed args
        {-  unpack  -} !(Seg 'BX) -- boxed args
  | Enum !Reference !Word64
  | DataU1 !Reference !Word64 !Int
  | DataU2 !Reference !Word64 !Int !Int
  | DataB1 !Reference !Word64 !Closure
  | DataB2 !Reference !Word64 !Closure !Closure
  | DataUB !Reference !Word64 !Int !Closure
  | DataG !Reference !Word64 !(Seg 'UN) !(Seg 'BX)
  | Captured !K {-# unpack #-} !(Seg 'UN) !(Seg 'BX)
  | Foreign !Foreign
  | BlackHole
  deriving (Show, Eq, Ord)

splitData :: Closure -> Maybe (Reference, Word64, [Int], [Closure])
splitData (Enum r t) = Just (r, t, [], [])
splitData (DataU1 r t i) = Just (r, t, [i], [])
splitData (DataU2 r t i j) = Just (r, t, [i,j], [])
splitData (DataB1 r t x) = Just (r, t, [], [x])
splitData (DataB2 r t x y) = Just (r, t, [], [x,y])
splitData (DataUB r t i y) = Just (r, t, [i], [y])
splitData (DataG r t us bs) = Just (r, t, ints us, reverse $ F.toList bs)
splitData _ = Nothing

ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [n-1,n-2..0]
  where
  n = sizeofByteArray ba `div` 8

useg :: [Int] -> Seg 'UN
useg ws = case L.fromList $ reverse ws of
  PrimArray ba -> ByteArray ba

formData :: Reference -> Word64 -> [Int] -> [Closure] -> Closure
formData r t [] [] = Enum r t
formData r t [i] [] = DataU1 r t i
formData r t [i,j] [] = DataU2 r t i j
formData r t [] [x] = DataB1 r t x
formData r t [] [x,y] = DataB2 r t x y
formData r t [i] [x] = DataUB r t i x
formData r t us bs = DataG r t (useg us) (L.fromList $ reverse bs)

pattern DataC :: Reference -> Word64 -> [Int] -> [Closure] -> Closure
pattern DataC rf ct us bs <- (splitData -> Just (rf, ct, us, bs))
  where
  DataC rf ct us bs = formData rf ct us bs

pattern PApV :: CombIx -> [Int] -> [Closure] -> Closure
pattern PApV ic us bs <- PAp ic (ints -> us) (L.toList -> bs)
  where
  PApV ic us bs = PAp ic (useg us) (L.fromList bs)

pattern CapV :: K -> [Int] -> [Closure] -> Closure
pattern CapV k us bs <- Captured k (ints -> us) (L.toList -> bs)
  where
  CapV k us bs = Captured k (useg us) (L.fromList bs)

{-# complete DataC, PAp, Captured, Foreign, BlackHole #-}
{-# complete DataC, PApV, Captured, Foreign, BlackHole #-}
{-# complete DataC, PApV, CapV, Foreign, BlackHole #-}

closureNum :: Closure -> Int
closureNum PAp{} = 0
closureNum DataC{} = 1
closureNum Captured{} = 2
closureNum Foreign{} = 3
closureNum BlackHole{} = error "BlackHole"

universalEq
  :: (Foreign -> Foreign -> Bool)
  -> Closure
  -> Closure
  -> Bool
universalEq frn = eqc False
  where
  eql cm l r = length l == length r && and (zipWith cm l r)
  eqc tyEq (DataC rf1 ct1 us1 bs1) (DataC rf2 ct2 us2 bs2)
    = (if tyEq then rf1 == rf2 else True)
   && ct1 == ct2
   && eql (==) us1 us2
   && eql (eqc $ tyEq || rf1 == Ty.anyRef) bs1 bs2
  eqc tyEq (PApV i1 us1 bs1) (PApV i2 us2 bs2)
    = i1 == i2
   && eql (==) us1 us2
   && eql (eqc tyEq) bs1 bs2
  eqc _ (CapV k1 us1 bs1) (CapV k2 us2 bs2)
    = k1 == k2
   && eql (==) us1 us2
   && eql (eqc True) bs1 bs2
  eqc tyEq (Foreign fl) (Foreign fr)
    | Just sl <- maybeUnwrapForeign Ty.listRef fl
    , Just sr <- maybeUnwrapForeign Ty.listRef fr
    = length sl == length sr && and (Sq.zipWith (eqc tyEq) sl sr)
    | otherwise = frn fl fr
  eqc _ c d = closureNum c == closureNum d


universalCompare
  :: (Foreign -> Foreign -> Ordering)
  -> Closure
  -> Closure
  -> Ordering
universalCompare frn = cmpc False
  where
  cmpl cm l r
    = compare (length l) (length r) <> fold (zipWith cm l r)
  cmpc tyEq (DataC rf1 ct1 us1 bs1) (DataC rf2 ct2 us2 bs2)
    = (if tyEq then compare rf1 rf2 else EQ)
   <> compare ct1 ct2
   <> cmpl compare us1 us2
   -- when comparing corresponding `Any` values, which have
   -- existentials inside check that type references match
   <> cmpl (cmpc $ tyEq || rf1 == Ty.anyRef) bs1 bs2
  cmpc tyEq (PApV i1 us1 bs1) (PApV i2 us2 bs2)
    = compare i1 i2
   <> cmpl compare us1 us2
   <> cmpl (cmpc tyEq) bs1 bs2
  cmpc _ (CapV k1 us1 bs1) (CapV k2 us2 bs2)
    = compare k1 k2
   <> cmpl compare us1 us2
   <> cmpl (cmpc True) bs1 bs2
  cmpc tyEq (Foreign fl) (Foreign fr)
    | Just sl <- maybeUnwrapForeign Ty.listRef fl
    , Just sr <- maybeUnwrapForeign Ty.listRef fr
    = comparing Sq.length sl sr <> fold (Sq.zipWith (cmpc tyEq) sl sr)
    | otherwise = frn fl fr
  cmpc _ c d = comparing closureNum c d

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

-- closure augmentation mode
-- instruction, kontinuation, call
data Augment = I | K | C

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
  augSeg :: Augment -> Stack b -> Seg b -> Maybe Args' -> IO (Seg b)
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

  augSeg mode (US ap fp sp stk) seg margs = do
    cop <- newByteArray $ ssz+psz+asz
    copyByteArray cop soff seg 0 ssz
    copyMutableByteArray cop 0 stk (bytes $ ap+1) psz
    for_ margs $ uargOnto stk sp cop (words poff + pix - 1)
    unsafeFreezeByteArray cop
   where
   ssz = sizeofByteArray seg
   pix | I <- mode = 0 | otherwise = fp-ap
   (poff,soff)
     | K <- mode = (ssz,0)
     | otherwise = (0,psz+asz)
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

pokeBi :: BuiltinForeign b => Stack 'BX -> b -> IO ()
pokeBi bstk x = poke bstk (Foreign $ wrapBuiltin x)
{-# inline pokeBi #-}

pokeOffBi :: BuiltinForeign b => Stack 'BX -> Int -> b -> IO ()
pokeOffBi bstk i x = pokeOff bstk i (Foreign $ wrapBuiltin x)
{-# inline pokeOffBi #-}

peekBi :: BuiltinForeign b => Stack 'BX -> IO b
peekBi bstk = unwrapForeign . marshalToForeign <$> peek bstk
{-# inline peekBi #-}

peekOffBi :: BuiltinForeign b => Stack 'BX -> Int -> IO b
peekOffBi bstk i = unwrapForeign . marshalToForeign <$> peekOff bstk i
{-# inline peekOffBi #-}

peekOffS :: Stack 'BX -> Int -> IO (Seq Closure)
peekOffS bstk i =
  unwrapForeign . marshalToForeign <$> peekOff bstk i
{-# inline peekOffS #-}

pokeS :: Stack 'BX -> Seq Closure -> IO ()
pokeS bstk s = poke bstk (Foreign $ Wrap Ty.listRef s)
{-# inline pokeS #-}

pokeOffS :: Stack 'BX -> Int -> Seq Closure -> IO ()
pokeOffS bstk i s = pokeOff bstk i (Foreign $ Wrap Ty.listRef s)
{-# inline pokeOffS #-}

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

  augSeg mode (BS ap fp sp stk) seg margs = do
    cop <- newArray (ssz+psz+asz) BlackHole
    copyArray cop soff seg 0 ssz
    copyMutableArray cop poff stk (ap+1) psz
    for_ margs $ bargOnto stk sp cop (poff+psz-1)
    unsafeFreezeArray cop
   where
   ssz = sizeofArray seg
   psz | I <- mode = 0 | otherwise = fp-ap
   (poff,soff)
     | K <- mode = (ssz,0)
     | otherwise = (0,psz+asz)
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

frameView :: MEM b => Show (Elem b) => Stack b -> IO ()
frameView stk = putStr "|" >> gof False 0
  where
  fsz = fsize stk
  asz = asize stk
  gof delim n
    | n >= fsz = putStr "|" >> goa False 0
    | otherwise = do
      when delim $ putStr ","
      putStr . show =<< peekOff stk n
      gof True (n+1)
  goa delim n
    | n >= asz = putStrLn "|.."
    | otherwise = do
      when delim $ putStr ","
      putStr . show =<< peekOff stk (fsz+n)
      goa True (n+1)

uscount :: Seg 'UN -> Int
uscount seg = words $ sizeofByteArray seg

bscount :: Seg 'BX -> Int
bscount seg = sizeofArray seg

