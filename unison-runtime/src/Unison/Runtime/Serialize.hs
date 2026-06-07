{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.Serialize where

import Control.Monad (replicateM)
import Control.Monad.Primitive
import Data.Bits (Bits, setBit, shiftL, shiftR, (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.Bytes.Signed
import Data.Int (Int64)
import Data.Map.Strict as Map (Map, fromList, toList)
import Data.Primitive.Array
  ( Array,
    indexArray,
    sizeofArray,
  )
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64, Word8)
import Numeric.Natural (Natural)
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Reference (Id' (..), Reference, Reference' (Builtin, DerivedId), pattern Derived)
import Unison.Referent (Referent, pattern Con, pattern Ref)
import Unison.ReferentPrime (Referent' (..))
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Canonicalizer
import Unison.Runtime.Exception (exn)
import Unison.Runtime.MCode
  ( Prim1 (..),
    Prim2 (..),
  )
import Unison.Runtime.Referenced (RefNum (..))
import Unison.Runtime.Serialize.Get as Get
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.EnumContainers as EC
import Prelude hiding (getChar)

unknownTag :: (PrimBase m) => String -> Word8 -> Get m a
unknownTag t w =
  remaining >>= \r ->
    exn [] $
      "unknown "
        ++ t
        ++ " word: "
        ++ show w
        ++ " ("
        ++ show (fromIntegral @_ @Int r)
        ++ " bytes remaining)"

class Tag t where
  tag2word :: t -> Word8
  word2tag :: (PrimBase m) => Word8 -> Get m t

putTag :: (Tag t) => t -> Builder
putTag = BU.word8 . tag2word
{-# INLINE putTag #-}

getTag :: (PrimBase m) => (Tag t) => Get m t
getTag = word2tag =<< getWord8
{-# INLINE getTag #-}

putVarInt ::
  (Integral a, Integral (Unsigned a), Bits (Unsigned a)) => a -> Builder
putVarInt = go . unsigned
  where
    go n
      | n < 0x80 = BU.word8 $ fromIntegral n
      | otherwise =
          BU.word8 (setBit (fromIntegral n) 7)
            <> go (shiftR n 7)
{-# INLINE putVarInt #-}

-- Some basics, moved over from V1 serialization
putChar :: Char -> Builder
putChar = putVarInt . fromEnum

getChar :: (PrimBase m) => Get m Char
getChar = toEnum <$> getVarInt
{-# INLINEABLE getChar #-}

putFloat :: Double -> Builder
putFloat = BU.doubleBE

getFloat :: (PrimBase m) => Get m Double
getFloat = getDoublebe
{-# INLINE getFloat #-}

putBool :: Bool -> Builder
putBool b = BU.word8 (if b then 1 else 0)

getBool :: (PrimBase m) => Get m Bool
getBool = d =<< getWord8
  where
    d 0 = pure False
    d 1 = pure True
    d n = exn [] $ "getBool: bad tag: " ++ show n
{-# INLINE getBool #-}

putNat :: Word64 -> Builder
putNat = BU.word64BE

getNat :: (PrimBase m) => Get m Word64
getNat = getWord64be
{-# INLINE getNat #-}

putInt :: Int64 -> Builder
putInt = BU.int64BE

getInt :: (PrimBase m) => Get m Int64
getInt = getInt64be
{-# INLINE getInt #-}

putLength ::
  ( Integral n,
    Integral (Unsigned n),
    Bits n,
    Bits (Unsigned n)
  ) =>
  n ->
  Builder
putLength = putVarInt
{-# INLINE putLength #-}

getLength ::
  ( PrimBase m,
    Integral n,
    Integral (Unsigned n),
    Bits n,
    Bits (Unsigned n)
  ) =>
  Get m n
getLength = getVarInt
{-# INLINE getLength #-}

-- Checks for negatives, in case you put an Integer, which does not
-- behave properly for negative numbers.
putPositive ::
  (Bits n, Bits (Unsigned n), Integral n, Integral (Unsigned n)) =>
  n ->
  Builder
putPositive n
  | n < 0 = exn [] $ "putPositive: negative number: " ++ show (toInteger n)
  | otherwise = putVarInt n
{-# INLINE putPositive #-}

-- Reads as an Integer, then checks that the result will fit in the
-- result type.
getPositive :: forall m n. (Bounded n, Integral n, PrimBase m) => Get m n
getPositive = validate =<< getVarInt
  where
    mx0 :: n
    mx0 = maxBound
    mx :: Integer
    mx = fromIntegral mx0

    validate :: Integer -> Get m n
    validate n
      | n <= mx = pure $ fromIntegral n
      | otherwise = fail $ "getPositive: overflow: " ++ show n
{-# INLINE getPositive #-}

putFoldable ::
  (Foldable f) => (a -> Builder) -> f a -> Builder
putFoldable putA as =
  putLength (length as) <> foldr (\x b -> putA x <> b) mempty as
{-# INLINE putFoldable #-}

putMap :: (a -> Builder) -> (b -> Builder) -> Map a b -> Builder
putMap putA putB m = putMapping putA putB $ Map.toList m

-- TODO: switch to MapBuilder when containers gets updated
getMap :: (PrimBase m, Ord a) => Get m a -> Get m b -> Get m (Map a b)
getMap getA getB =
  getAccumulatingRevList (Map.fromList . reverse) (getPair getA getB)
{-# INLINEABLE getMap #-}

putMapping :: (a -> Builder) -> (b -> Builder) -> [(a, b)] -> Builder
putMapping putA putB = putFoldable (putPair putA putB)
{-# INLINE putMapping #-}

getMapping :: (PrimBase m) => Get m a -> Get m b -> Get m [(a, b)]
getMapping getA getB = getList (getPair getA getB)
{-# INLINE getMapping #-}

putEnumMap ::
  (EnumKey k) =>
  (k -> Builder) ->
  (v -> Builder) ->
  EnumMap k v ->
  Builder
putEnumMap pk pv m = putFoldable (putPair pk pv) (mapToList m)

getEnumMap :: (PrimBase m) => (EnumKey k) => Get m k -> Get m v -> Get m (EnumMap k v)
getEnumMap gk gv = mapFromList <$> getList (getPair gk gv)

putEnumSet :: (EnumKey k) => (k -> Builder) -> EnumSet k -> Builder
putEnumSet pk s =
  putLength (setSize s) <> foldrSet (\k b -> pk k <> b) mempty s

getEnumSet :: (PrimBase m) => (EnumKey k) => Get m k -> Get m (EnumSet k)
getEnumSet gk = setFromList <$> getList gk

putMaybe :: Maybe a -> (a -> Builder) -> Builder
putMaybe Nothing _ = BU.word8 0
putMaybe (Just a) putA = BU.word8 1 <> putA a

getMaybe :: (PrimBase m) => Get m a -> Get m (Maybe a)
getMaybe getA =
  getWord8 >>= \tag -> case tag of
    0 -> pure Nothing
    1 -> Just <$> getA
    _ -> unknownTag "Maybe" tag
{-# INLINE getMaybe #-}

putPair :: (a -> Builder) -> (b -> Builder) -> (a, b) -> Builder
putPair putA putB (a, b) = putA a <> putB b
{-# INLINE putPair #-}

getPair :: (PrimBase m) => Get m a -> Get m b -> Get m (a, b)
getPair = liftA2 (,)
{-# INLINE getPair #-}

getBytes :: (PrimBase m) => Get m Bytes.Bytes
getBytes = Bytes.fromChunks <$> getList getBlock
{-# INLINE getBytes #-}

putBytes :: Bytes.Bytes -> Builder
putBytes = putFoldable putBlock . Bytes.chunks

getByteArray :: (PrimBase m) => Get m PA.ByteArray
getByteArray = PA.byteArrayFromList <$> getList getWord8

putByteArray :: PA.ByteArray -> Builder
putByteArray a =
  putLength (PA.sizeofByteArray a)
    <> BU.shortByteString (PA.byteArrayToShortByteString a)

-- Convert a Natural to a list of Word64 chunks (most significant first)
naturalToWord64s :: Natural -> [Word64]
naturalToWord64s = go []
  where
    go !acc 0 = acc
    go !acc n = go (fromIntegral (n `mod` (2 ^ (64 :: Int))) : acc) (n `shiftR` 64)

-- Convert a list of Word64 chunks (most significant first) back to Natural
word64sToNatural :: [Word64] -> Natural
word64sToNatural = foldl' (\acc w -> acc `shiftL` 64 .|. fromIntegral w) 0

-- Serialize a Natural as a length-prefixed list of VarInt-encoded Word64 chunks
putNatural :: Natural -> Builder
putNatural n = putLength (length chunks) <> foldMap putVarInt chunks
  where
    chunks = naturalToWord64s n

-- Deserialize a Natural from a list of VarInt-encoded Word64 chunks
getNatural :: (PrimBase m) => Get m Natural
getNatural = do
  len <- getLength
  chunks <- replicateM len getVarInt
  pure $ word64sToNatural chunks

-- Serialize an Integer as a sign byte followed by the Natural magnitude
putInteger :: Integer -> Builder
putInteger n
  | n >= 0 = BU.word8 0 <> putNatural (fromInteger n)
  | otherwise = BU.word8 1 <> putNatural (fromInteger (abs n))

-- Deserialize an Integer
getInteger :: (PrimBase m) => Get m Integer
getInteger = do
  sign <- getWord8
  mag <- getNatural
  pure $ if sign == 0 then toInteger mag else negate (toInteger mag)

putArray :: (a -> Builder) -> PA.Array a -> Builder
putArray putThing a = putLength sz <> go 0
  where
    sz = sizeofArray a
    go i
      | i < sz = putThing (indexArray a i) <> go (i + 1)
      | otherwise = mempty
{-# INLINE putArray #-}

getBlock :: (PrimBase m) => Get m Bytes.Chunk
getBlock = getLength >>= fmap Bytes.byteStringToChunk . getByteString

putBlock :: Bytes.Chunk -> Builder
putBlock b = putLength (Bytes.chunkSize b) <> BU.byteString (Bytes.chunkToByteString b)

putHash :: Hash -> Builder
putHash h = putLength (B.length bs) <> BU.byteString bs
  where
    bs = Hash.toByteString h
{-# INLINE putHash #-}

getHash :: (PrimBase m) => Get m Hash
getHash = do
  len <- getLength
  bs <- getByteString len
  pure $ Hash.fromByteString bs
{-# INLINE getHash #-}

putReferent :: Referent -> Builder
putReferent = \case
  Ref r ->
    BU.word8 0
      <> putReference r
  Con r ct ->
    BU.word8 1
      <> putConstructorReference r
      <> putConstructorType ct

getReferent :: (PrimBase m) => Get m Referent
getReferent = do
  tag <- getWord8
  case tag of
    0 -> Ref <$> getReference
    1 -> Con <$> getConstructorReference <*> getConstructorType
    _ -> unknownTag "getReferent" tag

-- Arguments for getting/putting references by numbering rather than
-- inline. For getting, the format is numbered, so we just need an
-- array of references. For putting, the assumption is that every
-- `Reference` has been resolved to a unique object in memory, so that
-- we can look them up by stable name.
type GetRefLookup = (Array Reference, Array Reference)

type PutRefLookup = (CanonMap Reference Int, CanonMap Reference Int)

putReferentByNumber :: PutRefLookup -> Referent -> Builder
putReferentByNumber (tys, tms) = \case
  Ref r ->
    BU.word8 0
      <> putReferenceByNumber tms r
  Con r ct ->
    BU.word8 1
      <> putConstructorReferenceByNumber tys r
      <> putConstructorType ct

putNumberedReferent :: Referent' RefNum -> Builder
putNumberedReferent = \case
  Ref' r ->
    BU.word8 0
      <> putRefNum r
  Con' r ct ->
    BU.word8 1
      <> putNumberedConstructorReference r
      <> putConstructorType ct

getReferentByNumber :: (PrimBase m) => GetRefLookup -> Get m Referent
getReferentByNumber (tys, tms) = do
  tag <- getWord8
  case tag of
    0 -> Ref <$> getReferenceByNumber tms
    1 -> Con <$> getConstructorReferenceByNumber tys <*> getConstructorType
    _ -> unknownTag "getReferent" tag

getNumberedReferent :: (PrimBase m) => Get m (Referent' RefNum)
getNumberedReferent =
  getWord8 >>= \case
    0 -> Ref' <$> getRefNum
    1 -> Con' <$> getNumberedConstructorReference <*> getConstructorType
    tag -> unknownTag "getNumberedReferent" tag

getConstructorType :: (PrimBase m) => Get m CT.ConstructorType
getConstructorType =
  getWord8 >>= \case
    0 -> pure CT.Data
    1 -> pure CT.Effect
    t -> unknownTag "getConstructorType" t

putConstructorType :: CT.ConstructorType -> Builder
putConstructorType = \case
  CT.Data -> BU.word8 0
  CT.Effect -> BU.word8 1

putConstructorReferenceByNumber ::
  CanonMap Reference Int -> ConstructorReference -> Builder
putConstructorReferenceByNumber tys (ConstructorReference r i) =
  putReferenceByNumber tys r <> putLength i

getConstructorReferenceByNumber ::
  (PrimBase m) => Array Reference -> Get m ConstructorReference
getConstructorReferenceByNumber tys =
  ConstructorReference <$> getReferenceByNumber tys <*> getLength

putNumberedConstructorReference ::
  GConstructorReference RefNum -> Builder
putNumberedConstructorReference (ConstructorReference r i) =
  putRefNum r <> putLength i

getNumberedConstructorReference ::
  (PrimBase m) => Get m (GConstructorReference RefNum)
getNumberedConstructorReference =
  ConstructorReference <$> getRefNum <*> getLength

putString :: String -> Builder
putString = putFoldable (putVarInt . fromEnum)

getString :: (PrimBase m) => Get m String
getString = getList (toEnum <$> getVarInt)

putText :: Text -> Builder
putText text = putLength (B.length bs) <> BU.byteString bs
  where
    bs = encodeUtf8 text
{-# INLINE putText #-}

getText :: (PrimBase m) => Get m Text
getText = do
  len <- getLength
  bs <- B.copy <$> getByteString len
  pure $ decodeUtf8 bs
{-# INLINE getText #-}

putReference :: Reference -> Builder
putReference r = case r of
  Builtin name -> BU.word8 0 <> putText name
  Derived hash i -> BU.word8 1 <> putHash hash <> putLength i
{-# INLINE putReference #-}

putReferenceByNumber :: CanonMap Reference Int -> Reference -> Builder
putReferenceByNumber cm r
  | Just i <- unsafeLookup r cm = putVarInt i
  | otherwise = exn [] $ "could not serialize reference: " ++ show r
{-# INLINE putReferenceByNumber #-}

putRefNum :: RefNum -> Builder
putRefNum (RefNum i) = putVarInt i
{-# INLINE putRefNum #-}

getReference :: (PrimBase m) => Get m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Builtin <$> getText
    1 -> DerivedId <$> (Id <$> getHash <*> getLength)
    _ -> unknownTag "Reference" tag
{-# INLINE getReference #-}

getReferenceByNumber :: (PrimBase m) => Array Reference -> Get m Reference
getReferenceByNumber refm = getVarInt >>= lookupRef refm
{-# INLINE getReferenceByNumber #-}

lookupRef :: (Monad m) => Array Reference -> Int -> m Reference
lookupRef arr i
  | 0 <= i && i < sizeofArray arr = pure $ indexArray arr i
  | otherwise = exn [] $ "lookupRef: index out of bounds: " ++ show i
{-# INLINE lookupRef #-}

getRefNum :: (PrimBase m) => Get m RefNum
getRefNum = RefNum <$> getVarInt
{-# INLINE getRefNum #-}

putConstructorReference :: ConstructorReference -> Builder
putConstructorReference (ConstructorReference r i) =
  putReference r <> putLength i

getConstructorReference :: (PrimBase m) => Get m ConstructorReference
getConstructorReference =
  ConstructorReference <$> getReference <*> getLength

instance Tag Prim1 where
  tag2word DECI = 0
  tag2word DECN = 1
  tag2word INCI = 2
  tag2word INCN = 3
  tag2word NEGI = 4
  tag2word SGNI = 5
  tag2word LZRO = 6
  tag2word TZRO = 7
  tag2word COMN = 8
  tag2word COMI = 9
  tag2word POPC = 10
  tag2word ABSF = 11
  tag2word EXPF = 12
  tag2word LOGF = 13
  tag2word SQRT = 14
  tag2word COSF = 15
  tag2word ACOS = 16
  tag2word COSH = 17
  tag2word ACSH = 18
  tag2word SINF = 19
  tag2word ASIN = 20
  tag2word SINH = 21
  tag2word ASNH = 22
  tag2word TANF = 23
  tag2word ATAN = 24
  tag2word TANH = 25
  tag2word ATNH = 26
  tag2word ITOF = 27
  tag2word NTOF = 28
  tag2word CEIL = 29
  tag2word FLOR = 30
  tag2word TRNF = 31
  tag2word RNDF = 32
  tag2word TRNC = 33
  tag2word NOTB = 34
  --
  tag2word SIZT = 35
  tag2word USNC = 36
  tag2word UCNS = 37
  tag2word ITOT = 38
  tag2word NTOT = 39
  tag2word FTOT = 40
  tag2word TTOI = 41
  tag2word TTON = 42
  tag2word TTOF = 43
  tag2word PAKT = 44
  tag2word UPKT = 45
  tag2word VWLS = 46
  tag2word VWRS = 47
  tag2word SIZS = 48
  tag2word PAKB = 49
  tag2word UPKB = 50
  tag2word SIZB = 51
  tag2word FLTB = 52
  tag2word MISS = 53
  tag2word CACH = 54
  tag2word LKUP = 55
  tag2word LOAD = 56
  tag2word CVLD = 57
  tag2word VALU = 58
  tag2word TLTT = 59
  tag2word DBTX = 60
  tag2word SDBL = 61
  tag2word REFN = 62
  tag2word REFR = 63
  tag2word RRFC = 64
  tag2word TIKR = 65
  tag2word MDCM = 66
  tag2word MTYC = 67
  tag2word MEVL = 68

  word2tag 0 = pure DECI
  word2tag 1 = pure DECN
  word2tag 2 = pure INCI
  word2tag 3 = pure INCN
  word2tag 4 = pure NEGI
  word2tag 5 = pure SGNI
  word2tag 6 = pure LZRO
  word2tag 7 = pure TZRO
  word2tag 8 = pure COMN
  word2tag 9 = pure COMI
  word2tag 10 = pure POPC
  word2tag 11 = pure ABSF
  word2tag 12 = pure EXPF
  word2tag 13 = pure LOGF
  word2tag 14 = pure SQRT
  word2tag 15 = pure COSF
  word2tag 16 = pure ACOS
  word2tag 17 = pure COSH
  word2tag 18 = pure ACSH
  word2tag 19 = pure SINF
  word2tag 20 = pure ASIN
  word2tag 21 = pure SINH
  word2tag 22 = pure ASNH
  word2tag 23 = pure TANF
  word2tag 24 = pure ATAN
  word2tag 25 = pure TANH
  word2tag 26 = pure ATNH
  word2tag 27 = pure ITOF
  word2tag 28 = pure NTOF
  word2tag 29 = pure CEIL
  word2tag 30 = pure FLOR
  word2tag 31 = pure TRNF
  word2tag 32 = pure RNDF
  word2tag 33 = pure TRNC
  word2tag 34 = pure NOTB
  --
  word2tag 35 = pure SIZT
  word2tag 36 = pure USNC
  word2tag 37 = pure UCNS
  word2tag 38 = pure ITOT
  word2tag 39 = pure NTOT
  word2tag 40 = pure FTOT
  word2tag 41 = pure TTOI
  word2tag 42 = pure TTON
  word2tag 43 = pure TTOF
  word2tag 44 = pure PAKT
  word2tag 45 = pure UPKT
  word2tag 46 = pure VWLS
  word2tag 47 = pure VWRS
  word2tag 48 = pure SIZS
  word2tag 49 = pure PAKB
  word2tag 50 = pure UPKB
  word2tag 51 = pure SIZB
  word2tag 52 = pure FLTB
  word2tag 53 = pure MISS
  word2tag 54 = pure CACH
  word2tag 55 = pure LKUP
  word2tag 56 = pure LOAD
  word2tag 57 = pure CVLD
  word2tag 58 = pure VALU
  word2tag 59 = pure TLTT
  word2tag 60 = pure DBTX
  word2tag 61 = pure SDBL
  word2tag 62 = pure REFN
  word2tag 63 = pure REFR
  word2tag 64 = pure RRFC
  word2tag 65 = pure TIKR
  word2tag 66 = pure MDCM
  word2tag 67 = pure MTYC
  word2tag 68 = pure MEVL
  word2tag n = unknownTag "Prim1" n

instance Tag Prim2 where
  tag2word ADDI = 0
  tag2word ADDN = 1
  tag2word SUBI = 2
  tag2word SUBN = 3
  tag2word MULI = 4
  tag2word MULN = 5
  tag2word DIVI = 6
  tag2word MODI = 7
  tag2word DIVN = 8
  tag2word MODN = 9
  tag2word SHLI = 10
  tag2word SHLN = 11
  tag2word SHRI = 12
  tag2word SHRN = 13
  tag2word POWI = 14
  tag2word POWN = 15
  tag2word EQLI = 16
  tag2word NEQI = 17
  tag2word EQLN = 18
  tag2word NEQN = 19
  tag2word LEQI = 20
  tag2word LEQN = 21
  tag2word LESI = 22
  tag2word LESN = 23
  tag2word ANDN = 24
  tag2word ANDI = 25
  tag2word IORN = 26
  tag2word IORI = 27
  tag2word XORN = 28
  tag2word XORI = 29
  tag2word EQLF = 30
  tag2word NEQF = 31
  tag2word LEQF = 32
  tag2word LESF = 33
  tag2word ADDF = 34
  tag2word SUBF = 35
  tag2word MULF = 36
  tag2word DIVF = 37
  tag2word ATN2 = 38
  tag2word POWF = 39
  tag2word LOGB = 40
  tag2word MAXF = 41
  tag2word MINF = 42
  tag2word CAST = 43
  tag2word DRPN = 44
  tag2word ANDB = 45
  tag2word IORB = 46
  tag2word EQLU = 47
  tag2word LEQU = 48
  tag2word LESU = 49
  tag2word CMPU = 50
  tag2word DRPT = 51
  tag2word CATT = 52
  tag2word TAKT = 53
  tag2word EQLT = 54
  tag2word LEQT = 55
  tag2word LEST = 56
  tag2word DRPS = 57
  tag2word CATS = 58
  tag2word TAKS = 59
  tag2word CONS = 60
  tag2word SNOC = 61
  tag2word IDXS = 62
  tag2word SPLL = 63
  tag2word SPLR = 64
  tag2word TAKB = 65
  tag2word DRPB = 66
  tag2word IDXB = 67
  tag2word CATB = 68
  tag2word THRO = 69
  tag2word TRCE = 70
  tag2word SDBX = 71
  tag2word IXOT = 72
  tag2word IXOB = 73
  tag2word SDBV = 74
  tag2word REFW = 75

  word2tag 0 = pure ADDI
  word2tag 1 = pure ADDN
  word2tag 2 = pure SUBI
  word2tag 3 = pure SUBN
  word2tag 4 = pure MULI
  word2tag 5 = pure MULN
  word2tag 6 = pure DIVI
  word2tag 7 = pure MODI
  word2tag 8 = pure DIVN
  word2tag 9 = pure MODN
  word2tag 10 = pure SHLI
  word2tag 11 = pure SHLN
  word2tag 12 = pure SHRI
  word2tag 13 = pure SHRN
  word2tag 14 = pure POWI
  word2tag 15 = pure POWN
  word2tag 16 = pure EQLI
  word2tag 17 = pure NEQI
  word2tag 18 = pure EQLN
  word2tag 19 = pure NEQN
  word2tag 20 = pure LEQI
  word2tag 21 = pure LEQN
  word2tag 22 = pure LESI
  word2tag 23 = pure LESN
  word2tag 24 = pure ANDN
  word2tag 25 = pure ANDI
  word2tag 26 = pure IORN
  word2tag 27 = pure IORI
  word2tag 28 = pure XORN
  word2tag 29 = pure XORI
  word2tag 30 = pure EQLF
  word2tag 31 = pure NEQF
  word2tag 32 = pure LEQF
  word2tag 33 = pure LESF
  word2tag 34 = pure ADDF
  word2tag 35 = pure SUBF
  word2tag 36 = pure MULF
  word2tag 37 = pure DIVF
  word2tag 38 = pure ATN2
  word2tag 39 = pure POWF
  word2tag 40 = pure LOGB
  word2tag 41 = pure MAXF
  word2tag 42 = pure MINF
  word2tag 43 = pure CAST
  word2tag 44 = pure DRPN
  word2tag 45 = pure ANDB
  word2tag 46 = pure IORB
  --
  word2tag 47 = pure EQLU
  word2tag 48 = pure LEQU
  word2tag 49 = pure LESU
  word2tag 50 = pure CMPU
  word2tag 51 = pure DRPT
  word2tag 52 = pure CATT
  word2tag 53 = pure TAKT
  word2tag 54 = pure EQLT
  word2tag 55 = pure LEQT
  word2tag 56 = pure LEST
  word2tag 57 = pure DRPS
  word2tag 58 = pure CATS
  word2tag 59 = pure TAKS
  word2tag 60 = pure CONS
  word2tag 61 = pure SNOC
  word2tag 62 = pure IDXS
  word2tag 63 = pure SPLL
  word2tag 64 = pure SPLR
  word2tag 65 = pure TAKB
  word2tag 66 = pure DRPB
  word2tag 67 = pure IDXB
  word2tag 68 = pure CATB
  word2tag 69 = pure THRO
  word2tag 70 = pure TRCE
  word2tag 71 = pure SDBX
  word2tag 72 = pure IXOT
  word2tag 73 = pure IXOB
  word2tag 74 = pure SDBV
  word2tag 75 = pure REFW
  word2tag n = unknownTag "Prim2" n
