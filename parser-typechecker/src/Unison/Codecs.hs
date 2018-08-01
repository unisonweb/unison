{-# LANGUAGE FlexibleContexts #-}

module Unison.Codecs where

import Data.Text (Text)
import           Control.Arrow (second)
import           Control.Monad.State
import           Data.Bits (Bits)
import qualified Data.Bytes.Serial as BS
import           Data.Bytes.Signed (Unsigned)
import           Data.Bytes.VarInt (VarInt(..))
import qualified Data.ByteString as B
import           Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Bytes.Put
import           Data.Foldable (toList, traverse_)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word64)
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hash as Hash
import           Unison.Reference
import           Unison.Term
import           Unison.UnisonFile (UnisonFile(..))
import           Unison.Var
import qualified Unison.Var as Var
import Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import Data.Int (Int64)

type Pos = Word64

serializeTerm :: (MonadPut m, MonadState Pos m, Var v)
              => AnnotatedTerm v a
              -> m Pos
serializeTerm x = do
  let putTag = do putWord8 111; putWord8 0
  let incPosition = do pos <- get; modify' (+1); pure pos
  case ABT.out x of
    ABT.Var v -> do
      putTag
      putWord8 0
      lengthEncode $ Var.qualifiedName v
      incPosition
    ABT.Abs v body -> do
      pbody <- serializeTerm body
      putTag
      putWord8 1
      lengthEncode $ Var.qualifiedName v
      putBackref pbody
      incPosition
    ABT.Cycle body -> do
      pbody <- serializeTerm body
      putTag
      putWord8 10
      putBackref pbody
      incPosition
    ABT.Tm f -> case f of
      Ann e _ -> do
        serializeTerm e -- ignore types (todo: revisit)
      Ref ref -> do
        putTag
        putWord8 2
        serializeReference ref
        incPosition
      Constructor ref id -> do
        putTag
        putWord8 3
        serializeReference ref
        putWord32be $ fromIntegral id
        incPosition
      Request ref id -> do
        putTag
        putWord8 4
        serializeReference ref
        putWord32be $ fromIntegral id
        incPosition
      Text text -> do
        putTag
        putWord8 5
        lengthEncode text
        incPosition
      Int64 n -> do
        putTag
        putWord8 6
        serializeInt64 n
        incPosition
      UInt64 n -> do
        putTag
        putWord8 6
        serializeUInt64 n
        incPosition
      Float n -> do
        putTag
        putWord8 6
        serializeFloat n
        incPosition
      Boolean b -> do
        putTag
        putWord8 6
        serializeBoolean b
        incPosition
      Vector v -> do
        elementPositions <- traverse serializeTerm v
        putTag
        putWord8 7
        putLength $ length elementPositions
        traverse_ putBackref elementPositions
        incPosition
      Lam body -> do
        pos <- serializeTerm body
        putTag
        putWord8 8
        putBackref pos
        incPosition
      App fn arg -> do
        posf <- serializeTerm fn
        posarg <- serializeTerm arg
        putTag
        putWord8 9
        putBackref posf
        putLength (1 :: Int)
        putBackref posarg
        incPosition
      Let binding body -> do
        posbind <- serializeTerm binding
        posbod <- serializeTerm body
        putTag
        putWord8 11
        putBackref posbind
        putBackref posbod
        incPosition
      If c t f -> do
        posc <- serializeTerm c
        post <- serializeTerm t
        posf <- serializeTerm f
        putTag
        putWord8 12
        putBackref posc
        putBackref post
        putBackref posf
        incPosition
      And x y -> do
        posx <- serializeTerm x
        posy <- serializeTerm y
        putTag
        putWord8 13
        putBackref posx
        putBackref posy
        incPosition
      Or x y -> do
        posx <- serializeTerm x
        posy <- serializeTerm y
        putTag
        putWord8 14
        putBackref posx
        putBackref posy
        incPosition
      Match scrutinee cases -> do
        poss <- serializeTerm scrutinee
        casePositions <- traverse serializeCase1 cases
        putTag
        putWord8 15
        putBackref poss
        putLength $ length casePositions
        traverse_ serializeCase2 casePositions
        incPosition
      Blank _ -> error "cannot serialize program with blanks"
      Handle h body -> do
        hpos <- serializeTerm h
        bpos <- serializeTerm body
        putTag
        putWord8 16
        putBackref hpos
        putBackref bpos
        incPosition
      EffectPure t -> do
        pos <- serializeTerm t
        putTag
        putWord8 17
        putBackref pos
        incPosition
      EffectBind r cid args k -> do
        positions <- traverse serializeTerm args
        kpos <- serializeTerm k
        putTag
        putWord8 18
        serializeReference r
        putWord32be $ fromIntegral cid
        putLength $ length positions
        traverse_ putBackref positions
        putBackref kpos
        incPosition
      LetRec bs body -> do
        positions <- traverse serializeTerm bs
        pbody <- serializeTerm body
        putTag
        putWord8 19
        putLength $ length positions
        traverse_ putBackref positions
        putBackref pbody
        incPosition

serializePattern :: MonadPut m => Pattern a -> m ()
serializePattern p = case p of
  -- note: the putWord8 0 is the tag before any unboxed pattern
  Pattern.Boolean _ b -> putWord8 0 *> serializeBoolean b
  Pattern.Int64 _ n -> putWord8 0 *> serializeInt64 n
  Pattern.UInt64 _ n -> putWord8 0 *> serializeUInt64 n
  Pattern.Float _ n -> putWord8 0 *> serializeFloat n
  Pattern.Var _ -> putWord8 1
  Pattern.Unbound _ -> putWord8 2
  Pattern.Constructor _ r cid ps -> do
    putWord8 3
    serializeReference r
    putWord32be $ fromIntegral cid
    putLength (length ps)
    traverse_ serializePattern ps
  Pattern.As _ p -> do
    putWord8 4
    serializePattern p
  Pattern.EffectPure _ p -> do
    putWord8 5
    serializePattern p
  Pattern.EffectBind _ r cid ps k -> do
    putWord8 6
    serializeReference r
    putWord32be $ fromIntegral cid
    putLength (length ps)
    traverse_ serializePattern ps
    serializePattern k
  _ -> error "todo: delete me after deleting PatternP - serializePattern match failure"

serializeFloat :: MonadPut m => Double -> m ()
serializeFloat n = do
  putByteString . BL.toStrict . toLazyByteString $ doubleBE n
  putWord8 3

serializeUInt64 :: MonadPut m => Word64 -> m ()
serializeUInt64 n = do
  putWord64be n
  putWord8 2

serializeInt64 :: MonadPut m => Int64 -> m ()
serializeInt64 n = do
  putByteString . BL.toStrict . toLazyByteString $ int64BE n
  putWord8 1

serializeBoolean :: MonadPut m => Bool -> m ()
serializeBoolean False = putWord64be 0 *> putWord8 0
serializeBoolean True = putWord64be 1 *> putWord8 0

serializeCase2 :: MonadPut m => MatchCase loc Pos -> m ()
serializeCase2 (MatchCase p guard body) = do
  serializePattern p
  serializeMaybe putBackref guard
  putBackref body

serializeCase1 :: (Var v, MonadPut m, MonadState Pos m)
               => MatchCase p (AnnotatedTerm v a) -> m (MatchCase p Pos)
serializeCase1 (MatchCase p guard body) = do
  posg <- traverse serializeTerm guard
  posb <- serializeTerm body
  pure $ MatchCase p posg posb

putBackref :: MonadPut m => Pos -> m ()
putBackref = BS.serialize . VarInt

putLength :: (MonadPut m, Integral n, Integral (Unsigned n),
                          Bits n,     Bits (Unsigned n))
          => n -> m ()
putLength = BS.serialize . VarInt

serializeMaybe :: (MonadPut m) => (a -> m ()) -> Maybe a -> m ()
serializeMaybe f b = case b of
  Nothing -> putWord8 0
  Just x -> putWord8 1 *> f x

lengthEncode :: MonadPut m => Text -> m ()
lengthEncode text = do
  let bs = encodeUtf8 text
  putLength $ B.length bs
  putByteString bs

serializeFoldable :: (MonadPut m, Foldable f) => (a -> m ()) -> f a -> m ()
serializeFoldable f fa = do
  putLength $ length fa
  traverse_ f fa

serializeReference :: MonadPut m => Reference -> m ()
serializeReference ref = case ref of
  Builtin text -> do
    putWord8 0
    lengthEncode text
  Derived hash -> do
    putWord8 1
    let bs = Hash.toBytes hash
    putLength $ B.length bs
    putByteString bs

serializeConstructorArities :: MonadPut m => Reference -> [Int] -> m ()
serializeConstructorArities r constructorArities = do
  serializeReference r
  serializeFoldable (putWord32be . fromIntegral) constructorArities

serializeFile :: (MonadPut m, MonadState Pos m, Var v) => UnisonFile v a -> m ()
serializeFile (UnisonFile dataDecls effectDecls body) = do
  let dataDecls' = second DD.constructorArities <$> toList dataDecls
  let effectDecls' = second (DD.constructorArities . DD.toDataDecl) <$> toList effectDecls
  serializeFoldable (uncurry serializeConstructorArities) dataDecls'
  serializeFoldable (uncurry serializeConstructorArities) effectDecls'
  pos <- serializeTerm body
  putWord8 0
  putBackref pos
