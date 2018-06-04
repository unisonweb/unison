{-# LANGUAGE FlexibleContexts #-}

module Unison.Codecs where

import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Bytes.Put
import           Data.Foldable (traverse_)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word64)
import qualified Unison.ABT as ABT
import qualified Unison.Hash as Hash
import           Unison.Reference
import           Unison.Term
import           Unison.Var
import qualified Unison.Var as Var
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern

type Pos = Word64

serializeTerm :: (MonadPut m, MonadState Pos m, Var v) => Term v -> m Pos
serializeTerm x = do
  case ABT.out x of
    ABT.Var v -> do
      putWord8 0
      lengthEncode $ Var.qualifiedName v
    ABT.Abs v body -> do
      pbody <- serializeTerm body
      putWord8 1
      lengthEncode $ Var.qualifiedName v
      putBackref pbody
    ABT.Cycle body -> do
      pbody <- serializeTerm body
      putWord8 10
      putBackref pbody
    ABT.Tm f -> case f of
      Ref ref -> do
        putWord8 2
        serializeReference ref
      Constructor ref id -> do
        putWord8 3
        serializeReference ref
        putWord32be $ fromIntegral id
      Request ref id -> do
        putWord8 4
        serializeReference ref
        putWord32be $ fromIntegral id
      Text text -> do
        putWord8 5
        lengthEncode text
      Int64 n -> do
        putWord8 6
        serializeInt64 n
      UInt64 n -> do
        putWord8 6
        serializeUInt64 n
      Float n -> do
        putWord8 6
        serializeFloat n
      Boolean b -> do
        putWord8 6
        serializeBoolean b
      Vector v -> do
        elementPositions <- traverse serializeTerm v
        putWord8 7
        putLength $ length elementPositions
        traverse_ putBackref elementPositions
      Lam body -> do
        pos <- serializeTerm body
        putWord8 8
        putBackref pos
      App fn arg -> do
        posf <- serializeTerm fn
        posarg <- serializeTerm arg
        putWord8 9
        putBackref posf
        putLength 1
        putBackref posarg
      Let binding body -> do
        posbind <- serializeTerm binding
        posbod <- serializeTerm body
        putWord8 11
        putBackref posbind
        putBackref posbod
      If c t f -> do
        posc <- serializeTerm c
        post <- serializeTerm t
        posf <- serializeTerm f
        putWord8 12
        putBackref posc
        putBackref post
        putBackref posf
      And x y -> do
        posx <- serializeTerm x
        posy <- serializeTerm y
        putWord8 13
        putBackref posx
        putBackref posy
      Or x y -> do
        posx <- serializeTerm x
        posy <- serializeTerm y
        putWord8 14
        putBackref posx
        putBackref posy
      Match scrutinee cases -> do
        poss <- serializeTerm scrutinee
        casePositions <- traverse serializeCase1 cases
        putWord8 15
        putBackref poss
        putLength $ length casePositions
        traverse_ serializeCase2 casePositions
  pos <- get
  modify' (+1)
  pure pos

serializePattern :: MonadPut m => Pattern -> m ()
serializePattern p = case p of
  -- note: the putWord8 0 is the tag before any unboxed pattern
  Pattern.Boolean b -> putWord8 0 *> serializeBoolean b
  Pattern.Int64 n -> putWord8 0 *> serializeInt64 n
  Pattern.UInt64 n -> putWord8 0 *> serializeUInt64 n
  Pattern.Float n -> putWord8 0 *> serializeFloat n
  Pattern.Var -> putWord8 1
  Pattern.Unbound -> putWord8 2
  Pattern.Constructor r cid ps -> do
    putWord8 3
    serializeReference r
    putWord32be $ fromIntegral cid
    putLength (length ps)
    traverse_ serializePattern ps
  Pattern.As p -> do
    putWord8 4
    serializePattern p
  Pattern.EffectPure p -> do
    putWord8 5
    serializePattern p
  Pattern.EffectBind r cid ps k -> do
    putWord8 6
    serializeReference r
    putWord32be $ fromIntegral cid
    putLength (length ps)
    traverse_ serializePattern ps
    serializePattern k

serializeFloat n = do
  putByteString . BL.toStrict . toLazyByteString $ doubleBE n
  putWord8 3

serializeUInt64 n = do
  putWord64be n
  putWord8 2

serializeInt64 n = do
  putByteString . BL.toStrict . toLazyByteString $ int64BE n
  putWord8 1

serializeBoolean :: MonadPut m => Bool -> m ()
serializeBoolean False = putWord64be 0 *> putWord8 0
serializeBoolean True = putWord64be 1 *> putWord8 0

serializeCase2 :: MonadPut m => MatchCase Pos -> m ()
serializeCase2 (MatchCase p guard body) = do
  serializePattern p
  serializeMaybe putBackref guard
  putBackref body

serializeCase1 :: (Var v, MonadPut m, MonadState Pos m)
               => MatchCase (Term v) -> m (MatchCase Pos)
serializeCase1 (MatchCase p guard body) = do
  posg <- traverse serializeTerm guard
  posb <- serializeTerm body
  pure $ MatchCase p posg posb

putBackref :: MonadPut m => Pos -> m ()
putBackref = putWord64be

putLength :: (MonadPut m, Integral n) => n -> m ()
putLength = putWord64be . fromIntegral

serializeMaybe :: (MonadPut m) => (a -> m ()) -> Maybe a -> m ()
serializeMaybe f b = case b of
  Nothing -> putWord8 0
  Just x -> putWord8 1 *> f x

lengthEncode text = do
  let bs = encodeUtf8 text
  putWord32be . fromIntegral $ B.length bs
  putByteString bs

serializeReference ref = case ref of
  Builtin text -> do
    putWord8 0
    lengthEncode text
  Derived hash -> do
    putWord8 1
    let bs = Hash.toBytes hash
    putWord32be . fromIntegral $ B.length bs
    putByteString bs

--
--
-- serializeDataDeclaration :: MonadPut m => DataDeclaration v -> m ()
--
--
-- serializeEffectDeclaration :: MonadPut m => EffectDeclaration v -> m ()
--
--
-- serializeFile :: MonadPut m => UnisonFile
