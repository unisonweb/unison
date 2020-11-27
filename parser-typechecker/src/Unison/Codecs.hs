{-# LANGUAGE PatternSynonyms #-}

module Unison.Codecs where

-- A format for encoding runtime values, with sharing for compiled nodes.

import Control.Arrow (second)
import Control.Monad.State
import Data.Bits (Bits)
import qualified Data.ByteString as B
import Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Put
import qualified Data.Bytes.Serial as BS
import Data.Bytes.Signed (Unsigned)
import Data.Bytes.VarInt (VarInt (..))
import qualified Unison.ABT as ABT
import qualified Unison.Blank as Blank
import qualified Unison.ConstructorType as ConstructorType
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hash as Hash
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.Reference (Reference, pattern Builtin, pattern Derived)
import qualified Unison.Referent as Referent
import Unison.Term
import Unison.UnisonFile (UnisonFile, pattern UnisonFile)
import qualified Unison.UnisonFile as UF
import Unison.Var (Var)
import qualified Unison.Var as Var

type Pos = Word64

serializeTerm ::
  (MonadPut m, MonadState Pos m, Var v) =>
  Term v a ->
  m Pos
serializeTerm x = do
  let putTag = do putWord8 111; putWord8 0
  let incPosition = do pos <- get; modify' (+ 1); pure pos
  case ABT.out x of
    ABT.Var v -> do
      putTag
      putWord8 0
      lengthEncode $ Var.name v
      incPosition
    ABT.Abs v body -> do
      pbody <- serializeTerm body
      putTag
      putWord8 1
      lengthEncode $ Var.name v
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
      Int n -> do
        putTag
        putWord8 6
        serializeInt n
        incPosition
      Nat n -> do
        putTag
        putWord8 6
        serializeNat n
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
      Sequence v -> do
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
      Let _ binding body -> do
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
      Blank b ->
        error $
          "cannot serialize program with blank "
            ++ fromMaybe "" (Blank.nameb b)
      Handle h body -> do
        hpos <- serializeTerm h
        bpos <- serializeTerm body
        putTag
        putWord8 16
        putBackref hpos
        putBackref bpos
        incPosition
      LetRec _ bs body -> do
        positions <- traverse serializeTerm bs
        pbody <- serializeTerm body
        putTag
        putWord8 19
        putLength $ length positions
        traverse_ putBackref positions
        putBackref pbody
        incPosition
      Char c -> do
        putTag
        putWord8 20
        putWord64be $ fromIntegral $ fromEnum c
        incPosition
      TermLink ref -> do
        putTag
        putWord8 21
        serializeReferent ref
        incPosition
      TypeLink ref -> do
        putTag
        putWord8 22
        serializeReference ref
        incPosition

serializePattern :: MonadPut m => Pattern a -> m ()
serializePattern p = case p of
  -- note: the putWord8 0 is the tag before any unboxed pattern
  Pattern.Boolean _ b -> putWord8 0 *> serializeBoolean b
  Pattern.Int _ n -> putWord8 0 *> serializeInt n
  Pattern.Nat _ n -> putWord8 0 *> serializeNat n
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

serializeNat :: MonadPut m => Word64 -> m ()
serializeNat n = do
  putWord64be n
  putWord8 2

serializeInt :: MonadPut m => Int64 -> m ()
serializeInt n = do
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

serializeCase1 ::
  (Var v, MonadPut m, MonadState Pos m) =>
  MatchCase p (Term v a) ->
  m (MatchCase p Pos)
serializeCase1 (MatchCase p guard body) = do
  posg <- traverse serializeTerm guard
  posb <- serializeTerm body
  pure $ MatchCase p posg posb

putBackref :: MonadPut m => Pos -> m ()
putBackref = BS.serialize . VarInt

putLength ::
  ( MonadPut m,
    Integral n,
    Integral (Unsigned n),
    Bits n,
    Bits (Unsigned n)
  ) =>
  n ->
  m ()
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

serializeReferent :: MonadPut m => Referent.Referent -> m ()
serializeReferent r = case r of
  Referent.Ref r -> putWord8 0 *> serializeReference r
  Referent.Con r cid ct -> do
    putWord8 1
    serializeReference r
    putLength cid
    serializeConstructorType ct

serializeConstructorType :: MonadPut m => ConstructorType.ConstructorType -> m ()
serializeConstructorType ct = case ct of
  ConstructorType.Data -> putWord8 0
  ConstructorType.Effect -> putWord8 1

serializeReference :: MonadPut m => Reference -> m ()
serializeReference ref = case ref of
  Builtin text -> do
    putWord8 0
    lengthEncode text
  Derived hash i n -> do
    putWord8 1
    let bs = Hash.toBytes hash
    putLength $ B.length bs
    putByteString bs
    putLength i
    putLength n
  _ -> error "impossible"

serializeConstructorArities :: MonadPut m => Reference -> [Int] -> m ()
serializeConstructorArities r constructorArities = do
  serializeReference r
  serializeFoldable (putWord32be . fromIntegral) constructorArities

serializeFile ::
  (MonadPut m, MonadState Pos m, Monoid a, Var v) =>
  UnisonFile v a ->
  Term v a ->
  m ()
serializeFile uf@(UnisonFile dataDecls effectDecls _ _) tm = do
  let body = UF.uberTerm' uf tm
  let dataDecls' = second DD.constructorArities <$> toList dataDecls
  let effectDecls' =
        second (DD.constructorArities . DD.toDataDecl) <$> toList effectDecls
  -- traceM $ show effectDecls'
  serializeFoldable (uncurry serializeConstructorArities) dataDecls'
  serializeFoldable (uncurry serializeConstructorArities) effectDecls'
  -- NB: we rewrite the term to minimize away let rec cycles, as let rec
  -- blocks aren't allowed to have effects
  pos <- serializeTerm body
  putWord8 0
  putBackref pos
