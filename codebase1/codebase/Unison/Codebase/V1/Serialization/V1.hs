{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.V1.Serialization.V1 where

import Control.Applicative (Applicative (liftA2), liftA3)
import Control.Monad (replicateM)
import Data.Bits (Bits)
import qualified Data.ByteString as B
import Data.Bytes.Get
import Data.Bytes.Serial (deserialize, deserializeBE)
import Data.Bytes.Signed (Unsigned)
import Data.Bytes.VarInt (VarInt (..))
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word64)
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import U.Util.Relation (Relation)
import qualified U.Util.Relation as Relation
import qualified Unison.Codebase.V1.ABT as ABT
import Unison.Codebase.V1.Branch.NameSegment as NameSegment
import qualified Unison.Codebase.V1.Branch.Raw as Branch
import Unison.Codebase.V1.Causal.Raw (Raw (..), RawHash (..))
import qualified Unison.Codebase.V1.Causal.Raw as Causal
import qualified Unison.Codebase.V1.ConstructorType as CT
import qualified Unison.Codebase.V1.DataDeclaration as DataDeclaration
import Unison.Codebase.V1.DataDeclaration (DataDeclaration, EffectDeclaration)
import Unison.Codebase.V1.Patch (Patch (..))
import Unison.Codebase.V1.Patch.TermEdit (TermEdit)
import qualified Unison.Codebase.V1.Patch.TermEdit as TermEdit
import Unison.Codebase.V1.Patch.TypeEdit (TypeEdit)
import qualified Unison.Codebase.V1.Patch.TypeEdit as TypeEdit
import Unison.Codebase.V1.Reference (Reference)
import qualified Unison.Codebase.V1.Reference as Reference
import Unison.Codebase.V1.Referent (Referent)
import qualified Unison.Codebase.V1.Referent as Referent
import Unison.Codebase.V1.Star3 (Star3)
import qualified Unison.Codebase.V1.Star3 as Star3
import Unison.Codebase.V1.Symbol (Symbol (..))
import Unison.Codebase.V1.Term (Term)
import qualified Unison.Codebase.V1.Term as Term
import qualified Unison.Codebase.V1.Term.Pattern as Pattern
import Unison.Codebase.V1.Term.Pattern (Pattern, SeqOp)
import qualified Unison.Codebase.V1.Type as Type
import Unison.Codebase.V1.Type (Type)
import Unison.Codebase.V1.Type.Kind (Kind)
import qualified Unison.Codebase.V1.Type.Kind as Kind
import Prelude hiding (getChar, putChar)

-- ABOUT THIS FORMAT:
--
-- A serialization format for uncompiled Unison syntax trees.
--
-- Finalized: No
--
-- If Finalized: Yes, don't modify this file in a way that affects serialized form.
-- Instead, create a new file, V(n + 1).
-- This ensures that we have a well-defined serialized form and can read
-- and write old versions.

unknownTag :: (MonadGet m, Show a) => String -> a -> m x
unknownTag msg tag =
  fail $
    "unknown tag " ++ show tag
      ++ " while deserializing: "
      ++ msg

getCausal0 :: MonadGet m => m a -> m (Causal.Raw h a)
getCausal0 getA = getWord8 >>= \case
  0 -> RawOne <$> getA
  1 -> flip RawCons <$> (RawHash <$> getHash) <*> getA
  2 -> flip RawMerge . Set.fromList <$> getList (RawHash <$> getHash) <*> getA
  x -> unknownTag "Causal0" x

getLength ::
  ( MonadGet m,
    Integral n,
    Integral (Unsigned n),
    Bits n,
    Bits (Unsigned n)
  ) =>
  m n
getLength = unVarInt <$> deserialize

getText :: MonadGet m => m Text
getText = do
  len <- getLength
  bs <- B.copy <$> getBytes len
  pure $ decodeUtf8 bs

getFloat :: MonadGet m => m Double
getFloat = deserializeBE

getNat :: MonadGet m => m Word64
getNat = getWord64be

getInt :: MonadGet m => m Int64
getInt = deserializeBE

getBoolean :: MonadGet m => m Bool
getBoolean = go =<< getWord8
  where
    go 0 = pure False
    go 1 = pure True
    go t = unknownTag "Boolean" t

getHash :: MonadGet m => m Hash
getHash = do
  len <- getLength
  bs <- B.copy <$> getBytes len
  pure $ Hash.fromBytes bs

getReference :: MonadGet m => m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Reference.Builtin <$> getText
    1 -> Reference.DerivedId <$> (Reference.Id <$> getHash <*> getLength <*> getLength)
    _ -> unknownTag "Reference" tag

getReferent :: MonadGet m => m Referent
getReferent = do
  tag <- getWord8
  case tag of
    0 -> Referent.Ref <$> getReference
    1 -> Referent.Con <$> getReference <*> getLength <*> getConstructorType
    _ -> unknownTag "getReferent" tag

getConstructorType :: MonadGet m => m CT.ConstructorType
getConstructorType = getWord8 >>= \case
  0 -> pure CT.Data
  1 -> pure CT.Effect
  t -> unknownTag "getConstructorType" t

getMaybe :: MonadGet m => m a -> m (Maybe a)
getMaybe getA = getWord8 >>= \tag -> case tag of
  0 -> pure Nothing
  1 -> Just <$> getA
  _ -> unknownTag "Maybe" tag

-- getFolded :: MonadGet m => (b -> a -> b) -> b -> m a -> m b
-- getFolded f z a =
--   foldl' f z <$> getList a

getList :: MonadGet m => m a -> m [a]
getList a = getLength >>= (`replicateM` a)

getABT ::
  (MonadGet m, Foldable f, Functor f, Ord v) =>
  m v ->
  m a ->
  (forall x. m x -> m (f x)) ->
  m (ABT.Term f v a)
getABT getVar getA getF = getList getVar >>= go []
  where
    go env fvs = do
      a <- getA
      tag <- getWord8
      case tag of
        0 -> do
          tag <- getWord8
          case tag of
            0 -> ABT.var a . (env !!) <$> getLength
            1 -> ABT.var a . (fvs !!) <$> getLength
            _ -> unknownTag "getABT.Var" tag
        1 -> ABT.tm a <$> getF (go env fvs)
        2 -> do
          v <- getVar
          body <- go (v : env) fvs
          pure $ ABT.abs a v body
        3 -> ABT.cycle a <$> go env fvs
        _ -> unknownTag "getABT" tag

getKind :: MonadGet m => m Kind
getKind = getWord8 >>= \tag -> case tag of
  0 -> pure Kind.Star
  1 -> Kind.Arrow <$> getKind <*> getKind
  _ -> unknownTag "getKind" tag

getType :: MonadGet m => m (Type Symbol ())
getType = getType' getSymbol (pure ())

getType' ::
  (MonadGet m, Ord v) =>
  m v ->
  m a ->
  m (Type v a)
getType' getVar getA = getABT getVar getA go
  where
    go getChild = getWord8 >>= \tag -> case tag of
      0 -> Type.Ref <$> getReference
      1 -> Type.Arrow <$> getChild <*> getChild
      2 -> Type.Ann <$> getChild <*> getKind
      3 -> Type.App <$> getChild <*> getChild
      4 -> Type.Effect <$> getChild <*> getChild
      5 -> Type.Effects <$> getList getChild
      6 -> Type.Forall <$> getChild
      7 -> Type.IntroOuter <$> getChild
      _ -> unknownTag "getType" tag

getSymbol :: MonadGet m => m Symbol
getSymbol = Symbol <$> getLength <*> getText

getSeqOp :: MonadGet m => m SeqOp
getSeqOp = getWord8 >>= \case
  0 -> pure Pattern.Cons
  1 -> pure Pattern.Snoc
  2 -> pure Pattern.Concat
  tag -> unknownTag "SeqOp" tag

getPattern :: MonadGet m => m a -> m Pattern
getPattern getA = getWord8 >>= \tag -> case tag of
  0 -> Pattern.Unbound <$ getA
  1 -> Pattern.Var <$ getA
  2 -> Pattern.Boolean <$ getA <*> getBoolean
  3 -> Pattern.Int <$ getA <*> getInt
  4 -> Pattern.Nat <$ getA <*> getNat
  5 -> Pattern.Float <$ getA <*> getFloat
  6 ->
    Pattern.Constructor <$ getA <*> getReference <*> getLength
      <*> getList
        (getPattern getA)
  7 -> Pattern.As <$ getA <*> getPattern getA
  8 -> Pattern.EffectPure <$ getA <*> getPattern getA
  9 ->
    Pattern.EffectBind
      <$ getA
      <*> getReference
      <*> getLength
      <*> getList (getPattern getA)
      <*> getPattern getA
  10 -> Pattern.SequenceLiteral <$ getA <*> getList (getPattern getA)
  11 ->
    Pattern.SequenceOp
      <$ getA
      <*> getPattern getA
      <*> getSeqOp
      <*> getPattern getA
  12 -> Pattern.Text <$ getA <*> getText
  13 -> Pattern.Char <$ getA <*> getChar
  _ -> unknownTag "Pattern" tag

getTerm :: MonadGet m => m (Term Symbol ())
getTerm = getTerm' getSymbol (pure ())

getTerm' ::
  (MonadGet m, Ord v) =>
  m v ->
  m a ->
  m (Term v a)
getTerm' getVar getA = getABT getVar getA go
  where
    go getChild = getWord8 >>= \tag -> case tag of
      0 -> Term.Int <$> getInt
      1 -> Term.Nat <$> getNat
      2 -> Term.Float <$> getFloat
      3 -> Term.Boolean <$> getBoolean
      4 -> Term.Text <$> getText
      5 -> Term.Ref <$> getReference
      6 -> Term.Constructor <$> getReference <*> getLength
      7 -> Term.Request <$> getReference <*> getLength
      8 -> Term.Handle <$> getChild <*> getChild
      9 -> Term.App <$> getChild <*> getChild
      10 -> Term.Ann <$> getChild <*> getType' getVar getA
      11 -> Term.Sequence . Sequence.fromList <$> getList getChild
      12 -> Term.If <$> getChild <*> getChild <*> getChild
      13 -> Term.And <$> getChild <*> getChild
      14 -> Term.Or <$> getChild <*> getChild
      15 -> Term.Lam <$> getChild
      16 -> Term.LetRec False <$> getList getChild <*> getChild
      17 -> Term.Let False <$> getChild <*> getChild
      18 ->
        Term.Match <$> getChild
          <*> getList (Term.MatchCase <$> getPattern getA <*> getMaybe getChild <*> getChild)
      19 -> Term.Char <$> getChar
      20 -> Term.TermLink <$> getReferent
      21 -> Term.TypeLink <$> getReference
      _ -> unknownTag "getTerm" tag

getPair :: MonadGet m => m a -> m b -> m (a, b)
getPair = liftA2 (,)

getTuple3 :: MonadGet m => m a -> m b -> m c -> m (a, b, c)
getTuple3 = liftA3 (,,)

getRelation :: (MonadGet m, Ord a, Ord b) => m a -> m b -> m (Relation a b)
getRelation getA getB = Relation.fromList <$> getList (getPair getA getB)

getMap :: (MonadGet m, Ord a) => m a -> m b -> m (Map a b)
getMap getA getB = Map.fromList <$> getList (getPair getA getB)

getTermEdit :: MonadGet m => m TermEdit
getTermEdit = getWord8 >>= \case
  1 ->
    TermEdit.Replace <$> getReference
      <*> ( getWord8 >>= \case
              1 -> pure TermEdit.Same
              2 -> pure TermEdit.Subtype
              3 -> pure TermEdit.Different
              t -> unknownTag "TermEdit.Replace" t
          )
  2 -> pure TermEdit.Deprecate
  t -> unknownTag "TermEdit" t

getTypeEdit :: MonadGet m => m TypeEdit
getTypeEdit = getWord8 >>= \case
  1 -> TypeEdit.Replace <$> getReference
  2 -> pure TypeEdit.Deprecate
  t -> unknownTag "TypeEdit" t

getStar3 ::
  (MonadGet m, Ord fact, Ord d1, Ord d2, Ord d3) =>
  m fact ->
  m d1 ->
  m d2 ->
  m d3 ->
  m (Star3 fact d1 d2 d3)
getStar3 getF getD1 getD2 getD3 =
  Star3.Star3
    <$> (Set.fromList <$> getList getF)
    <*> getRelation getF getD1
    <*> getRelation getF getD2
    <*> getRelation getF getD3

getBranchStar :: (Ord a, Ord n, MonadGet m) => m a -> m n -> m (Branch.Star a n)
getBranchStar getA getN = getStar3 getA getN getMetadataType (getPair getMetadataType getMetadataValue)

getChar :: MonadGet m => m Char
getChar = toEnum . unVarInt <$> deserialize

getNameSegment :: MonadGet m => m NameSegment
getNameSegment = NameSegment <$> getText

getMetadataType :: MonadGet m => m Branch.MetadataType
getMetadataType = getReference

getMetadataValue :: MonadGet m => m Branch.MetadataValue
getMetadataValue = getReference

getRawBranch :: MonadGet m => m Branch.Raw
getRawBranch =
  Branch.Raw
    <$> getBranchStar getReferent getNameSegment
    <*> getBranchStar getReference getNameSegment
    <*> getMap getNameSegment (Branch.BranchHash <$> getHash)
    <*> getMap getNameSegment (Branch.EditHash <$> getHash)

getDataDeclaration :: MonadGet m => m (DataDeclaration Symbol ())
getDataDeclaration = getDataDeclaration' getSymbol (pure ())

getDataDeclaration' :: (MonadGet m, Ord v) => m v -> m a -> m (DataDeclaration v a)
getDataDeclaration' getV getA =
  DataDeclaration.DataDeclaration
    <$> getModifier
    <*> getA
    <*> getList getV
    <*> getList (getTuple3 getA getV (getType' getV getA))

getModifier :: MonadGet m => m DataDeclaration.Modifier
getModifier = getWord8 >>= \case
  0 -> pure DataDeclaration.Structural
  1 -> DataDeclaration.Unique <$> getText
  tag -> unknownTag "DataDeclaration.Modifier" tag

getEffectDeclaration :: MonadGet m => m (EffectDeclaration Symbol ())
getEffectDeclaration =
  DataDeclaration.EffectDeclaration <$> getDataDeclaration

getEffectDeclaration' :: (MonadGet m, Ord v) => m v -> m a -> m (EffectDeclaration v a)
getEffectDeclaration' getV getA =
  DataDeclaration.EffectDeclaration <$> getDataDeclaration' getV getA

getEither :: MonadGet m => m a -> m b -> m (Either a b)
getEither getL getR = getWord8 >>= \case
  0 -> Left <$> getL
  1 -> Right <$> getR
  tag -> unknownTag "Either" tag

getEdits :: MonadGet m => m Patch
getEdits =
  Patch <$> getRelation getReference getTermEdit
    <*> getRelation getReference getTypeEdit
