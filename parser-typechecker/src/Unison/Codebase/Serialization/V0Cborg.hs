{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Unison.Codebase.Serialization.V0Cborg where

import qualified Unison.PatternP               as Pattern
import           Unison.PatternP                ( Pattern )
import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Control.Monad                  ( replicateM )

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding

import           Data.Foldable                  ( toList )
import           Data.List                      ( elemIndex )
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Causal         ( Causal )
import           Unison.Codebase.TermEdit       ( TermEdit )
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import           Unison.Hash                    ( Hash )
import           Unison.Kind                    ( Kind )
import           Unison.Reference               ( Reference )
import           Unison.Symbol                  ( Symbol(..) )
import           Unison.Term                    ( AnnotatedTerm )
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Sequence
import qualified Data.Set                      as Set
import qualified Unison.ABT                    as ABT
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import qualified Unison.Hash                   as Hash
import qualified Unison.Kind                   as Kind
import           Unison.Name                   (Name)
import qualified Unison.Name                   as Name
import qualified Unison.Reference              as Reference
import           Unison.Referent               (Referent)
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Util.Relation           ( Relation )
import qualified Unison.Util.Relation          as Relation
import qualified Unison.DataDeclaration        as DataDeclaration
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                )
import qualified Unison.Var                    as Var

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

unknownTag :: (Monad m, Show a) => String -> a -> m x
unknownTag msg tag =
  fail $ "unknown tag " ++ show tag ++
         " while deserializing: " ++ msg

putHash :: Hash -> Encoding
putHash = encodeBytes . Hash.toBytes

getHash :: Decoder s Hash
getHash = Hash.fromBytes <$> decodeBytes

putReference :: Reference -> Encoding
putReference r = case r of
  Reference.Builtin name -> encodeWord8 0 <> encodeString name
  Reference.Derived hash i n -> mconcat [
     encodeWord8 1
   , putHash hash
   , encodeWord64 i
   , encodeWord64 n
   ]
  _ -> error "unpossible"

getReference :: Decoder s Reference
getReference = do
  tag <- decodeWord8
  case tag of
    0 -> Reference.Builtin <$> decodeString
    1 -> Reference.DerivedId <$> (Reference.Id <$> getHash <*> decodeWord64 <*> decodeWord64)
    _ -> unknownTag "Reference" tag

putReferent :: Referent -> Encoding
putReferent r = case r of
  Referent.Ref r -> encodeWord8 0 <> putReference r
  Referent.Con r i -> encodeWord8 1 <> putReference r <> encodeInt i

getReferent :: Decoder s Referent
getReferent = do
  tag <- decodeWord8
  case tag of
    0 -> Referent.Ref <$> getReference
    1 -> Referent.Con <$> getReference <*> decodeInt
    _ -> unknownTag "getReferent" tag

putMaybe :: Maybe a -> (a -> Encoding) -> Encoding
putMaybe Nothing _ = encodeWord8 0
putMaybe (Just a) putA = encodeWord8 1 <> putA a

getMaybe :: Decoder s a -> Decoder s (Maybe a)
getMaybe getA = decodeWord8 >>= \tag -> case tag of
  0 -> pure Nothing
  1 -> Just <$> getA
  _ -> unknownTag "Maybe" tag

putFoldable :: (Functor f, Foldable f) => f a -> (a -> Encoding) -> Encoding
putFoldable as putA = len <> body where
  len  = encodeListLen (fromIntegral $ length as)
  body = mconcat (toList $ putA <$> as)

getList :: Decoder s a -> Decoder s [a]
getList a = decodeListLen >>= (`replicateM` a)

putABT :: (Foldable f, Functor f, Ord v)
  => (v -> Encoding)
  -> (a -> Encoding)
  -> (forall x . (x -> Encoding) -> f x -> Encoding)
  -> ABT.Term f v a
  -> Encoding
putABT putVar putA putF abt =
  putFoldable fvs putVar <> go (ABT.annotateBound'' abt)
  where
    fvs = Set.toList $ ABT.freeVars abt
    go (ABT.Term _ (a, env) abt) = putA a <> case abt of
      ABT.Var v      -> encodeWord8 0 <> putVarRef env v
      ABT.Tm f       -> encodeWord8 1 <> putF go f
      ABT.Abs v body -> encodeWord8 2 <> putVar v <> go body
      ABT.Cycle body -> encodeWord8 3 <> go body

    putVarRef env v = case v `elemIndex` env of
      Just i  -> encodeWord8 0 <> encodeInt i
      Nothing -> case v `elemIndex` fvs of
        Just i -> encodeWord8 1 <> encodeInt i
        Nothing -> error "impossible: var not free or bound"

getABT
  :: (Foldable f, Functor f, Ord v)
  => Decoder s v
  -> Decoder s a
  -> (forall x . Decoder s x -> Decoder s (f x))
  -> Decoder s (ABT.Term f v a)
getABT getVar getA getF = getList getVar >>= go [] where
  go env fvs = do
    a <- getA
    tag <- decodeWord8
    case tag of
      0 -> do
        tag <- decodeWord8
        case tag of
          0 -> ABT.annotatedVar a . (env !!) <$> decodeInt
          1 -> ABT.annotatedVar a . (fvs !!) <$> decodeInt
          _ -> unknownTag "getABT.Var" tag
      1 -> ABT.tm' a <$> getF (go env fvs)
      2 -> do
        v <- getVar
        body <- go (v:env) fvs
        pure $ ABT.abs' a v body
      3 -> ABT.cycle' a <$> go env fvs
      _ -> unknownTag "getABT" tag


putKind :: Kind -> Encoding
putKind k = case k of
  Kind.Star      -> encodeWord8 0
  Kind.Arrow i o -> encodeWord8 1 <> putKind i <> putKind o

getKind :: Decoder s Kind
getKind = decodeWord8 >>= \tag -> case tag of
  0 -> pure Kind.Star
  1 -> Kind.Arrow <$> getKind <*> getKind
  _ -> unknownTag "getKind" tag

putType :: (Ord v)
        => (v -> Encoding) -> (a -> Encoding)
        -> Type.AnnotatedType v a
        -> Encoding
putType putVar putA = putABT putVar putA go where
  go putChild t = case t of
    Type.Ref r -> encodeWord8 0 <> putReference r
    Type.Arrow i o -> encodeWord8 1 <> putChild i <> putChild o
    Type.Ann t k -> encodeWord8 2 <> putChild t <> putKind k
    Type.App f x -> encodeWord8 3 <> putChild f <> putChild x
    Type.Effect e t -> encodeWord8 4 <> putChild e <> putChild t
    Type.Effects es -> encodeWord8 5 <> putFoldable es putChild
    Type.Forall body -> encodeWord8 6 <> putChild body

getType :: (Ord v)
        => Decoder s v -> Decoder s a -> Decoder s (Type.AnnotatedType v a)
getType getVar getA = getABT getVar getA go where
  go getChild = decodeWord8 >>= \tag -> case tag of
    0 -> Type.Ref <$> getReference
    1 -> Type.Arrow <$> getChild <*> getChild
    2 -> Type.Ann <$> getChild <*> getKind
    3 -> Type.App <$> getChild <*> getChild
    4 -> Type.Effect <$> getChild <*> getChild
    5 -> Type.Effects <$> getList getChild
    6 -> Type.Forall <$> getChild
    _ -> unknownTag "getType" tag

putSymbol :: Symbol -> Encoding
putSymbol v@(Symbol id _) = encodeWord64 id <> encodeString (Var.name v)

getSymbol :: Decoder s Symbol
getSymbol = Symbol <$> decodeWord64 <*> (Var.User <$> decodeString)

putSeqOp :: Pattern.SeqOp -> Encoding
putSeqOp = \case
  Pattern.Cons -> encodeWord8 0
  Pattern.Snoc -> encodeWord8 1
  Pattern.Concat -> encodeWord8 2
  op -> error $ "unpossible SeqOp" ++ show op

getSeqOp :: Decoder s Pattern.SeqOp
getSeqOp = decodeWord8 >>= \case
  0 -> pure Pattern.Cons
  1 -> pure Pattern.Snoc
  2 -> pure Pattern.Concat
  i -> unknownTag "getSeqOp" i

putPattern :: (a -> Encoding) -> Pattern a -> Encoding
putPattern putA p = case p of
  Pattern.Unbound a
    -> encodeWord8 0 <> putA a
  Pattern.Var a
    -> encodeWord8 1 <> putA a
  Pattern.Boolean a b
    -> encodeWord8 2 <> putA a <> encodeBool b
  Pattern.Int a n
    -> encodeWord8 3 <> putA a <> encodeInt64 n
  Pattern.Nat a n
    -> encodeWord8 4 <> putA a <> encodeWord64 n
  Pattern.Float a n
    -> encodeWord8 5 <> putA a <> encodeDouble n
  Pattern.Constructor a r cid ps
    -> encodeWord8 6 <> putA a <> putReference r <> encodeInt cid
                     <> putFoldable ps (putPattern putA)
  Pattern.As a p
    -> encodeWord8 7 <> putA a <> putPattern putA p
  Pattern.EffectPure a p
    -> encodeWord8 8 <> putA a <> putPattern putA p
  Pattern.EffectBind a r cid args k
    -> encodeWord8 9 <> putA a <> putReference r <> encodeInt cid
                     <> putFoldable args (putPattern putA) <> putPattern putA k
  Pattern.SequenceLiteral a ps
    -> encodeWord8 10 <> putA a <> putFoldable ps (putPattern putA)
  Pattern.SequenceOp a l op r
    -> encodeWord8 11 <> putA a <> putPattern putA l <> putSeqOp op <> putPattern putA r
  _ -> error $ "unknown pattern: " ++ show p

getPattern :: Decoder s a -> Decoder s (Pattern a)
getPattern getA = decodeWord8 >>= \tag -> case tag of
  0 -> Pattern.Unbound <$> getA
  1 -> Pattern.Var <$> getA
  2 -> Pattern.Boolean <$> getA <*> decodeBool
  3 -> Pattern.Int <$> getA <*> decodeInt64
  4 -> Pattern.Nat <$> getA <*> decodeWord64
  5 -> Pattern.Float <$> getA <*> decodeDouble
  6 -> Pattern.Constructor <$> getA <*> getReference <*> decodeInt <*> getList (getPattern getA)
  7 -> Pattern.As <$> getA <*> getPattern getA
  8 -> Pattern.EffectPure <$> getA <*> getPattern getA
  9 -> Pattern.EffectBind <$> getA <*> getReference <*> decodeInt <*> getList (getPattern getA) <*> getPattern getA
  10 -> Pattern.SequenceLiteral <$> getA <*> getList (getPattern getA)
  11 -> Pattern.SequenceOp <$> getA <*> gp <*> getSeqOp <*> gp
        where gp = getPattern getA
  _ -> unknownTag "Pattern" tag

putTerm :: (Ord v)
        => (v -> Encoding) -> (a -> Encoding)
        -> AnnotatedTerm v a
        -> Encoding
putTerm putVar putA = putABT putVar putA go where
  go putChild t = case t of
    Term.Int n
      -> encodeWord8 0 <> encodeInt64 n
    Term.Nat n
      -> encodeWord8 1 <> encodeWord64 n
    Term.Float n
      -> encodeWord8 2 <> encodeDouble n
    Term.Boolean b
      -> encodeWord8 3 <> encodeBool b
    Term.Text t
      -> encodeWord8 4 <> encodeString t
    Term.Blank _
      -> error "can't serialize term with blanks"
    Term.Ref r
      -> encodeWord8 5 <> putReference r
    Term.Constructor r cid
      -> encodeWord8 6 <> putReference r <> encodeInt cid
    Term.Request r cid
      -> encodeWord8 7 <> putReference r <> encodeInt cid
    Term.Handle h a
      -> encodeWord8 8 <> putChild h <> putChild a
    Term.App f arg
      -> encodeWord8 9 <> putChild f <> putChild arg
    Term.Ann e t
      -> encodeWord8 10 <> putChild e <> putType putVar putA t
    Term.Sequence vs
      -> encodeWord8 11 <> putFoldable vs putChild
    Term.If cond t f
      -> encodeWord8 12 <> putChild cond <> putChild t <> putChild f
    Term.And x y
      -> encodeWord8 13 <> putChild x <> putChild y
    Term.Or x y
      -> encodeWord8 14 <> putChild x <> putChild y
    Term.Lam body
      -> encodeWord8 15 <> putChild body
    Term.LetRec _ bs body
      -> encodeWord8 16 <> putFoldable bs putChild <> putChild body
    Term.Let _ b body
      -> encodeWord8 17 <> putChild b <> putChild body
    Term.Match s cases
      -> encodeWord8 18 <> putChild s <> putFoldable cases (putMatchCase putA putChild)

  putMatchCase :: (a -> Encoding) -> (x -> Encoding) -> Term.MatchCase a x -> Encoding
  putMatchCase putA putChild (Term.MatchCase pat guard body) =
    putPattern putA pat <> putMaybe guard putChild <> putChild body

getTerm :: (Ord v)
        => Decoder s v -> Decoder s a -> Decoder s (Term.AnnotatedTerm v a)
getTerm getVar getA = getABT getVar getA go where
  go getChild = decodeWord8 >>= \tag -> case tag of
    0 -> Term.Int <$> decodeInt64
    1 -> Term.Nat <$> decodeWord64
    2 -> Term.Float <$> decodeDouble
    3 -> Term.Boolean <$> decodeBool
    4 -> Term.Text <$> decodeString
    5 -> Term.Ref <$> getReference
    6 -> Term.Constructor <$> getReference <*> decodeInt
    7 -> Term.Request <$> getReference <*> decodeInt
    8 -> Term.Handle <$> getChild <*> getChild
    9 -> Term.App <$> getChild <*> getChild
    10 -> Term.Ann <$> getChild <*> getType getVar getA
    11 -> Term.Sequence . Sequence.fromList <$> getList getChild
    12 -> Term.If <$> getChild <*> getChild <*> getChild
    13 -> Term.And <$> getChild <*> getChild
    14 -> Term.Or <$> getChild <*> getChild
    15 -> Term.Lam <$> getChild
    16 -> Term.LetRec False <$> getList getChild <*> getChild
    17 -> Term.Let False <$> getChild <*> getChild
    18 -> Term.Match <$> getChild
                     <*> getList (Term.MatchCase <$> getPattern getA <*> getMaybe getChild <*> getChild)
    _ -> unknownTag "getTerm" tag

putPair' :: (a -> Encoding) -> (b -> Encoding) -> (a,b) -> Encoding
putPair' putA putB (a,b) = putA a <> putB b

getPair :: Decoder s a -> Decoder s b -> Decoder s (a,b)
getPair = liftA2 (,)

putTuple3' :: (a -> Encoding) -> (b -> Encoding) -> (c -> Encoding) -> (a,b,c) -> Encoding
putTuple3' putA putB putC (a,b,c) = putA a <> putB b <> putC c

getTuple3 :: Decoder s a -> Decoder s b -> Decoder s c -> Decoder s (a,b,c)
getTuple3 = liftA3 (,,)

putRelation :: Relation a b -> (a -> Encoding) -> (b -> Encoding) -> Encoding
putRelation r putA putB = putFoldable (Relation.toList r) (putPair' putA putB)

getRelation :: (Ord a, Ord b) => Decoder s a -> Decoder s b -> Decoder s (Relation a b)
getRelation getA getB = Relation.fromList <$> getList (getPair getA getB)

putCausal :: Causal a -> (a -> Encoding) -> Encoding
putCausal (Causal.One hash a) putA =
  encodeWord8 1 <> putHash hash <> putA a
putCausal (Causal.ConsN conss tail) putA =
  encodeWord8 2 <> putFoldable conss (putPair' putHash putA) <> putCausal tail putA
putCausal (Causal.Merge hash a tails) putA =
  encodeWord8 3 <> putHash hash <> putA a <>
    putFoldable (Map.toList tails) (putPair' putHash (`putCausal` putA))
putCausal (Causal.Cons {}) _ =
  error "deserializing 'Causal': the ConsN pattern should have matched here!"

getCausal :: Decoder s a -> Decoder s (Causal a)
getCausal getA = decodeWord8 >>= \case
  1 -> Causal.One <$> getHash <*> getA
  2 -> Causal.consN <$> getList (getPair getHash getA) <*> getCausal getA
  3 -> Causal.Merge <$> getHash <*> getA <*>
          (Map.fromList <$> getList (getPair getHash $ getCausal getA))
  x -> unknownTag "causal" x

putTermEdit :: TermEdit -> Encoding
putTermEdit (TermEdit.Replace r typing) =
  encodeWord8 1 <> putReference r <> case typing of
    TermEdit.Same -> encodeWord8 1
    TermEdit.Subtype -> encodeWord8 2
    TermEdit.Different -> encodeWord8 3
putTermEdit TermEdit.Deprecate = encodeWord8 2

getTermEdit :: Decoder s TermEdit
getTermEdit = decodeWord8 >>= \case
  1 -> TermEdit.Replace <$> getReference <*> (decodeWord8 >>= \case
    1 -> pure TermEdit.Same
    2 -> pure TermEdit.Subtype
    3 -> pure TermEdit.Different
    t -> unknownTag "TermEdit.Replace" t
    )
  2 -> pure TermEdit.Deprecate
  t -> unknownTag "TermEdit" t

putTypeEdit :: TypeEdit -> Encoding
putTypeEdit (TypeEdit.Replace r) = encodeWord8 1 <> putReference r
putTypeEdit TypeEdit.Deprecate = encodeWord8 2

getTypeEdit :: Decoder s TypeEdit
getTypeEdit = decodeWord8 >>= \case
  1 -> TypeEdit.Replace <$> getReference
  2 -> pure TypeEdit.Deprecate
  t -> unknownTag "TypeEdit" t

putBranch :: Branch -> Encoding
putBranch (Branch b) = putCausal b $ \b -> mconcat [
    putRelation (Branch.termNamespace b) putName putReferent
  , putRelation (Branch.typeNamespace b) putName putReference
  , putRelation (Branch.oldTermNamespace b) putName putReferent
  , putRelation (Branch.oldTypeNamespace b) putName putReference
  , putRelation (Branch.editedTerms b) putReference putTermEdit
  , putRelation (Branch.editedTypes b) putReference putTypeEdit
  ]

putName :: Name -> Encoding
putName = encodeString . Name.toText

getName :: Decoder s Name
getName = Name.unsafeFromText <$> decodeString

getNamespace :: Decoder s Branch.Namespace
getNamespace =
  Branch.Namespace
    <$> getRelation getName getReferent
    <*> getRelation getName getReference

getBranch :: Decoder s Branch
getBranch = Branch <$> getCausal
  (   Branch0
  <$> getNamespace
  <*> getNamespace
  <*> getRelation getReference getTermEdit
  <*> getRelation getReference getTypeEdit
  )

putDataDeclaration :: (Ord v)
                   => (v -> Encoding) -> (a -> Encoding)
                   -> DataDeclaration' v a
                   -> Encoding
putDataDeclaration putV putA decl = mconcat [
   putModifier (DataDeclaration.modifier decl)
 , putA (DataDeclaration.annotation decl)
 , putFoldable (DataDeclaration.bound decl) putV
 , putFoldable (DataDeclaration.constructors' decl) (putTuple3' putA putV (putType putV putA))
 ]

getDataDeclaration :: (Ord v) => Decoder s v -> Decoder s a -> Decoder s (DataDeclaration' v a)
getDataDeclaration getV getA = DataDeclaration.DataDeclaration <$>
  getModifier <*>
  getA <*>
  getList getV <*>
  getList (getTuple3 getA getV (getType getV getA))

putModifier :: DataDeclaration.Modifier -> Encoding
putModifier DataDeclaration.Structural   = encodeWord8 0
putModifier (DataDeclaration.Unique txt) = encodeWord8 1 <> encodeString txt

getModifier :: Decoder s DataDeclaration.Modifier
getModifier = decodeWord8 >>= \case
  0 -> pure DataDeclaration.Structural
  1 -> DataDeclaration.Unique <$> decodeString
  tag -> unknownTag "DataDeclaration.Modifier" tag

putEffectDeclaration ::
  (Ord v) => (v -> Encoding) -> (a -> Encoding) -> EffectDeclaration' v a -> Encoding
putEffectDeclaration putV putA (DataDeclaration.EffectDeclaration d) =
  putDataDeclaration putV putA d

getEffectDeclaration :: (Ord v) => Decoder s v -> Decoder s a -> Decoder s (EffectDeclaration' v a)
getEffectDeclaration getV getA =
  DataDeclaration.EffectDeclaration <$> getDataDeclaration getV getA

putEither :: (a -> Encoding) -> (b -> Encoding) -> Either a b -> Encoding
putEither putL _ (Left a) = encodeWord8 0 <> putL a
putEither _ putR (Right b) = encodeWord8 1 <> putR b

getEither :: Decoder s a -> Decoder s b -> Decoder s (Either a b)
getEither getL getR = decodeWord8 >>= \case
  0 -> Left <$> getL
  1 -> Right <$> getR
  tag -> unknownTag "Either" tag
