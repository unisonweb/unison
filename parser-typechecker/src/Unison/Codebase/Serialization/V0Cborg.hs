{-# LANGUAGE BangPatterns, FlexibleContexts, RankNTypes #-}

module Unison.Codebase.Serialization.V0Cborg where

import qualified Unison.PatternP               as Pattern
import           Unison.PatternP                ( Pattern )
import           Control.Monad                  ( replicateM )

import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import qualified Data.ByteString.Base64        as Base64
import           Data.Foldable                  ( toList )
import           Data.List                      ( elemIndex )
import           Data.Text.Encoding             (decodeUtf8, encodeUtf8)
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
import           Data.Word                      (Word8)
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

newtype Tag = Tag Word8 deriving (Eq, Ord, Show)
newtype NrArgs = NrArgs Int deriving (Eq, Ord, Show)

tag :: Word8 -> Encoding
tag = encodeWord8

getTag :: Decoder s Tag
getTag = Tag <$> decodeWord8

getSize :: Decoder s NrArgs
getSize = NrArgs <$> decodeListLen

getDataType :: Decoder s (Tag, NrArgs)
getDataType = do
  (NrArgs len) <- getSize
  tag <- getTag
  return (tag, NrArgs $ len - 1)

putDataType :: Encoding -> [Encoding] -> Encoding
putDataType tag body = putValues (tag : body)

unknownTag :: (Monad m, Show a) => String -> a -> m x
unknownTag msg tag =
  fail $ "unknown tag " ++ show tag ++
         " while deserializing: " ++ msg

putHash :: Hash -> Encoding
putHash = encodeString . decodeUtf8 . Base64.encode . Hash.toBytes

getHash :: Decoder s Hash
getHash = do
  eB64 <- Base64.decode . encodeUtf8 <$> decodeString
  Hash.fromBytes <$> f eB64
  where f (Left err) = fail $ "couldn't Base64.decode hash, " ++ err
        f (Right hash) = return hash

putReference :: Reference -> Encoding
putReference r = case r of
  Reference.Builtin name -> putDataType (tag 0) [encodeString name]
  Reference.Derived hash i n ->
    putDataType (tag 1) [putHash hash, encodeWord64 i, encodeWord64 n]
  _ -> error "unpossible"

getReference :: Decoder s Reference
getReference = getDataType >>= \case
  (Tag 0, NrArgs 1) -> Reference.Builtin <$> decodeString
  (Tag 1, NrArgs 3) -> Reference.DerivedId <$> (Reference.Id <$> getHash <*> decodeWord64 <*> decodeWord64)
  (tag, len) -> unknownTag "Reference" (tag, len)

putReferent :: Referent -> Encoding
putReferent r = case r of
  Referent.Ref r -> putDataType (tag 0) [putReference r]
  Referent.Con r i -> putDataType (tag 1) [putReference r, encodeInt i]

getReferent :: Decoder s Referent
getReferent = getDataType >>= \case
  (Tag 0, NrArgs 1) -> Referent.Ref <$> getReference
  (Tag 1, NrArgs 2) -> Referent.Con <$> getReference <*> decodeInt
  (tag, len) -> unknownTag "getReferent" (tag, len)

putMaybe :: Maybe a -> (a -> Encoding) -> Encoding
putMaybe Nothing _ = encodeListLen 0
putMaybe (Just a) putA = encodeListLen 1 <> putA a

getMaybe :: Decoder s a -> Decoder s (Maybe a)
getMaybe getA = do
  len <- decodeListLen
  case len of
    0 -> pure Nothing
    1 -> do !a <- getA
            return (Just a)
    _ -> unknownTag "Maybe" len

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
  putValues [putFoldable fvs putVar, go (ABT.annotateBound'' abt)]
  where
    fvs = Set.toList $ ABT.freeVars abt
    go (ABT.Term _ (a, env) abt) = putA a <> case abt of
      ABT.Var v      -> putDataType (tag 0) [putVarRef env v]
      ABT.Tm f       -> putDataType (tag 1) [putF go f]
      ABT.Abs v body -> putDataType (tag 2) [putVar v, go body]
      ABT.Cycle body -> putDataType (tag 3) [go body]

    putVarRef env v = case v `elemIndex` env of
      Just i  -> putDataType (tag 0) [encodeInt i]
      Nothing -> case v `elemIndex` fvs of
        Just i -> putDataType (tag 1) [encodeInt i]
        Nothing -> error "impossible: var not free or bound"

getABT
  :: (Foldable f, Functor f, Ord v)
  => Decoder s v
  -> Decoder s a
  -> (forall x . Decoder s x -> Decoder s (f x))
  -> Decoder s (ABT.Term f v a)
getABT getVar getA getF = getSize >>= \case
  NrArgs 2 ->
    getList getVar >>= go []
    where
    go env fvs = do
      a <- getA
      getDataType >>= \case
        (Tag 0, NrArgs 1) -> getVarRef a env fvs
        (Tag 1, NrArgs 1) -> ABT.tm' a <$> getF (go env fvs)
        (Tag 2, NrArgs 2) -> getVar >>= \v -> ABT.abs' a v <$> go (v:env) fvs
        (Tag 3, NrArgs 1) -> ABT.cycle' a <$> go env fvs
        (tag, len) -> unknownTag "getABT" (tag, len)

    getVarRef a env fvs = getDataType >>= \case
      (Tag 0, NrArgs 1) -> ABT.annotatedVar a . (env !!) <$> decodeInt
      (Tag 1, NrArgs 1) -> ABT.annotatedVar a . (fvs !!) <$> decodeInt
      (tag, len) -> unknownTag "getVarRef" (tag, len)
  len -> unknownTag "getABT length" len


putKind :: Kind -> Encoding
putKind k = case k of
  Kind.Star      -> putDataType (tag 0) []
  Kind.Arrow i o -> putDataType (tag 1) [putKind i, putKind o]

getKind :: Decoder s Kind
getKind = getDataType >>= \case
  (Tag 0, NrArgs 0) -> pure Kind.Star
  (Tag 1, NrArgs 2) -> Kind.Arrow <$> getKind <*> getKind
  (tag, len) -> unknownTag "getKind" (tag, len)

putType :: (Ord v)
        => (v -> Encoding) -> (a -> Encoding)
        -> Type.AnnotatedType v a
        -> Encoding
putType putVar putA = putABT putVar putA go where
  go putChild t = case t of
    Type.Ref r -> putDataType (tag 0) [putReference r]
    Type.Arrow i o -> putDataType (tag 1) [putChild i, putChild o]
    Type.Ann t k -> putDataType (tag 2) [putChild t, putKind k]
    Type.App f x -> putDataType (tag 3) [putChild f, putChild x]
    Type.Effect e t -> putDataType (tag 4) [putChild e, putChild t]
    Type.Effects es -> putDataType (tag 5) [putFoldable es putChild]
    Type.Forall body -> putDataType (tag 6) [putChild body]

getType :: (Ord v)
        => Decoder s v -> Decoder s a -> Decoder s (Type.AnnotatedType v a)
getType getVar getA = getABT getVar getA go where
  go getChild = getDataType >>= \case
    (Tag 0, NrArgs 1) -> Type.Ref <$> getReference
    (Tag 1, NrArgs 2) -> Type.Arrow <$> getChild <*> getChild
    (Tag 2, NrArgs 2) -> Type.Ann <$> getChild <*> getKind
    (Tag 3, NrArgs 2) -> Type.App <$> getChild <*> getChild
    (Tag 4, NrArgs 2) -> Type.Effect <$> getChild <*> getChild
    (Tag 5, NrArgs 1) -> Type.Effects <$> getList getChild
    (Tag 6, NrArgs 1) -> Type.Forall <$> getChild
    (tag, len) -> unknownTag "getType" (tag, len)

putSymbol :: Symbol -> Encoding
putSymbol v@(Symbol id _) =
  putValues [encodeWord64 id, encodeString (Var.name v)]

getSymbol :: Decoder s Symbol
getSymbol = decodeListLenOf 2 >>
  Symbol <$> decodeWord64 <*> (Var.User <$> decodeString)

putSeqOp :: Pattern.SeqOp -> Encoding
putSeqOp = \case
  Pattern.Cons -> tag 0
  Pattern.Snoc -> tag 1
  Pattern.Concat -> tag 2
  op -> error $ "unpossible SeqOp" ++ show op

getSeqOp :: Decoder s Pattern.SeqOp
getSeqOp = getTag >>= \case
  Tag 0 -> pure Pattern.Cons
  Tag 1 -> pure Pattern.Snoc
  Tag 2 -> pure Pattern.Concat
  i -> unknownTag "getSeqOp" i

putPattern :: (a -> Encoding) -> Pattern a -> Encoding
putPattern putA p = case p of
  Pattern.Unbound a
    -> putDataType (tag 0) [putA a]
  Pattern.Var a
    -> putDataType (tag 1) [putA a]
  Pattern.Boolean a b
    -> putDataType (tag 2) [putA a, encodeBool b]
  Pattern.Int a n
    -> putDataType (tag 3) [putA a, encodeInt64 n]
  Pattern.Nat a n
    -> putDataType (tag 4) [putA a, encodeWord64 n]
  Pattern.Float a n
    -> putDataType (tag 5) [putA a, encodeDouble n]
  Pattern.Constructor a r cid ps
    -> putDataType (tag 6) [putA a, putReference r, encodeInt cid
                    , putFoldable ps (putPattern putA)]
  Pattern.As a p
    -> putDataType (tag 7) [putA a, putPattern putA p]
  Pattern.EffectPure a p
    -> putDataType (tag 8) [putA a, putPattern putA p]
  Pattern.EffectBind a r cid args k
    -> putDataType (tag 9) [putA a, putReference r, encodeInt cid
                    , putFoldable args (putPattern putA), putPattern putA k]
  Pattern.SequenceLiteral a ps
    -> putDataType (tag 10) [putA a, putFoldable ps (putPattern putA)]
  Pattern.SequenceOp a l op r
    -> putDataType (tag 11) [putA a, putPattern putA l, putSeqOp op, putPattern putA r]
  _ -> error $ "unknown pattern: " ++ show p

getPattern :: Decoder s a -> Decoder s (Pattern a)
getPattern getA = getDataType >>= \case
  (Tag 0,  NrArgs 1) -> Pattern.Unbound <$> getA
  (Tag 1,  NrArgs 1) -> Pattern.Var <$> getA
  (Tag 2,  NrArgs 2) -> Pattern.Boolean <$> getA <*> decodeBool
  (Tag 3,  NrArgs 2) -> Pattern.Int <$> getA <*> decodeInt64
  (Tag 4,  NrArgs 2) -> Pattern.Nat <$> getA <*> decodeWord64
  (Tag 5,  NrArgs 2) -> Pattern.Float <$> getA <*> decodeDouble
  (Tag 6,  NrArgs 4) -> Pattern.Constructor <$> getA <*> getReference <*> decodeInt <*> getList (getPattern getA)
  (Tag 7,  NrArgs 2) -> Pattern.As <$> getA <*> getPattern getA
  (Tag 8,  NrArgs 2) -> Pattern.EffectPure <$> getA <*> getPattern getA
  (Tag 9,  NrArgs 5) -> Pattern.EffectBind <$> getA <*> getReference <*> decodeInt <*> getList (getPattern getA) <*> getPattern getA
  (Tag 10, NrArgs 2) -> Pattern.SequenceLiteral <$> getA <*> getList (getPattern getA)
  (Tag 11, NrArgs 4) -> Pattern.SequenceOp <$> getA <*> gp <*> getSeqOp <*> gp
                        where gp = getPattern getA
  tag -> unknownTag "Pattern" tag

putTerm :: (Ord v)
        => (v -> Encoding) -> (a -> Encoding)
        -> AnnotatedTerm v a
        -> Encoding
putTerm putVar putA = putABT putVar putA go where
  go putChild t = case t of
    Term.Int n
      -> putDataType (tag 0) [encodeInt64 n]
    Term.Nat n
      -> putDataType (tag 1) [encodeWord64 n]
    Term.Float n
      -> putDataType (tag 2) [encodeDouble n]
    Term.Boolean b
      -> putDataType (tag 3) [encodeBool b]
    Term.Text t
      -> putDataType (tag 4) [encodeString t]
    Term.Blank _
      -> error "can't serialize term with blanks"
    Term.Ref r
      -> putDataType (tag 5) [putReference r]
    Term.Constructor r cid
      -> putDataType (tag 6) [putReference r, encodeInt cid]
    Term.Request r cid
      -> putDataType (tag 7) [putReference r, encodeInt cid]
    Term.Handle h a
      -> putDataType (tag 8) [putChild h, putChild a]
    Term.App f arg
      -> putDataType (tag 9) [putChild f, putChild arg]
    Term.Ann e t
      -> putDataType (tag 10) [putChild e, putType putVar putA t]
    Term.Sequence vs
      -> putDataType (tag 11) [putFoldable vs putChild]
    Term.If cond t f
      -> putDataType (tag 12) [putChild cond, putChild t, putChild f]
    Term.And x y
      -> putDataType (tag 13) [putChild x, putChild y]
    Term.Or x y
      -> putDataType (tag 14) [putChild x, putChild y]
    Term.Lam body
      -> putDataType (tag 15) [putChild body]
    Term.LetRec _ bs body
      -> putDataType (tag 16) [putFoldable bs putChild, putChild body]
    Term.Let _ b body
      -> putDataType (tag 17) [putChild b, putChild body]
    Term.Match s cases
      -> putDataType (tag 18) [putChild s, putFoldable cases (putMatchCase putA putChild)]

  putMatchCase :: (a -> Encoding) -> (x -> Encoding) -> Term.MatchCase a x -> Encoding
  putMatchCase putA putChild (Term.MatchCase pat guard body) =
    putValues [putPattern putA pat, putMaybe guard putChild, putChild body]

getTerm :: (Ord v)
        => Decoder s v -> Decoder s a -> Decoder s (Term.AnnotatedTerm v a)
getTerm getVar getA = getABT getVar getA go where
  go getChild = getDataType >>= \case
    (Tag 0,  NrArgs 1) -> Term.Int <$> decodeInt64
    (Tag 1,  NrArgs 1) -> Term.Nat <$> decodeWord64
    (Tag 2,  NrArgs 1) -> Term.Float <$> decodeDouble
    (Tag 3,  NrArgs 1) -> Term.Boolean <$> decodeBool
    (Tag 4,  NrArgs 1) -> Term.Text <$> decodeString
    (Tag 5,  NrArgs 1) -> Term.Ref <$> getReference
    (Tag 6,  NrArgs 2) -> Term.Constructor <$> getReference <*> decodeInt
    (Tag 7,  NrArgs 2) -> Term.Request <$> getReference <*> decodeInt
    (Tag 8,  NrArgs 2) -> Term.Handle <$> getChild <*> getChild
    (Tag 9,  NrArgs 2) -> Term.App <$> getChild <*> getChild
    (Tag 10, NrArgs 2) -> Term.Ann <$> getChild <*> getType getVar getA
    (Tag 11, NrArgs 1) -> Term.Sequence . Sequence.fromList <$> getList getChild
    (Tag 12, NrArgs 3) -> Term.If <$> getChild <*> getChild <*> getChild
    (Tag 13, NrArgs 2) -> Term.And <$> getChild <*> getChild
    (Tag 14, NrArgs 2) -> Term.Or <$> getChild <*> getChild
    (Tag 15, NrArgs 1) -> Term.Lam <$> getChild
    (Tag 16, NrArgs 2) -> Term.LetRec False <$> getList getChild <*> getChild
    (Tag 17, NrArgs 2) -> Term.Let False <$> getChild <*> getChild
    (Tag 18, NrArgs 2) -> Term.Match <$> getChild <*> getList (getMatchCase getA getChild)
    (tag, len) -> unknownTag "getTerm" (tag, len)

  getMatchCase :: Decoder s loc -> Decoder s a -> Decoder s (Term.MatchCase loc a)
  getMatchCase getA getChild = do
    len <- getSize
    case len of
      NrArgs 3 -> Term.MatchCase <$> getPattern getA <*> getMaybe getChild <*> getChild
      _ -> unknownTag "getMatchCase length" len

putValues :: [Encoding] -> Encoding
putValues es = encodeListLen (fromIntegral $ length es) <> mconcat es

putPair' :: (a -> Encoding) -> (b -> Encoding) -> (a,b) -> Encoding
putPair' putA putB (a,b) = putValues [putA a, putB b]

getPair :: Decoder s a -> Decoder s b -> Decoder s (a,b)
getPair getA getB = decodeListLenOf 2 >> (,) <$> getA <*> getB

putTuple3' :: (a -> Encoding) -> (b -> Encoding) -> (c -> Encoding) -> (a,b,c) -> Encoding
putTuple3' putA putB putC (a,b,c) = putValues [putA a, putB b, putC c]

getTuple3 :: Decoder s a -> Decoder s b -> Decoder s c -> Decoder s (a,b,c)
getTuple3 getA getB getC = decodeListLenOf 2 >> (,,) <$> getA <*> getB <*> getC

putRelation :: Relation a b -> (a -> Encoding) -> (b -> Encoding) -> Encoding
putRelation r putA putB = putFoldable (Relation.toList r) (putPair' putA putB)

getRelation :: (Ord a, Ord b) => Decoder s a -> Decoder s b -> Decoder s (Relation a b)
getRelation getA getB = Relation.fromList <$> getList (getPair getA getB)

putCausal :: Causal a -> (a -> Encoding) -> Encoding
putCausal (Causal.One hash a) putA =
  putDataType (tag 0) [putHash hash, putA a]
putCausal (Causal.ConsN conss tail) putA =
  putDataType (tag 1) [putFoldable conss (putPair' putHash putA), putCausal tail putA]
putCausal (Causal.Merge hash a tails) putA =
  putDataType (tag 2) [putHash hash, putA a,
    putFoldable (Map.toList tails) (putPair' putHash (`putCausal` putA))]
putCausal Causal.Cons {} _ =
  error "deserializing 'Causal': the ConsN pattern should have matched here!"

getCausal :: Decoder s a -> Decoder s (Causal a)
getCausal getA =
  getDataType >>= \case
    (Tag 0, NrArgs 2) -> Causal.One <$> getHash <*> getA
    (Tag 1, NrArgs 2) -> Causal.consN <$> getList (getPair getHash getA) <*> getCausal getA
    (Tag 2, NrArgs 3) -> Causal.Merge <$> getHash <*> getA <*>
                (Map.fromList <$> getList (getPair getHash $ getCausal getA))
    (tag, len) -> unknownTag "causal" (tag, len)

putTermEdit :: TermEdit -> Encoding
putTermEdit (TermEdit.Replace r typing) =
  putDataType (tag 0) [putReference r, case typing of
    TermEdit.Same -> tag 1
    TermEdit.Subtype -> tag 2
    TermEdit.Different -> tag 3 ]
putTermEdit TermEdit.Deprecate = putDataType (tag 1) []

getTermEdit :: Decoder s TermEdit
getTermEdit = getDataType >>= \case
  (Tag 0, NrArgs 2) -> TermEdit.Replace <$> getReference <*> (getTag >>= \case
    Tag 1 -> pure TermEdit.Same
    Tag 2 -> pure TermEdit.Subtype
    Tag 3 -> pure TermEdit.Different
    t -> unknownTag "TermEdit.Replace" t
    )
  (Tag 2, NrArgs 0) -> pure TermEdit.Deprecate
  (tag, len) -> unknownTag "TermEdit" (tag, len)

putTypeEdit :: TypeEdit -> Encoding
putTypeEdit (TypeEdit.Replace r) = putDataType (tag 0) [putReference r]
putTypeEdit TypeEdit.Deprecate = putDataType (tag 2) []

getTypeEdit :: Decoder s TypeEdit
getTypeEdit = getDataType >>= \case
  (Tag 0, NrArgs 1) -> TypeEdit.Replace <$> getReference
  (Tag 1, NrArgs 0) -> pure TypeEdit.Deprecate
  (tag, len) -> unknownTag "TypeEdit" (tag, len)

putBranch :: Branch -> Encoding
putBranch (Branch b) = putCausal b $ \b -> putValues [
    putRelation (Branch.termNamespace b) putName putReferent
  , putRelation (Branch.typeNamespace b) putName putReference
  , putRelation (Branch.oldTermNamespace b) putName putReferent
  , putRelation (Branch.oldTypeNamespace b) putName putReference
  , putRelation (Branch.editedTerms b) putReference putTermEdit
  , putRelation (Branch.editedTypes b) putReference putTypeEdit
  ]
  where
  putName :: Name -> Encoding
  putName = encodeString . Name.toText

getBranch :: Decoder s Branch
getBranch = decodeListLenOf 6 >> Branch <$> getCausal
  (   Branch0
  <$> getNamespace
  <*> getNamespace
  <*> getRelation getReference getTermEdit
  <*> getRelation getReference getTypeEdit
  )
  where
  getNamespace :: Decoder s Branch.Namespace
  getNamespace =
    Branch.Namespace
      <$> getRelation getName getReferent
      <*> getRelation getName getReference

  getName :: Decoder s Name
  getName = Name.unsafeFromText <$> decodeString

putDataDeclaration :: (Ord v)
                   => (v -> Encoding) -> (a -> Encoding)
                   -> DataDeclaration' v a
                   -> Encoding
putDataDeclaration putV putA decl = putValues [
   putModifier (DataDeclaration.modifier decl)
 , putA (DataDeclaration.annotation decl)
 , putFoldable (DataDeclaration.bound decl) putV
 , putFoldable (DataDeclaration.constructors' decl) (putTuple3' putA putV (putType putV putA))
 ]

getDataDeclaration :: (Ord v) => Decoder s v -> Decoder s a -> Decoder s (DataDeclaration' v a)
getDataDeclaration getV getA = decodeListLenOf 4 >>
  DataDeclaration.DataDeclaration <$>
    getModifier <*>
    getA <*>
    getList getV <*>
    getList (getTuple3 getA getV (getType getV getA))

putModifier :: DataDeclaration.Modifier -> Encoding
putModifier DataDeclaration.Structural   = putDataType (tag 0) []
putModifier (DataDeclaration.Unique txt) = putDataType (tag 1) [encodeString txt]

getModifier :: Decoder s DataDeclaration.Modifier
getModifier =  getDataType >>= \case
  (Tag 0, NrArgs 0) -> pure DataDeclaration.Structural
  (Tag 1, NrArgs 1) -> DataDeclaration.Unique <$> decodeString
  (tag, len) -> unknownTag "DataDeclaration.Modifier" (tag, len)

putEffectDeclaration ::
  (Ord v) => (v -> Encoding) -> (a -> Encoding) -> EffectDeclaration' v a -> Encoding
putEffectDeclaration putV putA (DataDeclaration.EffectDeclaration d) =
  putDataDeclaration putV putA d

getEffectDeclaration :: (Ord v) => Decoder s v -> Decoder s a -> Decoder s (EffectDeclaration' v a)
getEffectDeclaration getV getA =
  DataDeclaration.EffectDeclaration <$> getDataDeclaration getV getA

putEither :: (a -> Encoding) -> (b -> Encoding) -> Either a b -> Encoding
putEither putL _ (Left a) = putDataType (tag 0) [putL a]
putEither _ putR (Right b) = putDataType (tag 1) [putR b]

getEither :: Decoder s a -> Decoder s b -> Decoder s (Either a b)
getEither getL getR = getDataType >>= \case
  (Tag 0, NrArgs 0) -> do !a <- getL
                          return (Left a)
  (Tag 1, NrArgs 0) -> do !b <- getR
                          return (Right b)
  (tag, len) -> unknownTag "Either" (tag, len)
