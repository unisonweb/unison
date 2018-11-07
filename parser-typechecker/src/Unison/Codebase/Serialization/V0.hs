{-# LANGUAGE FlexibleContexts, RankNTypes, LambdaCase, RecordWildCards #-}

module Unison.Codebase.Serialization.V0 where

-- import qualified Data.Text as Text
import qualified Data.Vector                   as Vector
import qualified Unison.PatternP               as Pattern
import           Unison.PatternP                ( Pattern )
import           Control.Applicative            ( liftA2
                                                , liftA3
                                                )
import           Control.Monad                  ( replicateM )
import           Data.Bits                      ( Bits )
import           Data.Bytes.Get
import           Data.Bytes.Put
import           Data.Bytes.Serial              ( serialize
                                                , deserialize
                                                , serializeBE
                                                , deserializeBE
                                                )
import           Data.Bytes.Signed              ( Unsigned )
import           Data.Bytes.VarInt              ( VarInt(..) )
import           Data.Foldable                  ( traverse_ )
import           Data.Int                       ( Int64 )
import           Data.List                      ( elemIndex
                                                , foldl'
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.Word                      ( Word64 )
import           Unison.Codebase.Branch         ( Branch(..)
                                                , Branch0(..)
                                                )
import           Unison.Codebase.Causal         ( Causal )
import           Unison.Codebase.TermEdit       ( TermEdit )
import           Unison.Codebase.TypeEdit       ( TypeEdit )
import           Unison.Hash                    ( Hash )
import           Unison.Kind                    ( Kind )
import           Unison.Reference               ( Reference )
import           Unison.Symbol                  ( Symbol(..) )
import           Unison.Term                    ( AnnotatedTerm )
import qualified Data.ByteString               as B
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Unison.ABT                    as ABT
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Codebase.TermEdit      as TermEdit
import qualified Unison.Codebase.TypeEdit      as TypeEdit
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Hash                   as Hash
import qualified Unison.Kind                   as Kind
import qualified Unison.Reference              as Reference
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Util.Relation           ( Relation )
import qualified Unison.Util.Relation          as Relation
import qualified Unison.DataDeclaration        as DataDeclaration
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                )

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
  fail $ "unknown tag " ++ show tag ++
         " while deserializing: " ++ msg

putLength ::
  (MonadPut m, Integral n, Integral (Unsigned n),
   Bits n, Bits (Unsigned n))
  => n -> m ()
putLength = serialize . VarInt

getLength ::
  (MonadGet m, Integral n, Integral (Unsigned n),
   Bits n, Bits (Unsigned n))
  => m n
getLength = unVarInt <$> deserialize

putText :: MonadPut m => Text -> m ()
putText text = do
  let bs = encodeUtf8 text
  putLength $ B.length bs
  putByteString bs

getText :: MonadGet m => m Text
getText = do
  len <- getLength
  bs <- getBytes len
  pure $ decodeUtf8 bs

putFloat :: MonadPut m => Double -> m ()
putFloat = serializeBE

getFloat :: MonadGet m => m Double
getFloat = deserializeBE

putNat :: MonadPut m => Word64 -> m ()
putNat = putWord64be

getNat :: MonadGet m => m Word64
getNat = getWord64be

putInt :: MonadPut m => Int64 -> m ()
putInt n = serializeBE n

getInt :: MonadGet m => m Int64
getInt = deserializeBE

putBoolean :: MonadPut m => Bool -> m ()
putBoolean False = putWord8 0
putBoolean True  = putWord8 1

getBoolean :: MonadGet m => m Bool
getBoolean = go =<< getWord8 where
  go 0 = pure False
  go 1 = pure True
  go t = unknownTag "Boolean" t

putHash :: MonadPut m => Hash -> m ()
putHash h = do
  let bs = Hash.toBytes h
  putLength (B.length bs)
  putByteString bs

getHash :: MonadGet m => m Hash
getHash = do
  len <- getLength
  bs <- getBytes len
  pure $ Hash.fromBytes bs

putReference :: MonadPut m => Reference -> m ()
putReference r = case r of
  Reference.Builtin name -> do
    putWord8 0
    putText name
  Reference.Derived hash i n -> do
    putWord8 1
    putHash hash
    putLength i
    putLength n
  _ -> error "unpossible"

getReference :: MonadGet m => m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Reference.Builtin <$> getText
    1 -> Reference.DerivedPrivate_ <$> (Reference.Id <$> getHash <*> getLength <*> getLength)
    _ -> unknownTag "Reference" tag

putMaybe :: MonadPut m => Maybe a -> (a -> m ()) -> m ()
putMaybe Nothing _ = putWord8 0
putMaybe (Just a) putA = putWord8 1 *> putA a

getMaybe :: MonadGet m => m a -> m (Maybe a)
getMaybe getA = getWord8 >>= \tag -> case tag of
  0 -> pure Nothing
  1 -> Just <$> getA
  _ -> unknownTag "Maybe" tag

putFoldable
  :: (Foldable f, MonadPut m) => f a -> (a -> m ()) -> m ()
putFoldable as putA = do
  putLength (length as)
  traverse_ putA as

getFolded :: MonadGet m => (b -> a -> b) -> b -> m a -> m b
getFolded f z a =
  foldl' f z <$> getList a

getList :: MonadGet m => m a -> m [a]
getList a = getLength >>= (`replicateM` a)

putABT
  :: (MonadPut m, Foldable f, Functor f, Ord v)
  => (v -> m ())
  -> (a -> m ())
  -> (forall x . (x -> m ()) -> f x -> m ())
  -> ABT.Term f v a
  -> m ()
putABT putVar putA putF abt =
  putFoldable fvs putVar *> go (ABT.annotateBound'' abt)
  where
    fvs = Set.toList $ ABT.freeVars abt
    go (ABT.Term _ (a, env) abt) = putA a *> case abt of
      ABT.Var v      -> putWord8 0 *> putVarRef env v
      ABT.Tm f       -> putWord8 1 *> putF go f
      ABT.Abs v body -> putWord8 2 *> putVar v *> go body
      ABT.Cycle body -> putWord8 3 *> go body

    putVarRef env v = case v `elemIndex` env of
      Just i  -> putWord8 0 *> putLength i
      Nothing -> case v `elemIndex` fvs of
        Just i -> putWord8 1 *> putLength i
        Nothing -> error $ "impossible: var not free or bound"

getABT
  :: (MonadGet m, Foldable f, Functor f, Ord v)
  => m v
  -> m a
  -> (forall x . m x -> m (f x))
  -> m (ABT.Term f v a)
getABT getVar getA getF = getList getVar >>= go [] where
  go env fvs = do
    a <- getA
    tag <- getWord8
    case tag of
      0 -> do
        tag <- getWord8
        case tag of
          0 -> ABT.annotatedVar a . (env !!) <$> getLength
          1 -> ABT.annotatedVar a . (fvs !!) <$> getLength
          _ -> unknownTag "getABT.Var" tag
      1 -> ABT.tm' a <$> getF (go env fvs)
      2 -> do
        v <- getVar
        body <- go (v:env) fvs
        pure $ ABT.abs' a v body
      3 -> ABT.cycle' a <$> go env fvs
      _ -> unknownTag "getABT" tag

putKind :: MonadPut m => Kind -> m ()
putKind k = case k of
  Kind.Star      -> putWord8 0
  Kind.Arrow i o -> putWord8 1 *> putKind i *> putKind o

getKind :: MonadGet m => m Kind
getKind = getWord8 >>= \tag -> case tag of
  0 -> pure Kind.Star
  1 -> Kind.Arrow <$> getKind <*> getKind
  _ -> unknownTag "getKind" tag

putType :: (MonadPut m, Ord v)
        => (v -> m ()) -> (a -> m ())
        -> Type.AnnotatedType v a
        -> m ()
putType putVar putA typ = putABT putVar putA go typ where
  go putChild t = case t of
    Type.Ref r       -> putWord8 0 *> putReference r
    Type.Arrow i o   -> putWord8 1 *> putChild i *> putChild o
    Type.Ann t k     -> putWord8 2 *> putChild t *> putKind k
    Type.App f x     -> putWord8 3 *> putChild f *> putChild x
    Type.Effect e t  -> putWord8 4 *> putChild e *> putChild t
    Type.Effects es  -> putWord8 5 *> putFoldable es putChild
    Type.Forall body -> putWord8 6 *> putChild body

getType :: (MonadGet m, Ord v)
        => m v -> m a -> m (Type.AnnotatedType v a)
getType getVar getA = getABT getVar getA go where
  go getChild = getWord8 >>= \tag -> case tag of
    0 -> Type.Ref <$> getReference
    1 -> Type.Arrow <$> getChild <*> getChild
    2 -> Type.Ann <$> getChild <*> getKind
    3 -> Type.App <$> getChild <*> getChild
    4 -> Type.Effect <$> getChild <*> getChild
    5 -> Type.Effects <$> getList getChild
    6 -> Type.Forall <$> getChild
    _ -> unknownTag "getType" tag

putSymbol :: MonadPut m => Symbol -> m ()
putSymbol (Symbol id name) = putLength id *> putText name

getSymbol :: MonadGet m => m Symbol
getSymbol = Symbol <$> getLength <*> getText

putPattern :: MonadPut m => (a -> m ()) -> Pattern a -> m ()
putPattern putA p = case p of
  Pattern.Unbound a
    -> putWord8 0 *> putA a
  Pattern.Var a
    -> putWord8 1 *> putA a
  Pattern.Boolean a b
    -> putWord8 2 *> putA a *> putBoolean b
  Pattern.Int a n
    -> putWord8 3 *> putA a *> putInt n
  Pattern.Nat a n
    -> putWord8 4 *> putA a *> putNat n
  Pattern.Float a n
    -> putWord8 5 *> putA a *> putFloat n
  Pattern.Constructor a r cid ps
    -> putWord8 6 *> putA a *> putReference r *> putLength cid
                  *> putFoldable ps (putPattern putA)
  Pattern.As a p
    -> putWord8 7 *> putA a *> putPattern putA p
  Pattern.EffectPure a p
    -> putWord8 8 *> putA a *> putPattern putA p
  Pattern.EffectBind a r cid args k
    -> putWord8 9 *> putA a *> putReference r *> putLength cid
                  *> putFoldable args (putPattern putA) *> putPattern putA k
  _ -> error $ "unknown pattern: " ++ show p

getPattern :: MonadGet m => m a -> m (Pattern a)
getPattern getA = getWord8 >>= \tag -> case tag of
  0 -> Pattern.Unbound <$> getA
  1 -> Pattern.Var <$> getA
  2 -> Pattern.Boolean <$> getA <*> getBoolean
  3 -> Pattern.Int <$> getA <*> getInt
  4 -> Pattern.Nat <$> getA <*> getNat
  5 -> Pattern.Float <$> getA <*> getFloat
  6 -> Pattern.Constructor <$> getA <*> getReference <*> getLength <*> getList (getPattern getA)
  7 -> Pattern.As <$> getA <*> getPattern getA
  8 -> Pattern.EffectPure <$> getA <*> getPattern getA
  9 -> Pattern.EffectBind <$> getA <*> getReference <*> getLength <*> getList (getPattern getA) <*> getPattern getA
  _ -> unknownTag "Pattern" tag

putTerm :: (MonadPut m, Ord v)
        => (v -> m ()) -> (a -> m ())
        -> AnnotatedTerm v a
        -> m ()
putTerm putVar putA typ = putABT putVar putA go typ where
  go putChild t = case t of
    Term.Int n
      -> putWord8 0 *> putInt n
    Term.Nat n
      -> putWord8 1 *> putNat n
    Term.Float n
      -> putWord8 2 *> putFloat n
    Term.Boolean b
      -> putWord8 3 *> putBoolean b
    Term.Text t
      -> putWord8 4 *> putText t
    Term.Blank _
      -> error $ "can't serialize term with blanks"
    Term.Ref r
      -> putWord8 5 *> putReference r
    Term.Constructor r cid
      -> putWord8 6 *> putReference r *> putLength cid
    Term.Request r cid
      -> putWord8 7 *> putReference r *> putLength cid
    Term.Handle h a
      -> putWord8 8 *> putChild h *> putChild a
    Term.App f arg
      -> putWord8 9 *> putChild f *> putChild arg
    Term.Ann e t
      -> putWord8 10 *> putChild e *> putType putVar putA t
    Term.Vector vs
      -> putWord8 11 *> putFoldable vs putChild
    Term.If cond t f
      -> putWord8 12 *> putChild cond *> putChild t *> putChild f
    Term.And x y
      -> putWord8 13 *> putChild x *> putChild y
    Term.Or x y
      -> putWord8 14 *> putChild x *> putChild y
    Term.Lam body
      -> putWord8 15 *> putChild body
    Term.LetRec _ bs body
      -> putWord8 16 *> putFoldable bs putChild *> putChild body
    Term.Let _ b body
      -> putWord8 17 *> putChild b *> putChild body
    Term.Match s cases
      -> putWord8 18 *> putChild s *> putFoldable cases (putMatchCase putA putChild)

  putMatchCase :: MonadPut m => (a -> m ()) -> (x -> m ()) -> Term.MatchCase a x -> m ()
  putMatchCase putA putChild (Term.MatchCase pat guard body) =
    putPattern putA pat *> putMaybe guard putChild *> putChild body

getTerm :: (MonadGet m, Ord v)
        => m v -> m a -> m (Term.AnnotatedTerm v a)
getTerm getVar getA = getABT getVar getA go where
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
    10 -> Term.Ann <$> getChild <*> getType getVar getA
    11 -> Term.Vector . Vector.fromList <$> getList getChild
    12 -> Term.If <$> getChild <*> getChild <*> getChild
    13 -> Term.And <$> getChild <*> getChild
    14 -> Term.Or <$> getChild <*> getChild
    15 -> Term.Lam <$> getChild
    16 -> Term.LetRec False <$> getList getChild <*> getChild
    17 -> Term.Let False <$> getChild <*> getChild
    18 -> Term.Match <$> getChild
                     <*> getList (Term.MatchCase <$> getPattern getA <*> getMaybe getChild <*> getChild)
    _ -> unknownTag "getTerm" tag

putPair' :: MonadPut m => (a -> m ()) -> (b -> m ()) -> (a,b) -> m ()
putPair' putA putB (a,b) = putA a *> putB b

getPair :: MonadGet m => m a -> m b -> m (a,b)
getPair = liftA2 (,)

putTuple3' :: MonadPut m => (a -> m ()) -> (b -> m ()) -> (c -> m ()) -> (a,b,c) -> m ()
putTuple3' putA putB putC (a,b,c) = putA a *> putB b *> putC c

getTuple3 :: MonadGet m => m a -> m b -> m c -> m (a,b,c)
getTuple3 = liftA3 (,,)

putRelation :: MonadPut m => Relation a b -> (a -> m ()) -> (b -> m ()) -> m ()
putRelation r putA putB = putFoldable (Relation.toList r) (putPair' putA putB)

getRelation :: (MonadGet m, Ord a, Ord b) => m a -> m b -> m (Relation a b)
getRelation getA getB = Relation.fromList <$> getList (getPair getA getB)

putCausal :: MonadPut m => Causal a -> (a -> m ()) -> m ()
putCausal (Causal.One hash a) putA =
  putWord8 1 *> putHash hash *> putA a
putCausal (Causal.ConsN conss tail) putA =
  putWord8 2 *> putFoldable conss (putPair' putHash putA) *> putCausal tail putA
putCausal (Causal.Merge hash a tails) putA =
  putWord8 3 *> putHash hash *> putA a *>
    putFoldable (Map.toList tails) (putPair' putHash (`putCausal` putA))
putCausal (Causal.Cons _ _ _) _ =
  error "deserializing 'Causal': the ConsN pattern should have matched here!"

getCausal :: MonadGet m => m a -> m (Causal a)
getCausal getA = getWord8 >>= \case
  1 -> Causal.One <$> getHash <*> getA
  2 -> Causal.consN <$> getList (getPair getHash getA) <*> getCausal getA
  3 -> Causal.Merge <$> getHash <*> getA <*>
          (Map.fromList <$> getList (getPair getHash $ getCausal getA))
  x -> unknownTag "causal" x

putTermEdit :: MonadPut m => TermEdit -> m ()
putTermEdit (TermEdit.Replace r typing) =
  putWord8 1 *> putReference r *> case typing of
    TermEdit.Same -> putWord8 1
    TermEdit.Subtype -> putWord8 2
    TermEdit.Different -> putWord8 3
putTermEdit TermEdit.Deprecate = putWord8 2

getTermEdit :: MonadGet m => m TermEdit
getTermEdit = getWord8 >>= \case
  1 -> TermEdit.Replace <$> getReference <*> (getWord8 >>= \case
    1 -> pure TermEdit.Same
    2 -> pure TermEdit.Subtype
    3 -> pure TermEdit.Different
    t -> unknownTag "TermEdit.Replace" t
    )
  2 -> pure TermEdit.Deprecate
  t -> unknownTag "TermEdit" t

putTypeEdit :: MonadPut m => TypeEdit -> m ()
putTypeEdit (TypeEdit.Replace r) = putWord8 1 *> putReference r
putTypeEdit TypeEdit.Deprecate = putWord8 2

getTypeEdit :: MonadGet m => m TypeEdit
getTypeEdit = getWord8 >>= \case
  1 -> TypeEdit.Replace <$> getReference
  2 -> pure TypeEdit.Deprecate
  t -> unknownTag "TypeEdit" t

putBranch :: MonadPut m => Branch -> m ()
putBranch (Branch b) = putCausal b $ \Branch0 {..} -> do
  putRelation termNamespace putText putReference
  putRelation patternNamespace putText (putPair' putReference putLength)
  putRelation typeNamespace putText putReference
  putRelation editedTerms putReference putTermEdit
  putRelation editedTypes putReference putTypeEdit

getBranch :: MonadGet m => m Branch
getBranch = Branch <$> getCausal
  (Branch0 <$> getRelation getText getReference
           <*> getRelation getText (getPair getReference getLength)
           <*> getRelation getText getReference
           <*> getRelation getReference getTermEdit
           <*> getRelation getReference getTypeEdit)

putDataDeclaration :: (MonadPut m, Ord v)
                   => (v -> m ()) -> (a -> m ())
                   -> DataDeclaration' v a
                   -> m ()
putDataDeclaration putV putA decl = do
  putA $ DataDeclaration.annotation decl
  putFoldable (DataDeclaration.bound decl) putV
  putFoldable (DataDeclaration.constructors' decl) (putTuple3' putA putV (putType putV putA))

getDataDeclaration :: (MonadGet m, Ord v) => m v -> m a -> m (DataDeclaration' v a)
getDataDeclaration getV getA = DataDeclaration.DataDeclaration <$>
  getA <*>
  getList getV <*>
  getList (getTuple3 getA getV (getType getV getA))

putEffectDeclaration ::
  (MonadPut m, Ord v) => (v -> m ()) -> (a -> m ()) -> EffectDeclaration' v a -> m ()
putEffectDeclaration putV putA (DataDeclaration.EffectDeclaration d) =
  putDataDeclaration putV putA d

getEffectDeclaration :: (MonadGet m, Ord v) => m v -> m a -> m (EffectDeclaration' v a)
getEffectDeclaration getV getA =
  DataDeclaration.EffectDeclaration <$> getDataDeclaration getV getA

putEither :: (MonadPut m) => (a -> m ()) -> (b -> m ()) -> Either a b -> m ()
putEither putL _ (Left a) = putWord8 0 *> putL a
putEither _ putR (Right b) = putWord8 1 *> putR b

getEither :: MonadGet m => m a -> m b -> m (Either a b)
getEither getL getR = getWord8 >>= \case
  0 -> Left <$> getL
  1 -> Right <$> getR
  tag -> unknownTag "Either" tag

formatSymbol :: S.Format Symbol
formatSymbol = S.Format getSymbol putSymbol
