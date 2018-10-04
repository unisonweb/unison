{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Unison.Codebase.Serialization.V0 where

-- import qualified Data.Text as Text
import Control.Monad (replicateM)
import Data.Bits (Bits)
import Data.Bytes.Get as R
import Data.Bytes.Put as W
import Data.Bytes.Serial (serialize, deserialize)
import Data.Bytes.Signed (Unsigned)
import Data.Bytes.VarInt (VarInt(..))
import Data.Foldable (traverse_)
import Data.List (elemIndex, foldl')
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Unison.Hash (Hash)
import Unison.Kind (Kind)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol(..))
import qualified Data.ByteString as B
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Hash as Hash
import qualified Unison.Kind as Kind
import qualified Unison.Reference as Reference
import qualified Unison.Type as Type

-- About this format:
--
-- Finalized: No
--
-- If Finalized: Yes, don't modify this file in a way that affects serialized form.
-- Instead, create a new file, V(n + 1).
-- This ensures that we have a well-defined serialized form and can read
-- and write old versions.
--

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
  Reference.Derived hash -> do
    putWord8 1
    putHash hash

getReference :: MonadGet m => m Reference
getReference = do
  tag <- getWord8
  case tag of
    0 -> Reference.Builtin <$> getText
    1 -> Reference.Derived <$> getHash
    _ -> unknownTag "Reference" tag

putFoldable
  :: (Foldable f, MonadPut m) => f a -> (a -> m ()) -> m ()
putFoldable as putA = do
  putLength (length as)
  traverse_ putA as

getFolded :: MonadGet m => (b -> a -> b) -> b -> m a -> m b
getFolded f z a = do
  len <- getLength
  as <- replicateM len a
  pure $ foldl' f z as

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
