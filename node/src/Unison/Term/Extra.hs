{-# OPTIONS_GHC -fno-warn-orphans #-} -- for a Serial1 Vector and various instances

module Unison.Term.Extra where

import Control.Applicative
import Data.Bytes.Serial
import Data.Vector (Vector)
import Data.Foldable (traverse_)
import Unison.Term
import Unison.ABT.Extra ()
import Unison.Distance.Extra () -- instance for `Serial` and `Digestable`
import Unison.Type.Extra ()
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Get as Get
import qualified Data.Vector as Vector
import qualified Unison.Digest as Digest
import qualified Unison.Hash as Hash
import qualified Unison.Reference as Reference

instance Serial Literal
instance Serial1 F where
  serializeWith f e = case e of
    Lit l -> Put.putWord8 0 *> serialize l
    Blank -> Put.putWord8 1
    Ref r -> Put.putWord8 2 *> serialize r
    App a a2 -> Put.putWord8 3 *> f a *> f a2
    Ann a t -> Put.putWord8 4 *> f a *> serialize t
    Vector as -> Put.putWord8 5 *> serializeWith f as
    Lam a -> Put.putWord8 6 *> f a
    LetRec as a -> Put.putWord8 7 *> serializeWith f as *> f a
    Let b a -> Put.putWord8 8 *> f b *> f a
  deserializeWith v = Get.getWord8 >>= \tag -> case tag of
    0 -> Lit <$> deserialize
    1 -> pure Blank
    2 -> Ref <$> deserialize
    3 -> App <$> v <*> v
    4 -> Ann <$> v <*> deserialize
    5 -> Vector <$> deserializeWith v
    6 -> Lam <$> v
    7 -> LetRec <$> deserializeWith v <*> v
    8 -> Let <$> v <*> v
    _ -> fail $ "unknown tag: " ++ show tag

instance Serial1 Vector where
  serializeWith f vs = serialize (Vector.length vs) *> traverse_ f vs
  deserializeWith v = deserialize >>= \len -> sequence (Vector.replicate len v)

instance Digest.Digestable1 F where
  digest1 hashCycle hash e = case e of
    -- References are 'transparent' wrt hash - we return the precomputed hash,
    -- so for example `x = 1 + 1` and `y = x` hash the same. Thus hashing is
    -- unaffected by whether expressions are linked or not.
    Ref (Reference.Derived h) -> Hash.hashBytes h
    -- Note: start each layer with leading `1` byte, to avoid collisions with
    -- types, which start each layer with leading `0`. See `Digestable1 Type.F`
    _ -> Digest.run $ Put.putWord8 1 *> case e of
      Lit l -> Put.putWord8 0 *> serialize l
      Blank -> Put.putWord8 1
      Ref (Reference.Builtin name) -> Put.putWord8 2 *> serialize name
      Ref (Reference.Derived _) -> error "impossible, caught in above branch, but GHC can't figure this out"
      App a a2 -> Put.putWord8 3 *> serialize (hash a) *> serialize (hash a2)
      Ann a t -> Put.putWord8 4 *> serialize (hash a) *> serialize t
      Vector as -> Put.putWord8 5 *> serialize (Vector.length as)
                                  *> traverse_ (serialize . hash) as
      Lam a -> Put.putWord8 6 *> serialize (hash a)
      -- note: we use `hashCycle` to ensure result is independent of let binding order
      LetRec as a ->
        Put.putWord8 7 *> do
          hash <- hashCycle as
          serialize (hash a)
      -- here, order is significant, so don't use hashCycle
      Let b a -> Put.putWord8 8 *> serialize (hash b) *> serialize (hash a)
