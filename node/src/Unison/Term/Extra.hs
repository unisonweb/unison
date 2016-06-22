{-# OPTIONS_GHC -fno-warn-orphans #-} -- for a Serial1 Vector and various instances

module Unison.Term.Extra where

import Data.Bytes.Serial
import Data.Vector (Vector)
import Data.Foldable (traverse_)
import Unison.Term
import Unison.ABT.Extra ()
import Unison.Distance.Extra () -- instance for `Serial`
import Unison.Type.Extra ()
import Unison.Remote.Extra ()
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.Get as Get
import qualified Data.Vector as Vector

instance Serial Literal
instance Serial1 Distributed

instance (Serial v, Ord v) => Serial1 (F v) where
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
    Distributed d -> Put.putWord8 9 *> serializeWith f d
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
    9 -> Distributed <$> deserializeWith v
    _ -> fail $ "unknown tag: " ++ show tag

instance Serial1 Vector where
  serializeWith f vs = serialize (Vector.length vs) *> traverse_ f vs
  deserializeWith v = deserialize >>= \len -> sequence (Vector.replicate len v)
