{-# OPTIONS_GHC -fno-warn-orphans #-} -- for a Serial1 Vector and various instances

module Unison.Term.Extra where

import Data.Bytes.Serial
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import Unison.ABT.Extra ()
import Unison.Distance.Extra () -- instance for `Serial`
import Unison.Literal (Literal)
import Unison.Remote.Extra ()
import Unison.Term
import Unison.Type.Extra ()
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Vector as Vector

instance Serial Literal
instance Serial1 Distributed

instance (Serial v, Ord v) => Serial1 (F v) where
  serializeWith f e = case e of
    Lit l -> Put.putWord8 0 *> serialize l
    Blank -> Put.putWord8 1
    Ref r -> Put.putWord8 2 *> serialize r
    Constructor r n -> Put.putWord8 3 *> serialize r *> serialize n
    App a a2 -> Put.putWord8 4 *> f a *> f a2
    Ann a t -> Put.putWord8 5 *> f a *> serialize t
    Vector as -> Put.putWord8 6 *> serializeWith f as
    Lam a -> Put.putWord8 7 *> f a
    LetRec as a -> Put.putWord8 8 *> serializeWith f as *> f a
    Let b a -> Put.putWord8 9 *> f b *> f a
    Distributed d -> Put.putWord8 10 *> serializeWith f d
  deserializeWith v = Get.getWord8 >>= \tag -> case tag of
    0 -> Lit <$> deserialize
    1 -> pure Blank
    2 -> Ref <$> deserialize
    3 -> Constructor <$> deserialize <*> deserialize
    4 -> App <$> v <*> v
    5 -> Ann <$> v <*> deserialize
    6 -> Vector <$> deserializeWith v
    7 -> Lam <$> v
    8 -> LetRec <$> deserializeWith v <*> v
    9 -> Let <$> v <*> v
    10 -> Distributed <$> deserializeWith v
    _ -> fail $ "unknown tag: " ++ show tag

instance Serial1 Vector where
  serializeWith f vs = serialize (Vector.length vs) *> traverse_ f vs
  deserializeWith v = deserialize >>= \len -> sequence (Vector.replicate len v)
