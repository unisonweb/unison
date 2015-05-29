{-# OPTIONS_GHC -fno-warn-orphans #-} -- for a Serial1 Vector and various instances

module Unison.Term.Extra where

import Data.Bytes.Serial
import Data.Vector (Vector)
import Data.Foldable (traverse_)
import Unison.Term
import Unison.ABT.Extra ()
import Unison.Distance.Extra () -- instance for `Serial` and `Digestable`
import Unison.Type.Extra ()
import qualified Data.Bytes.Put as Put
import qualified Data.Vector as Vector
import qualified Unison.Digest as Digest
import qualified Unison.Hash as Hash
import qualified Unison.Reference as Reference

instance Serial Literal
instance Serial a => Serial (Pattern a)
instance Serial1 F
instance Serial1 Vector where
  serializeWith f vs = serializeWith f (Vector.toList vs)
  deserializeWith v = Vector.fromList <$> deserializeWith v

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
