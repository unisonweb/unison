module Unison.Hashing.V2.TypeAlias
  ( TypeAlias (..),
    hashTypeAlias,
  )
where

import Unison.ABT qualified as ABT
import Unison.Hashing.V2.ABT qualified as ABT
import Unison.Hashing.V2.Reference (ReferenceId (..))
import Unison.Hashing.V2.Tokenizable (Hashable1)
import Unison.Hashing.V2.Tokenizable qualified as Hashable
import Unison.Hashing.V2.Type (Type, TypeF)
import Unison.Prelude
import Prelude hiding (cycle)

-- | The hashable shape of a type alias.
--
-- Aliases bind a list of parameters and a body. The body is a 'Type' that may
-- reference the bound parameters as ordinary type variables. Hashing handles
-- alpha-equivalence through ABT and ability-row canonicalization through the
-- existing 'TypeF' 'Hashable1' instance — see
-- 'Unison.Hashing.V2.Type'. No special preprocessing is needed at this layer.
data TypeAlias v a = TypeAlias
  { aliasAnnotation :: a,
    paramNames :: [v],
    body :: Type v a
  }
  deriving stock (Functor)

-- | Compute the hash of a type alias.
--
-- Aliases are non-recursive and stored as single-element components, so the
-- returned 'ReferenceId' always has @pos = 0@.
hashTypeAlias :: (ABT.Var v, Show v) => TypeAlias v a -> ReferenceId
hashTypeAlias ta = ReferenceId (ABT.hash (toABT (void ta))) 0

toABT :: (ABT.Var v) => TypeAlias v () -> ABT.Term F v ()
toABT ta = ABT.absChain (paramNames ta) (ABT.tm (Alias (ABT.transform Type (body ta))))

-- | Base functor used while hashing a type alias.
--
-- The 'Alias' tag distinguishes alias bodies from raw type bodies, so a
-- standalone type @T@ and an alias body @T@ never collide. The outer tag byte
-- (3) puts aliases in their own collision-free namespace alongside terms (1)
-- and decls (2).
data F a
  = Type (TypeF a)
  | Alias a
  deriving stock (Foldable, Functor)

instance Hashable1 F where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
     in Hashable.accumulate $
          tag 3 : case e of
            Type t -> [tag 0, hashed (Hashable.hash1 hashCycle hash t)]
            Alias a -> [tag 1, hashed (hash a)]

