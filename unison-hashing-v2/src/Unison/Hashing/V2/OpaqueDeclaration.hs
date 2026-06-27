module Unison.Hashing.V2.OpaqueDeclaration
  ( OpaqueDeclaration (..),
    OpaqueModifier (..),
    hashOpaqueDeclaration,
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

-- | OpaqueModifier governing the salt of an opaque type's hash. 'Structural'
-- opaque decls collapse identical-RHS decls together (rarely what you want);
-- 'Unique' decls carry a per-decl GUID so two opaque decls with the same RHS
-- are nominally distinct.
data OpaqueModifier = OpaqueStructural | OpaqueUnique !Text
  deriving stock (Eq, Show)

-- | The hashable shape of an opaque type declaration's *identity*.
--
-- Only the modifier, bound type parameters, and the RHS contribute to the
-- hash. Body functions have their own term hashes; the opaque type's
-- identity is fixed by its modifier + RHS, decoupling identity from API
-- evolution.
--
-- Field names are prefixed to avoid record-field clashes when the type is
-- re-exported alongside 'Unison.Hashing.V2.TypeAlias'.
data OpaqueDeclaration v a = OpaqueDeclaration
  { opaqueAnnotation :: a,
    opaqueModifier :: OpaqueModifier,
    opaqueParamNames :: [v],
    opaqueRhs :: Type v a
  }
  deriving stock (Functor)

-- | Compute the hash of an opaque type declaration.
--
-- Opaque decls are non-recursive and stored as single-element components, so
-- the returned 'ReferenceId' always has @pos = 0@.
hashOpaqueDeclaration :: (ABT.Var v, Show v) => OpaqueDeclaration v a -> ReferenceId
hashOpaqueDeclaration od = ReferenceId (ABT.hash (toABT (void od))) 0

toABT :: (ABT.Var v) => OpaqueDeclaration v () -> ABT.Term F v ()
toABT od =
  ABT.absChain
    (opaqueParamNames od)
    (ABT.tm (Opaque (opaqueModifier od) (ABT.transform Type (opaqueRhs od))))

-- | Base functor used while hashing an opaque type declaration.
--
-- The 'Opaque' tag, carrying the modifier, distinguishes opaque-type bodies
-- from raw type bodies and from type aliases. The outer tag byte (4) places
-- opaque decls in their own collision-free namespace alongside terms (1),
-- decls (2), and aliases (3).
data F a
  = Type (TypeF a)
  | Opaque !OpaqueModifier a
  deriving stock (Foldable, Functor)

instance Hashable1 F where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
     in Hashable.accumulate $
          tag 4 : case e of
            Type t -> [tag 0, hashed (Hashable.hash1 hashCycle hash t)]
            Opaque modifier a ->
              case modifier of
                OpaqueStructural -> [tag 1, hashed (hash a)]
                OpaqueUnique guid -> [tag 2, Hashable.Text guid, hashed (hash a)]
