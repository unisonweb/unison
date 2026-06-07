module U.Codebase.OpaqueDeclaration where

import U.Codebase.Reference (Reference)
import U.Codebase.Type (TypeR)
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import Unison.Prelude

-- | The hash-identity-bearing modifier on an opaque declaration. 'Unique' carries
-- a GUID that salts the hash so two opaque decls with the same RHS are
-- nominally distinct.
data OpaqueModifier
  = OpaqueStructural
  | OpaqueUnique !Text
  deriving (Eq, Show)

-- | The v2 representation of an opaque type declaration. Mirrors
-- 'U.Codebase.TypeAlias.TypeAliasR' but adds a modifier (since opaque types
-- have nominal identity) and is named distinctly so it occupies its own
-- decl/alias-style namespace at the codebase layer.
--
-- Opaque types cannot be recursive in their RHS (mentioning the LHS in the
-- RHS is rejected at parse time), so references are always fully-qualified
-- 'Reference', not the @Reference' Text (Maybe Hash)@ used for
-- potentially-recursive data/effect decls.
--
-- Body functions are *not* part of this record: they are stored as ordinary
-- terms with their own hashes and linked to their parent opaque type via the
-- membership relation (plan §2.2), which lands separately.
data OpaqueDeclarationR r v = OpaqueDeclarationR
  { modifier :: OpaqueModifier,
    paramNames :: [v],
    rhs :: TypeR r v
  }
  deriving (Show)

type OpaqueDeclaration v = OpaqueDeclarationR Reference v

-- | Number of parameters the opaque type takes.
arity :: OpaqueDeclarationR r v -> Int
arity = length . paramNames

-- | The set of references appearing in the RHS.
dependencies :: (Ord r, Ord v) => OpaqueDeclarationR r v -> Set r
dependencies = Type.dependencies . rhs

vmap :: (Ord v') => (v -> v') -> OpaqueDeclarationR r v -> OpaqueDeclarationR r v'
vmap f (OpaqueDeclarationR m ps b) = OpaqueDeclarationR m (f <$> ps) (ABT.vmap f b)

rmap :: (Ord v) => (r -> r') -> OpaqueDeclarationR r v -> OpaqueDeclarationR r' v
rmap f (OpaqueDeclarationR m ps b) = OpaqueDeclarationR m ps (Type.rmap f b)
