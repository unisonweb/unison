module U.Codebase.TypeAlias where

import U.Codebase.Reference (Reference)
import U.Codebase.Type (TypeR)
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import Unison.Prelude

-- | The v2 representation of a type alias: parameter names plus a body whose
-- references are of type @r@.
--
-- Aliases cannot be recursive, so unlike 'U.Codebase.Decl.DeclR' there is no
-- notion of a self-reference. The reference type is always a fully-qualified
-- 'Reference', not the @Reference' Text (Maybe Hash)@ used for
-- potentially-recursive decls.
data TypeAliasR r v = TypeAliasR
  { paramNames :: [v],
    body :: TypeR r v
  }
  deriving (Show)

type TypeAlias v = TypeAliasR Reference v

-- | Number of parameters the alias takes.
arity :: TypeAliasR r v -> Int
arity = length . paramNames

-- | The set of references appearing in the alias body.
dependencies :: (Ord r, Ord v) => TypeAliasR r v -> Set r
dependencies = Type.dependencies . body

vmap :: (Ord v') => (v -> v') -> TypeAliasR r v -> TypeAliasR r v'
vmap f (TypeAliasR ps b) = TypeAliasR (f <$> ps) (ABT.vmap f b)

rmap :: (Ord v) => (r -> r') -> TypeAliasR r v -> TypeAliasR r' v
rmap f (TypeAliasR ps b) = TypeAliasR ps (Type.rmap f b)
