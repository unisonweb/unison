module Unison.TypeAlias
  ( TypeAlias (..),
    arity,
    dependencies,
    labeledDependencies,
    amap,
    vmap,
  )
where

import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.LabeledDependency qualified as LD
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.Type qualified as Type

-- | A type alias: a parameterized type expression that is fully expanded at every
-- use site before the surrounding term or declaration is hashed.
--
-- Aliases are transparent: `Username` and `Text` produce the same hash when
-- `type alias Username = Text` is in scope. The alias itself is a namespace
-- entry; references tagged 'RtTypeAlias' point to it.
--
-- See @docs/type-aliases.markdown@ for the full design.
data TypeAlias v a = TypeAlias
  { paramNames :: [v],
    body :: Type v a
  }
  deriving (Show, Eq, Ord)

-- | Number of parameters the alias takes. An alias must be saturated at every
-- use site by exactly this many arguments.
arity :: TypeAlias v a -> Int
arity = length . paramNames

-- | The set of type references appearing in the alias body.
dependencies :: (Ord v) => TypeAlias v a -> Set Reference
dependencies = Type.dependencies . body

labeledDependencies :: (Ord v) => TypeAlias v a -> Set LD.LabeledDependency
labeledDependencies = Set.map LD.TypeReference . dependencies

amap :: (Ord v) => (a -> a') -> TypeAlias v a -> TypeAlias v a'
amap f (TypeAlias ps b) = TypeAlias ps (ABT.amap f b)

vmap :: (Ord v') => (v -> v') -> TypeAlias v a -> TypeAlias v' a
vmap f (TypeAlias ps b) = TypeAlias (f <$> ps) (ABT.vmap f b)
