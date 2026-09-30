module Unison.TypeAlias
  ( TypeAlias (..),
    arity,
    dependencies,
    amap,
    vmap,
  )
where

import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Type (Type)
import Unison.Type qualified as Type

-- | A type alias: a parameterized type expression. Alias refs stay in
-- stored types and are expanded lazily by the typechecker (via
-- 'Unison.Typechecker.Context.whnfAlias').
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

amap :: (Ord v) => (a -> a') -> TypeAlias v a -> TypeAlias v a'
amap f (TypeAlias ps b) = TypeAlias ps (ABT.amap f b)

vmap :: (Ord v') => (v -> v') -> TypeAlias v a -> TypeAlias v' a
vmap f (TypeAlias ps b) = TypeAlias (f <$> ps) (ABT.vmap f b)
