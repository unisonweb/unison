module Unison.OpaqueDeclaration
  ( OpaqueDeclaration (..),
    OpaqueBody (..),
    arity,
    rhsDependencies,
    amap,
    vmap,
  )
where

import Unison.ABT qualified as ABT
import Unison.DataDeclaration (Modifier)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type

-- | A single body item in an opaque type declaration. Each item is a binding
-- whose term may carry an embedded type signature via 'Term.ann'.
data OpaqueBody v a = OpaqueBody
  { name :: v,
    nameAnn :: a,
    term :: Term v a
  }
  deriving (Show, Eq)

-- | An opaque type declaration: a nominal type whose underlying representation
-- is the parameterized type 'rhs', plus a list of body functions that have
-- privileged unification access to that representation. The body is part of the
-- decl at the syntactic and codebase-organization level, but each body function
-- gets its own hash at storage time; this AST carries them as plain terms.
data OpaqueDeclaration v a = OpaqueDeclaration
  { modifier :: Modifier,
    annotation :: a,
    paramNames :: [v],
    rhs :: Type v a,
    body :: [OpaqueBody v a]
  }
  deriving (Show, Eq)

-- | Number of parameters the opaque type takes.
arity :: OpaqueDeclaration v a -> Int
arity = length . paramNames

-- | Type references appearing in the RHS. (Body fns have their own
-- dependencies tracked at the term level.)
rhsDependencies :: (Ord v) => OpaqueDeclaration v a -> Set Reference
rhsDependencies = Type.dependencies . rhs

amap :: (Ord v) => (a -> a') -> OpaqueDeclaration v a -> OpaqueDeclaration v a'
amap f (OpaqueDeclaration m a ps r bs) =
  OpaqueDeclaration m (f a) ps (ABT.amap f r) (amapBody f <$> bs)

amapBody :: (Ord v) => (a -> a') -> OpaqueBody v a -> OpaqueBody v a'
amapBody f (OpaqueBody n na t) = OpaqueBody n (f na) (Term.amap f t)

vmap :: (Ord v, Ord v') => (v -> v') -> OpaqueDeclaration v a -> OpaqueDeclaration v' a
vmap f (OpaqueDeclaration m a ps r bs) =
  OpaqueDeclaration m a (f <$> ps) (ABT.vmap f r) (vmapBody f <$> bs)

vmapBody :: (Ord v, Ord v') => (v -> v') -> OpaqueBody v a -> OpaqueBody v' a
vmapBody f (OpaqueBody n na t) = OpaqueBody (f n) na (Term.vmap f t)
