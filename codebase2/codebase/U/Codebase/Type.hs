module U.Codebase.Type where

import qualified Control.Monad.Writer.Strict as Writer
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import U.Codebase.Kind (Kind)
import U.Codebase.Reference (Reference, Reference')
import qualified U.Core.ABT as ABT
import Unison.Hash (Hash)
import Unison.Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | For standalone types, like those in Term.Ann
type FT = F' Reference

-- | For potentially recursive types, like those in DataDeclaration
type FD = F' (Reference' Text (Maybe Hash))

data F' r a
  = Ref r
  | Arrow a a
  | Ann a Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a -- binder like ∀, used to introduce variables that are
  -- bound by outer type signatures, to support scoped type
  -- variables
  deriving (Foldable, Functor, Eq, Ord, Show, Traversable)

-- | Non-recursive type
type TypeT v = ABT.Term FT v ()

-- | Potentially-recursive type
type TypeD v = ABT.Term FD v ()

type TypeR r v = ABT.Term (F' r) v ()

rmap :: Ord v => (r -> r') -> ABT.Term (F' r) v a -> ABT.Term (F' r') v a
rmap f = ABT.transform \case
  Ref r -> Ref (f r)
  x -> unsafeCoerce x

typeD2T :: Ord v => Hash -> TypeD v -> TypeT v
typeD2T h = rmap $ bimap id $ Maybe.fromMaybe h

dependencies :: (Ord v, Ord r) => ABT.Term (F' r) v a -> Set r
dependencies = Writer.execWriter . ABT.visit' f
  where
    f :: Ord r => F' r a -> Writer.Writer (Set r) (F' r a)
    f t@(Ref r) = Writer.tell (Set.singleton r) $> t
    f t = pure t
