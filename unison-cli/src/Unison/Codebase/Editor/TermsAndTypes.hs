{-# LANGUAGE TypeFamilies #-}

module Unison.Codebase.Editor.TermsAndTypes where

import Data.Distributive
import Data.Functor.Adjunction
import Data.Functor.Rep

data TermOrType = TypeTag | TermTag
  deriving stock (Show, Eq, Ord)

data TermedOrTyped a = Typed a | Termed a
  deriving stock (Functor, Foldable, Traversable)
  deriving stock (Show, Eq, Ord)

unTermedOrTyped :: TermedOrTyped a -> a
unTermedOrTyped = \case
  Typed a -> a
  Termed a -> a

data TermsAndTypes a = TermsAndTypes {terms :: a, types :: a}
  deriving stock (Functor, Foldable, Traversable)

fromTypes :: Monoid a => a -> TermsAndTypes a
fromTypes a = TermsAndTypes {terms = mempty, types = a}

fromTerms :: Monoid a => a -> TermsAndTypes a
fromTerms a = TermsAndTypes {terms = a, types = mempty}

instance Semigroup a => Semigroup (TermsAndTypes a) where
  TermsAndTypes terms1 types1 <> TermsAndTypes terms2 types2 =
    TermsAndTypes (terms1 <> terms2) (types1 <> types2)

instance Monoid a => Monoid (TermsAndTypes a) where
  mempty = TermsAndTypes mempty mempty

instance Applicative TermsAndTypes where
  pure a = TermsAndTypes a a
  TermsAndTypes f g <*> TermsAndTypes a b = TermsAndTypes (f a) (g b)

instance Distributive TermsAndTypes where
  distribute = distributeRep

instance Representable TermsAndTypes where
  type Rep TermsAndTypes = TermOrType
  index tt = \case
    TermTag -> terms tt
    TypeTag -> types tt
  tabulate f = TermsAndTypes {terms = f TermTag, types = f TypeTag}

instance Adjunction TermedOrTyped TermsAndTypes where
  unit a = TermsAndTypes {terms = Termed a, types = Typed a}
  counit (Termed (TermsAndTypes {terms})) = terms
  counit (Typed (TermsAndTypes {types})) = types

labeled :: TermsAndTypes a -> TermsAndTypes (TermedOrTyped a)
labeled (TermsAndTypes {terms, types}) =
  TermsAndTypes {terms = Termed terms, types = Typed types}

labeledF :: Functor f => TermsAndTypes (f a) -> TermsAndTypes (f (TermedOrTyped a))
labeledF (TermsAndTypes {terms, types}) =
  TermsAndTypes {terms = fmap Termed terms, types = fmap Typed types}

mapWithTag :: (TermOrType -> a -> b) -> TermsAndTypes a -> TermsAndTypes b
mapWithTag f (TermsAndTypes {terms, types}) = TermsAndTypes (f TermTag terms) (f TypeTag types)
