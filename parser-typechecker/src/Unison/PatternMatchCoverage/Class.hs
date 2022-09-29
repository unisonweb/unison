{-# LANGUAGE FunctionalDependencies #-}

module Unison.PatternMatchCoverage.Class where

import Control.Monad.Fix (MonadFix)
import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.ListPat (ListPat)
import Unison.Type (Type)
import Unison.Var (Var)

class (Ord loc, Var vt, Var v, MonadFix m) => Pmc vt v loc m | m -> vt v loc where
  getConstructors :: Type vt loc -> m (EnumeratedConstructors vt v loc)
  getConstructorVarTypes :: Type vt loc -> ConstructorReference -> m [Type vt loc]
  fresh :: m v

data EnumeratedConstructors vt v loc
  = ConstructorType [(v, ConstructorReference, Type vt loc)]
  | SequenceType [(ListPat, [Type vt loc])]
  | BooleanType
  | OtherType
  deriving stock (Show)

traverseConstructors ::
  Applicative f =>
  (v -> ConstructorReference -> Type vt loc -> f (v, ConstructorReference, Type vt loc)) ->
  EnumeratedConstructors vt v loc ->
  f (EnumeratedConstructors vt v loc)
traverseConstructors f = \case
  ConstructorType xs -> ConstructorType <$> traverse (\(a, b, c) -> f a b c) xs
  SequenceType x -> pure (SequenceType x)
  BooleanType -> pure BooleanType
  OtherType -> pure OtherType
