{-# LANGUAGE FunctionalDependencies #-}

module Unison.PatternMatchCoverage.Class
  ( Pmc (..),
    EnumeratedConstructors (..),
    traverseConstructors,
  )
where

import Control.Monad.Fix (MonadFix)
import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.ListPat (ListPat)
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Type (Type)
import Unison.Var (Var)

-- | A typeclass for the queries required to perform pattern match
-- coverage checking.
class (Ord loc, Var vt, Var v, MonadFix m) => Pmc vt v loc m | m -> vt v loc where
  -- | Get the constructors of a type
  getConstructors :: Type vt loc -> m (EnumeratedConstructors vt v loc)

  -- | Get the types of the arguments of a specific constructor
  getConstructorVarTypes :: Type vt loc -> ConstructorReference -> m [Type vt loc]

  -- | Get a fresh variable
  fresh :: m v

  getPrettyPrintEnv :: m PrettyPrintEnv

data EnumeratedConstructors vt v loc
  = ConstructorType [(v, ConstructorReference, Type vt loc)]
  | SequenceType [(ListPat, [Type vt loc])]
  | BooleanType
  | OtherType
  deriving stock (Show)

traverseConstructors ::
  (Applicative f) =>
  (v -> ConstructorReference -> Type vt loc -> f (v, ConstructorReference, Type vt loc)) ->
  EnumeratedConstructors vt v loc ->
  f (EnumeratedConstructors vt v loc)
traverseConstructors f = \case
  ConstructorType xs -> ConstructorType <$> traverse (\(a, b, c) -> f a b c) xs
  SequenceType x -> pure (SequenceType x)
  BooleanType -> pure BooleanType
  OtherType -> pure OtherType
