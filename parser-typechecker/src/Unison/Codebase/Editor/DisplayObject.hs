{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.Editor.DisplayObject where

import Data.Bifoldable
import Data.Bitraversable
import Data.Set qualified as Set
import U.Codebase.Reference (TermReference, TypeReference)
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.Dependencies qualified as DD
import Unison.LabeledDependency qualified as LD
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type

data DisplayObject b a = BuiltinObject b | MissingObject ShortHash | UserObject a
  deriving (Eq, Ord, Show, Functor, Generic, Foldable, Traversable)

instance Bifunctor DisplayObject where
  bimap _ _ (MissingObject sh) = MissingObject sh
  bimap f _ (BuiltinObject b) = BuiltinObject (f b)
  bimap _ f (UserObject a) = UserObject (f a)

instance Bitraversable DisplayObject where
  bitraverse f _ (BuiltinObject b) = BuiltinObject <$> f b
  bitraverse _ _ (MissingObject sh) = pure (MissingObject sh)
  bitraverse _ g (UserObject a) = UserObject <$> g a

instance Bifoldable DisplayObject where
  bifoldMap = bifoldMapDefault

toMaybe :: DisplayObject b a -> Maybe a
toMaybe = \case
  UserObject a -> Just a
  _ -> Nothing

termDisplayObjectLabeledDependencies :: TermReference -> DisplayObject (Type Symbol Ann) (Term Symbol Ann) -> (Set LD.LabeledDependency)
termDisplayObjectLabeledDependencies termRef displayObject = do
  displayObject
    & bifoldMap (Type.labeledDependencies) (Term.labeledDependencies)
    & Set.insert (LD.TermReference termRef)

typeDisplayObjectLabeledDependencies :: TypeReference -> DisplayObject () (DD.Decl Symbol Ann) -> Set LD.LabeledDependency
typeDisplayObjectLabeledDependencies typeRef displayObject = do
  displayObject
    & foldMap (DD.labeledDeclDependenciesIncludingSelfAndFieldAccessors typeRef)
