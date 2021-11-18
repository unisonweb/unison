{-# LANGUAGE PatternSynonyms #-}

module Unison.LabeledDependency
  ( derivedTerm
  , derivedType
  , termRef
  , typeRef
  , referent
  , dataConstructor
  , effectConstructor
  , fold
  , referents
  -- , toReference
  , LabeledDependency(..)
  , pattern ConstructorReference
  , pattern TermReference
  , partition
  ) where

import Unison.Prelude hiding (fold)

import Unison.ConstructorType (ConstructorType(Data, Effect))
import Unison.Reference (Reference(DerivedId), Id)
import Unison.Referent (Referent, ConstructorId)
import qualified Data.Set as Set
import qualified Unison.Referent as Referent

data LabeledDependency =
  TermReferent Referent
  | TypeReference Reference
  deriving (Eq, Ord, Show)

pattern ConstructorReference :: Reference -> ConstructorId -> ConstructorType -> LabeledDependency
pattern ConstructorReference ref conId conType = TermReferent (Referent.Con ref conId conType)
pattern TermReference :: Reference -> LabeledDependency
pattern TermReference ref = TermReferent (Referent.Ref ref)
{-# COMPLETE ConstructorReference, TermReference, TypeReference #-}


derivedType, derivedTerm :: Id -> LabeledDependency
typeRef, termRef :: Reference -> LabeledDependency
referent :: Referent -> LabeledDependency
dataConstructor :: Reference -> ConstructorId -> LabeledDependency
effectConstructor :: Reference -> ConstructorId -> LabeledDependency

derivedType = TypeReference . DerivedId
derivedTerm = TermReference . DerivedId
typeRef = TypeReference
termRef = TermReference
referent = TermReferent
dataConstructor r cid = ConstructorReference r cid Data
effectConstructor r cid = ConstructorReference r cid Effect

referents :: Foldable f => f Referent -> Set LabeledDependency
referents rs = Set.fromList (map referent $ toList rs)

fold :: (Reference -> a) -> (Referent -> a) -> LabeledDependency -> a
fold f _ (TypeReference r) = f r
fold _ g (TermReferent r) = g r

partition :: Foldable t => t LabeledDependency -> ([Reference], [Referent])
partition =
  foldMap \case
    TypeReference ref -> ([ref], [])
    TermReferent ref -> ([], [ref])

-- -- | Left TypeRef | Right TermRef
-- toReference :: LabeledDependency -> Either Reference Reference
-- toReference = \case
--   TermReference (Ref r) ->
--   TermReference (Con r)
--   X (Left r)             -> Left r
--   X (Right (Ref r))     -> Right r
--   X (Right (Con r _ _)) -> Left r
