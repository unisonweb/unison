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

-- | A Union Type which contains either Type References or Term Referents.
data LabeledDependency =
    TypeReference Reference
  | TermReferent Referent
  deriving (Eq, Ord, Show)

-- | Match on a TermReferent which is a Constructor.
pattern ConstructorReference :: Reference -> ConstructorId -> ConstructorType -> LabeledDependency
pattern ConstructorReference ref conId conType = TermReferent (Referent.Con ref conId conType)
-- | Match on a TermReferent which is NOT a Constructor.
pattern TermReference :: Reference -> LabeledDependency
pattern TermReference ref = TermReferent (Referent.Ref ref)
{-# COMPLETE ConstructorReference, TermReference, TypeReference #-}


derivedType :: Id -> LabeledDependency
derivedType = TypeReference . DerivedId

derivedTerm :: Id -> LabeledDependency
derivedTerm = TermReference . DerivedId

typeRef :: Reference -> LabeledDependency
typeRef = TypeReference

termRef :: Reference -> LabeledDependency
termRef = TermReference

referent :: Referent -> LabeledDependency
referent = TermReferent

dataConstructor :: Reference -> ConstructorId -> LabeledDependency
dataConstructor r cid = ConstructorReference r cid Data

effectConstructor :: Reference -> ConstructorId -> LabeledDependency
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
