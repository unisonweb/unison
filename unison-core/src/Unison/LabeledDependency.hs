{-# LANGUAGE PatternSynonyms #-}

module Unison.LabeledDependency
  ( derivedTerm,
    derivedType,
    termRef,
    typeRef,
    referent,
    dataConstructor,
    effectConstructor,
    fold,
    LabeledDependency (..),
    pattern ConReference,
    pattern TermReference,
    partition,
  )
where

import Unison.ConstructorReference (ConstructorReference)
import Unison.ConstructorType (ConstructorType (Data, Effect))
import Unison.Reference (Id, Reference, Reference' (DerivedId))
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent

-- | A Union Type which contains either Type References or Term Referents.
data LabeledDependency
  = TypeReference Reference
  | TermReferent Referent
  deriving (Eq, Ord, Show)

-- | Match on a TermReferent which is a Constructor.
pattern ConReference :: ConstructorReference -> ConstructorType -> LabeledDependency
pattern ConReference ref conType = TermReferent (Referent.Con ref conType)

-- | Match on a TermReferent which is NOT a Constructor.
pattern TermReference :: Reference -> LabeledDependency
pattern TermReference ref = TermReferent (Referent.Ref ref)

{-# COMPLETE ConReference, TermReference, TypeReference #-}

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

dataConstructor :: ConstructorReference -> LabeledDependency
dataConstructor r = ConReference r Data

effectConstructor :: ConstructorReference -> LabeledDependency
effectConstructor r = ConReference r Effect

fold :: (Reference -> a) -> (Referent -> a) -> LabeledDependency -> a
fold f _ (TypeReference r) = f r
fold _ g (TermReferent r) = g r

partition :: (Foldable t) => t LabeledDependency -> ([Reference], [Referent])
partition =
  foldMap \case
    TypeReference ref -> ([ref], [])
    TermReferent ref -> ([], [ref])
