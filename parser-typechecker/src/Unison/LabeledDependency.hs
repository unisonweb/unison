module Unison.LabeledDependency (derivedTerm, derivedType, termRef, typeRef, referent, dataConstructor, effectConstructor, fold, LabeledDependency) where

import Unison.Referent (Referent(Ref, Con))
import Unison.Reference (Reference(DerivedId), Id)
import Unison.ConstructorType (ConstructorType(Data, Effect))

-- dumb constructor name is private
newtype LabeledDependency = X (Either Reference Referent) deriving (Eq, Ord, Show)

derivedType, derivedTerm :: Id -> LabeledDependency
typeRef, termRef :: Reference -> LabeledDependency
referent :: Referent -> LabeledDependency
dataConstructor :: Reference -> Int -> LabeledDependency
effectConstructor :: Reference -> Int -> LabeledDependency

derivedType = X . Left . DerivedId
derivedTerm = X . Right . Ref . DerivedId
typeRef = X . Left
termRef = X . Right . Ref
referent = X . Right
dataConstructor r cid = X . Right $ Con r cid Data
effectConstructor r cid = X . Right $ Con r cid Effect

fold :: (Reference -> a) -> (Referent -> a) -> LabeledDependency -> a
fold f g (X e) = either f g e