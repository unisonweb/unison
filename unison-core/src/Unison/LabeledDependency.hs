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
    referents,
    toReference,
    LabeledDependency,
    partition,
  )
where

import qualified Data.Set as Set
import Unison.ConstructorType (ConstructorType (Data, Effect))
import Unison.Prelude hiding (fold)
import Unison.Reference (Id, Reference (DerivedId))
import Unison.Referent (Referent, Referent' (Con', Ref'), pattern Con, pattern Ref)

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

referents :: Foldable f => f Referent -> Set LabeledDependency
referents rs = Set.fromList (map referent $ toList rs)

fold :: (Reference -> a) -> (Referent -> a) -> LabeledDependency -> a
fold f g (X e) = either f g e

partition :: Foldable t => t LabeledDependency -> ([Reference], [Referent])
partition = partitionEithers . map (\(X e) -> e) . toList

-- | Left TypeRef | Right TermRef
toReference :: LabeledDependency -> Either Reference Reference
toReference = \case
  X (Left r) -> Left r
  X (Right (Ref' r)) -> Right r
  X (Right (Con' r _ _)) -> Left r
