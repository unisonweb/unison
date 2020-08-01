{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , toReference
  , LabeledDependency
  , LabeledDependencyH
  , partition
  ) where

import Unison.Prelude hiding (fold)

import Unison.ConstructorType (ConstructorType(Data, Effect))
import Unison.Reference (Reference, ReferenceH, pattern DerivedId, IdH)
import Unison.Referent (Referent, ReferentH, pattern Ref, pattern Con, Referent'(Ref', Con'))
import Unison.Hash (Hash)
import qualified Data.Set as Set

-- dumb constructor name is private
type LabeledDependency = LabeledDependencyH Hash
newtype LabeledDependencyH h = X (Either (ReferenceH h) (ReferentH h)) deriving (Eq, Ord)
deriving instance Show (ReferenceH h) => Show (LabeledDependencyH h)

derivedType, derivedTerm :: IdH h -> LabeledDependencyH h
typeRef, termRef :: ReferenceH h -> LabeledDependencyH h
referent :: ReferentH h -> LabeledDependencyH h
dataConstructor, effectConstructor :: ReferenceH h -> Int -> LabeledDependencyH h

derivedType = X . Left . DerivedId
derivedTerm = X . Right . Ref . DerivedId
typeRef = X . Left
termRef = X . Right . Ref
referent = X . Right
dataConstructor r cid = X . Right $ Con r cid Data
effectConstructor r cid = X . Right $ Con r cid Effect

referents :: Foldable f => f Referent -> Set LabeledDependency
referents rs = Set.fromList (map referent $ toList rs)

fold :: (ReferenceH h -> a) -> (ReferentH h -> a) -> LabeledDependencyH h -> a
fold f g (X e) = either f g e

partition :: Foldable t => t LabeledDependency -> ([Reference], [Referent])
partition = partitionEithers . map (\(X e) -> e) . toList

-- | Left TypeRef | Right TermRef
toReference :: LabeledDependency -> Either Reference Reference
toReference = \case
  X (Left r)             -> Left r
  X (Right (Ref' r))     -> Right r
  X (Right (Con' r _ _)) -> Left r
