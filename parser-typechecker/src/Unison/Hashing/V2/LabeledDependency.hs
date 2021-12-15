{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE PatternSynonyms #-}

module Unison.Hashing.V2.LabeledDependency
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
  , partition
  ) where

import Unison.Prelude hiding (fold)

import qualified Data.Set as Set
import Unison.Hashing.V2.Reference (Id, Reference (DerivedId))
import Unison.Hashing.V2.Referent (ConstructorId, Referent, pattern Con, pattern Ref)
import Unison.ConstructorType (ConstructorType (Data, Effect))

-- dumb constructor name is private
newtype LabeledDependency = X (Either Reference Referent) deriving (Eq, Ord, Show)

derivedType, derivedTerm :: Id -> LabeledDependency
typeRef, termRef :: Reference -> LabeledDependency
referent :: Referent -> LabeledDependency
dataConstructor :: Reference -> ConstructorId -> LabeledDependency
effectConstructor :: Reference -> ConstructorId -> LabeledDependency

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
  X (Left r)             -> Left r
  X (Right (Ref r))     -> Right r
  X (Right (Con r _ _)) -> Left r
