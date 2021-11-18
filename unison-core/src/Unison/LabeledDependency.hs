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
  , partition
  ) where

import Unison.Prelude hiding (fold)

import Unison.ConstructorType (ConstructorType(Data, Effect))
import Unison.Reference (Reference(DerivedId), Id)
import Unison.Referent (Referent, pattern Ref, pattern Con, ConstructorId)
import qualified Data.Set as Set
import qualified Unison.Referent as Referent

data LabeledDependency =
  TermReference Referent
  | TypeReference Reference
  deriving (Eq, Ord, Show)

pattern ConstructorReference ref conId conType = TermReference (Referent.Con ref conId conType)

derivedType, derivedTerm :: Id -> LabeledDependency
typeRef, termRef :: Reference -> LabeledDependency
referent :: Referent -> LabeledDependency
dataConstructor :: Reference -> ConstructorId -> LabeledDependency
effectConstructor :: Reference -> ConstructorId -> LabeledDependency

derivedType = TypeReference . DerivedId
derivedTerm = TermReference . Ref . DerivedId
typeRef = TypeReference
termRef = TermReference . Ref
referent = TermReference
dataConstructor r cid = TermReference $ Con r cid Data
effectConstructor r cid = TermReference $ Con r cid Effect

referents :: Foldable f => f Referent -> Set LabeledDependency
referents rs = Set.fromList (map referent $ toList rs)

fold :: (Reference -> a) -> (Referent -> a) -> LabeledDependency -> a
fold f _ (TypeReference r) = f r
fold _ g (TermReference r) = g r

partition :: Foldable t => t LabeledDependency -> ([Reference], [Referent])
partition =
  foldMap \case
    TypeReference ref -> ([ref], [])
    TermReference ref -> ([], [ref])

-- -- | Left TypeRef | Right TermRef
-- toReference :: LabeledDependency -> Either Reference Reference
-- toReference = \case
--   TermReference (Ref r) ->
--   TermReference (Con r)
--   X (Left r)             -> Left r
--   X (Right (Ref r))     -> Right r
--   X (Right (Con r _ _)) -> Left r
