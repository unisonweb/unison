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
    LabeledDependency (..),
    pattern ConReference,
    pattern TermReference,
    pattern BuiltinTerm,
    pattern BuiltinType,
    pattern DerivedTerm,
    pattern DerivedType,
    pattern DataConstructor,
    pattern EffectConstructor,
    partition,
  )
where

import Data.Set qualified as Set
import Unison.ConstructorReference (ConstructorReference)
import Unison.ConstructorType (ConstructorType (Data, Effect))
import Unison.Prelude hiding (fold)
import Unison.Reference (Id, Reference (Builtin, DerivedId))
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

pattern DerivedType, DerivedTerm :: Id -> LabeledDependency
pattern DerivedType i = TypeReference (DerivedId i)
pattern DerivedTerm i = TermReference (DerivedId i)

pattern BuiltinType, BuiltinTerm :: Text -> LabeledDependency
pattern BuiltinType t = TypeReference (Builtin t)
pattern BuiltinTerm t = TermReference (Builtin t)

pattern DataConstructor, EffectConstructor :: ConstructorReference -> LabeledDependency
pattern DataConstructor ref = ConReference ref Data
pattern EffectConstructor ref = ConReference ref Effect

{- ORMOLU_DISABLE -}
-- Y = type, Ct = constructor, M = term
-- B = builtin, D = derived (or Data), E = effect
-- {} mean grouping of multiple patterns that together are complete relative to some other pattern
{-# COMPLETE BuiltinType, DerivedType, DataConstructor, EffectConstructor, BuiltinTerm, DerivedTerm #-} --  YB YD   CtD CtE   MB MD
{-# COMPLETE TypeReference, DataConstructor, EffectConstructor, BuiltinTerm, DerivedTerm #-}            -- {YB YD}  CtD CtE   MB MD
{-# COMPLETE BuiltinType, DerivedType, ConReference, BuiltinTerm, DerivedTerm #-}                       --  YB YD  {CtD CtE}  MB MD
{-# COMPLETE BuiltinType, DerivedType, DataConstructor, EffectConstructor, TermReference #-}            --  YB YD   CtD CtE  {MB MD}
{-# COMPLETE TypeReference, DataConstructor, EffectConstructor, TermReference #-}                       -- {YB YD}  CtD CtE  {MB MD}
{-# COMPLETE TypeReference, ConReference, BuiltinTerm, DerivedTerm #-}                                  -- {YB YD} {CtD CtE}  MB MD
{-# COMPLETE BuiltinType, DerivedType, ConReference, TermReference #-}                                  --  YB YD  {CtD CtE} {MB MD}
{-# COMPLETE TypeReference, ConReference, TermReference #-}                                             -- {YB YD} {CtD CtE} {MB MD}
{-# COMPLETE BuiltinType, DerivedType, TermReferent #-}                                                 --  YB YD  {CtD CtE   MB MD}
-- default set                                                                                          -- {YB YD} {CtD CtE   MB MD}
{- ORMOLU_ENABLE -}

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

referents :: (Foldable f) => f Referent -> Set LabeledDependency
referents rs = Set.fromList (map referent $ toList rs)

fold :: (Reference -> a) -> (Referent -> a) -> LabeledDependency -> a
fold f _ (TypeReference r) = f r
fold _ g (TermReferent r) = g r

partition :: (Foldable t) => t LabeledDependency -> ([Reference], [Referent])
partition =
  foldMap \case
    TypeReference ref -> ([ref], [])
    TermReferent ref -> ([], [ref])
