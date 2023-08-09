module Unison.Merge () where

import U.Codebase.Sqlite.Symbol (Symbol)
import Control.Lens ((%~))
import Unison.Hash (Hash)
import qualified U.Codebase.Reference as Reference
import U.Codebase.Sqlite.HashHandle (HashHandle)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Reference (TypeReference)
import U.Codebase.Type as Type
import Unison.PatternMatchCoverage.UFMap qualified as UFMap
import Unison.Prelude
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation

data NamespaceDiff reference referent = NamespaceDiff
  { -- Mapping from old term to new term.
    termUpdates :: Map referent referent,
    -- Mapping from old type to new type.
    typeUpdates :: Map reference reference
  }

type ConstructorMapping = forall a. [a] -> [a]

computeTypeECs :: Map reference reference -> Map reference reference -> [Set reference]
computeTypeECs = undefined

computeEquivClassLookupFunc :: forall x. Ord x => Relation x x -> x -> Maybe x
computeEquivClassLookupFunc rel =
  let nodes :: Set x
      nodes = Map.keysSet (Relation.domain rel) `Set.union` Map.keysSet (Relation.range rel)

      edges :: [(x, x)]
      edges = Relation.toList rel

      ufmap :: UFMap.UFMap x x
      ufmap =
        let nodesOnly = foldl' (\b a -> UFMap.insert a a b) UFMap.empty nodes
            addEdge :: (x, x) -> UFMap.UFMap x x -> UFMap.UFMap x x
            addEdge (a, b) m0 = fromMaybe m0 $ runIdentity $ UFMap.union a b m0 \canonk nonCanonV m -> do
              let m' =
                    UFMap.alter
                      canonk
                      (error "impossible")
                      (\_ equivClassSize canonV -> UFMap.Canonical equivClassSize (min canonV nonCanonV))
                      m
              Identity (Just m')
         in foldl' (\b a -> addEdge a b) nodesOnly edges
      canonMap :: Map x x
      canonMap = UFMap.freeze ufmap
   in \k -> Map.lookup k canonMap

computeTypeUserUpdates ::
  forall a m.
  (Monad m) =>
  HashHandle -> 
  (TypeReference -> m (Decl Symbol)) ->
  (TypeReference -> TypeReference -> ConstructorMapping) ->
  Relation TypeReference TypeReference ->
  m (Relation TypeReference TypeReference)
computeTypeUserUpdates hashHandle loadDecl constructorMapping allUpdates =
  Relation.fromList <$> filterM isUserUpdate0 (Relation.toList allUpdates)
  where
    lookupCanon :: TypeReference -> TypeReference
    lookupCanon =
      let lu = computeEquivClassLookupFunc allUpdates
       in \ref -> case lu ref of
            Just x -> x
            Nothing -> error ("[impossible] lookupCanon failed to find: " <> show ref)

    isUserUpdate0 :: (TypeReference, TypeReference) -> m Bool
    isUserUpdate0 (oldRef, newRef) = do
      oldDecl <- loadDecl oldRef
      newDecl <- loadDecl newRef
      pure
        case Decl.declType oldDecl == Decl.declType newDecl of
          True -> isUserUpdateDecl oldRef oldDecl newRef newDecl
          False -> True

    isUserUpdateDecl ::
      TypeReference ->
      Decl Symbol ->
      TypeReference ->
      Decl Symbol ->
      Bool
    isUserUpdateDecl oldRef oldDecl newRef newDecl =
      or
        [ Decl.modifier oldDecl /= Decl.modifier newDecl,
          length (Decl.bound oldDecl) /= length (Decl.bound newDecl),
          length (Decl.constructorTypes oldDecl) /= length (Decl.constructorTypes newDecl),
          case (oldRef, newRef) of
            (Reference.ReferenceDerived (Reference.Id h0 _) , Reference.ReferenceDerived (Reference.Id h1 _)) ->
              any
                (\(a, b) -> not (alphaEquivalentTypesModCandidateRefs h0 h1 a b))
                ( zip
                    (Decl.constructorTypes oldDecl)
                    (constructorMapping oldRef newRef (Decl.constructorTypes newDecl))
                )
            (Reference.ReferenceBuiltin txt0, Reference.ReferenceBuiltin txt1) -> txt0 /= txt1
            (Reference.ReferenceBuiltin _, Reference.ReferenceDerived _) -> True
            (Reference.ReferenceDerived _, Reference.ReferenceBuiltin _) -> True
        ]

    alphaEquivalentTypesModCandidateRefs :: Hash -> Hash -> TypeD Symbol -> TypeD Symbol -> Bool
    alphaEquivalentTypesModCandidateRefs hlhs hrhs lhs0 rhs0 =
      let lhs = Type.rmap (Reference._RReferenceReference %~ lookupCanon) lhs0
          rhs = Type.rmap (Reference._RReferenceReference %~ lookupCanon) rhs0
       in HashHandle.toReferenceDecl hashHandle hlhs lhs == HashHandle.toReferenceDecl hashHandle hrhs rhs
