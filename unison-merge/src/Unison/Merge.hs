module Unison.Merge () where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Decl (Decl, DeclR)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Referent (Referent)
import U.Codebase.Type ()
import U.Codebase.Type as Type
import Unison.ABT qualified as ABT
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
  forall a m v.
  (Monad m, Ord v) =>
  (Decl.TypeRef -> m (Decl v)) ->
  (Decl.TypeRef -> Decl.TypeRef -> ConstructorMapping) ->
  Relation Decl.TypeRef Decl.TypeRef ->
  m (Relation Decl.TypeRef Decl.TypeRef)
computeTypeUserUpdates loadDecl constructorMapping allUpdates =
  Relation.fromList <$> filterM isUserUpdate0 (Relation.toList allUpdates)
  where
    lookupCanon :: Decl.TypeRef -> Decl.TypeRef
    lookupCanon =
      let lu = computeEquivClassLookupFunc allUpdates
       in \ref -> case lu ref of
            Just x -> x
            Nothing -> error ("[impossible] lookupCanon failed to find: " <> show ref)

    isUserUpdate0 :: (Decl.TypeRef, Decl.TypeRef) -> m Bool
    isUserUpdate0 (oldRef, newRef) = do
      oldDecl <- loadDecl oldRef
      newDecl <- loadDecl newRef
      pure
        case Decl.declType oldDecl == Decl.declType newDecl of
          True -> isUserUpdate2 oldRef oldDecl newRef newDecl
          False -> True

    isUserUpdate2 ::
      Decl.TypeRef ->
      DeclR Decl.TypeRef v ->
      Decl.TypeRef ->
      DeclR Decl.TypeRef v ->
      Bool
    isUserUpdate2 oldRef oldDecl newRef newDecl =
      or
        [ Decl.modifier oldDecl /= Decl.modifier newDecl,
          length (Decl.bound oldDecl) /= length (Decl.bound newDecl),
          length (Decl.constructorTypes oldDecl) /= length (Decl.constructorTypes newDecl),
          any
            isUserUpdate3
            ( zip
                (Decl.constructorTypes oldDecl)
                (constructorMapping oldRef newRef (Decl.constructorTypes newDecl))
            )
        ]

    isUserUpdate3 :: (TypeR Decl.TypeRef v, TypeR Decl.TypeRef v) -> Bool
    isUserUpdate3 (lhs0, rhs0) =
      let lhs = Type.rmap lookupCanon lhs0
          rhs = Type.rmap lookupCanon rhs0
       in False
