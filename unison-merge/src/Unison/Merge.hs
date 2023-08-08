module Unison.Merge () where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Type ()
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
  forall a m reference v.
  (Monad m, Ord reference, Ord v) =>
  (reference -> m (Decl v)) ->
  (reference -> reference -> ConstructorMapping) ->
  Relation reference reference ->
  m (Relation reference reference)
computeTypeUserUpdates loadDecl constructorMapping allUpdates =
  Relation.fromList <$> filterM isUserUpdate0 (Relation.toList allUpdates)
  where
    isUserUpdate0 :: (reference, reference) -> m Bool
    isUserUpdate0 (oldRef, newRef) = do
      oldDecl <- loadDecl oldRef
      newDecl <- loadDecl newRef
      pure
        case Decl.declType oldDecl == Decl.declType newDecl of
          True -> isUserUpdate2 oldRef oldDecl newRef newDecl
          False -> True

    isUserUpdate2 ::
      reference ->
      Decl v ->
      reference ->
      Decl v ->
      Bool
    isUserUpdate2 oldRef oldDecl newRef newDecl =
      let oldBounds = boundsIndices oldDecl
          newBounds = boundsIndices newDecl
       in -- FIXME rename
          or
            [ Decl.modifier oldDecl /= Decl.modifier newDecl,
              Map.size oldBounds /= Map.size newBounds,
              length (Decl.constructorTypes oldDecl) /= length (Decl.constructorTypes newDecl),
              any
                isUserUpdate3
                ( zip
                    (Decl.constructorTypes oldDecl)
                    (constructorMapping oldRef newRef (Decl.constructorTypes newDecl))
                )
            ]
      where
        boundsIndices :: Decl v -> Map v Int
        boundsIndices =
          fst . foldl' step (Map.empty, 0) . Decl.bound
          where
            step (acc, i) v =
              let !acc' = Map.insert v i acc
                  !i' = i + 1
               in (acc', i')

isUserUpdate3 :: (Decl.Type v, Decl.Type v) -> Bool
isUserUpdate3 = undefined
