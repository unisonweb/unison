module Unison.Merge () where

import Data.Graph qualified as Graph
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.DataDeclaration (DataDeclaration, Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.Prelude
import Unison.Type (Type)
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

candidateUpdatesToReachabilityGraph ::
  forall reference.
  Ord reference =>
  Relation reference reference ->
  reference ->
  reference ->
  Bool
candidateUpdatesToReachabilityGraph inputRelation =
  let adjList :: [(reference, reference, [reference])]
      adjList = (\(i, sos) -> (i, i, Set.toList sos)) <$> Map.toList (Relation.domain inputRelation)
      (graph, lookupNode, lookupVertex) = Graph.graphFromEdges adjList
      transitiveClosure :: Map reference (Set reference)
      transitiveClosure =
        let vertices :: [reference]
            vertices = Map.keys (Relation.domain inputRelation)

            makeSingletonMap :: reference -> Map reference (Set reference)
            makeSingletonMap ref =
              let vertex :: Graph.Vertex
                  vertex = case lookupVertex ref of
                    Just v -> v
                    Nothing -> undefined

                  lookupNode' :: Graph.Vertex -> reference
                  lookupNode' v = case lookupNode v of
                    (ref, _, _) -> ref
                    
                  reachable :: [reference]
                  reachable = lookupNode' <$> Graph.reachable graph vertex
              in Map.singleton ref (Set.fromList reachable)
        in foldMap makeSingletonMap vertices
   in \a b -> case Map.lookup a transitiveClosure of
             Nothing -> False
             Just refs -> Set.member b refs

computeTypeUserUpdates ::
  forall a m reference v.
  (Monad m, Ord reference, Ord v) =>
  (reference -> m (Decl v a)) ->
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
      pure case (oldDecl, newDecl) of
        (Left oldDecl1, Left newDecl1) ->
          isUserUpdate2
            oldRef
            (Decl.toDataDecl oldDecl1)
            newRef
            (Decl.toDataDecl newDecl1)
        (Right oldDecl1, Right newDecl1) -> isUserUpdate2 oldRef oldDecl1 newRef newDecl1
        _ -> True

    isUserUpdate2 ::
      reference ->
      DataDeclaration v a ->
      reference ->
      DataDeclaration v a ->
      Bool
    isUserUpdate2 oldRef oldDecl newRef newDecl =
      let oldBounds = boundsIndices oldDecl
          newBounds = boundsIndices newDecl
          -- FIXME rename
          twiddle bounds = map (\(_, _, ty) -> ABT.vmap (bounds Map.!) ty)
       in or
            [ Decl.modifier oldDecl /= Decl.modifier newDecl,
              Map.size oldBounds /= Map.size newBounds,
              length (Decl.constructors' oldDecl) /= length (Decl.constructors' newDecl),
              any
                isUserUpdate3
                ( zip
                    (twiddle oldBounds (Decl.constructors' oldDecl))
                    (twiddle newBounds (constructorMapping oldRef newRef (Decl.constructors' newDecl)))
                )
            ]
      where
        boundsIndices :: DataDeclaration v a -> Map v Int
        boundsIndices =
          fst . foldl' step (Map.empty, 0) . Decl.bound
          where
            step (acc, i) v =
              let !acc' = Map.insert v i acc
                  !i' = i + 1
               in (acc', i')

    isUserUpdate3 :: (Type Int a, Type Int a) -> Bool
    isUserUpdate3 (oldType, newType) = undefined
