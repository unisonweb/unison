module Unison.Merge () where

import Data.Map.Strict qualified as Map
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

candidateUpdatesToReachabilityGraph :: Relation reference reference -> reference -> reference -> Bool
candidateUpdatesToReachabilityGraph = undefined

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
