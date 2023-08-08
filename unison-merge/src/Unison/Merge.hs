module Unison.Merge () where


import Data.Map.Strict qualified as Map
import Unison.ABT qualified as ABT
import U.Codebase.Decl (Decl)
import U.Codebase.Decl qualified as Decl
import Unison.Prelude
import U.Codebase.Type ()
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
          -- FIXME rename
       in or
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
