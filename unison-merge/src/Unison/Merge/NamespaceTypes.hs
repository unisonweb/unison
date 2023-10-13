module Unison.Merge.NamespaceTypes
  ( Defns (..),
    DefnsA,
    DefnsB,
    NamespaceTree,
    flattenNamespaceTree,
    unflattenNamespaceTree,
    mergeNamespaceTrees
  )
where

import Data.Semigroup.Generic (GenericSemigroupMonoid(..))
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.List.NonEmpty (NonEmpty, pattern (:|))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment
import Unison.Prelude
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: !terms,
    types :: !types
  }
  deriving stock (Generic, Show)
  deriving (Semigroup) via GenericSemigroupMonoid (Defns terms types)


-- haha rename or delete
type DefnsA terms types =
  Defns (BiMultimap terms Name) (BiMultimap types Name)

-- haha rename or delete
type DefnsB terms types =
  Defns (Map Name terms) (Map Name types)

-- | A namespace tree has values, and a collection of children namespace trees keyed by name segment.
type NamespaceTree a =
  Cofree (Map NameSegment) a

mergeNamespaceTrees ::
  (a -> c) ->
  (b -> c) ->
  (a -> b -> c) ->
  NamespaceTree a ->
  NamespaceTree b ->
  NamespaceTree c
mergeNamespaceTrees ac bc abc =
  let go (a :< as) (b :< bs) =
        abc a b
          :< Map.merge
            (Map.mapMaybeMissing (\_nameSeg cofreeA -> Just (ac <$> cofreeA)))
            (Map.mapMaybeMissing (\_nameSeg cofreeB -> Just (bc <$> cofreeB)))
            (Map.zipWithMaybeMatched (\_nameSeg cofreeA cofreeB -> Just (go cofreeA cofreeB)))
            as
            bs
   in go

-- | 'flattenNamespaceTree' organizes a namespace tree like
--
-- > "foo" = #foo
-- > "foo": {
-- >   "bar" = #bar
-- >   "bar": {
-- >     "baz" = #baz
-- >   }
-- > }
--
-- into an equivalent-but-flatter association between names and definitions, like
--
-- > {
-- >   "foo" = #bar,
-- >   "foo.bar" = #bar,
-- >   "foo.bar.baz" = #baz
-- > }
flattenNamespaceTree ::
  forall terms types.
  (Ord terms, Ord types) =>
  NamespaceTree (Defns (Map NameSegment terms) (Map NameSegment types)) ->
  Defns (BiMultimap terms Name) (BiMultimap types Name)
flattenNamespaceTree =
  go []
  where
    go ::
      [NameSegment] ->
      NamespaceTree (Defns (Map NameSegment terms) (Map NameSegment types)) ->
      Defns (BiMultimap terms Name) (BiMultimap types Name)
    go prefix (defns :< children) =
      foldr step (fff defns) (Map.toList children)
      where
        step ::
          (NameSegment, NamespaceTree (Defns (Map NameSegment terms) (Map NameSegment types))) ->
          Defns (BiMultimap terms Name) (BiMultimap types Name) ->
          Defns (BiMultimap terms Name) (BiMultimap types Name)
        step (name, child) (Defns accTerms accTypes) =
          let Defns childTerms childTypes = go (name : prefix) child
           in -- These unions are safe because the input namespace tree had unconflicted names
              Defns (BiMultimap.unsafeUnion accTerms childTerms) (BiMultimap.unsafeUnion accTypes childTypes)

        fff ::
          Defns (Map NameSegment terms) (Map NameSegment types) ->
          Defns (BiMultimap terms Name) (BiMultimap types Name)
        fff Defns {terms, types} =
          Defns
            { terms =
                BiMultimap.fromRange
                  ( Map.mapKeysMonotonic
                      (\name -> Name.fromReverseSegments (name :| prefix))
                      terms
                  ),
              types = BiMultimap.fromRange (Map.mapKeysMonotonic Name.fromSegment types)
            }

unflattenNamespaceTree ::
  forall terms types.
  Ord terms =>
  Ord types =>
  Defns (BiMultimap terms Name) (BiMultimap types Name) ->
  NamespaceTree (Defns (Map NameSegment terms) (Map NameSegment types))
unflattenNamespaceTree defns0 =
  let inputTerms :: [(NonEmpty NameSegment, terms)]
      inputTerms = map (first Name.segments) $ Map.toList (BiMultimap.range $ terms defns0)
      inputTypes :: [(NonEmpty NameSegment, types)]
      inputTypes = map (first Name.segments) $ Map.toList (BiMultimap.range $ types defns0)
      unflattenLevel ::
        [(NonEmpty NameSegment, terms)] ->
        [(NonEmpty NameSegment, types)] ->
        ( Defns (Map NameSegment terms) (Map NameSegment types),
          Map NameSegment ([(NonEmpty NameSegment, terms)], [(NonEmpty NameSegment, types)])
        )
      unflattenLevel terms0 types0 =
        let (terms, children0) = foldl' phi (Map.empty, Map.empty) terms0
            (types, children) = foldl' psi (Map.empty, children0) types0
            phi (ts, cs) (n :| ns, v) =
              case ns of
                [] -> (Map.insert n v ts, cs)
                n1 : restNames -> (ts, Map.insertWith (\(a, b) (c, d) -> (a ++ c, b ++ d)) n ([(n1 :| restNames, v)], []) cs)
            psi (ts, cs) (n :| ns, v) =
              case ns of
                [] -> (Map.insert n v ts, cs)
                n1 : restNames -> (ts, Map.insertWith (\(a, b) (c, d) -> (a ++ c, b ++ d)) n ([], [(n1 :| restNames, v)]) cs)
         in (Defns {terms, types}, children)

      unflatten ::
        [(NonEmpty NameSegment, terms)] ->
        [(NonEmpty NameSegment, types)] ->
        NamespaceTree (Defns (Map NameSegment terms) (Map NameSegment types))
      unflatten a b =
        let (curr, children) = unflattenLevel a b
            finalChildren = fmap (uncurry unflatten) children
         in curr :< finalChildren
   in unflatten inputTerms inputTypes
