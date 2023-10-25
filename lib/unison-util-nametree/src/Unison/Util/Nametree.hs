module Unison.Util.Nametree
  ( -- * Nametree
    Nametree (..),
    traverseNametreeWithName,
    flattenNametree,
    unflattenNametree,

    -- * Definitions
    Defns (..),
  )
where

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty, pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (Semialign (alignWith), Unzip (unzipWith), Zip (zipWith))
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.These (These (..), these)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment
import Unison.Prelude
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Prelude hiding (zipWith)

-- | A nametree has a value, and a collection of children nametrees keyed by name segment.
data Nametree a = Nametree
  { value :: !a,
    children :: !(Map NameSegment (Nametree a))
  }
  deriving stock (Functor, Generic, Show)

instance Semialign Nametree where
  alignWith :: (These a b -> c) -> Nametree a -> Nametree b -> Nametree c
  alignWith f (Nametree x xs) (Nametree y ys) =
    Nametree (f (These x y)) (alignWith (these (fmap (f . This)) (fmap (f . That)) (alignWith f)) xs ys)

instance Zip Nametree where
  zipWith :: (a -> b -> c) -> Nametree a -> Nametree b -> Nametree c
  zipWith f (Nametree x xs) (Nametree y ys) =
    Nametree (f x y) (zipWith (zipWith f) xs ys)

instance Unzip Nametree where
  unzipWith :: (c -> (a, b)) -> Nametree c -> (Nametree a, Nametree b)
  unzipWith f (Nametree x xs) =
    (Nametree y ys, Nametree z zs)
    where
      (y, z) = f x
      (ys, zs) = unzipWith (unzipWith f) xs

-- | Traverse over a nametree, with access to the list of name segments (in reverse order) leading to each value.
traverseNametreeWithName :: Applicative f => ([NameSegment] -> a -> f b) -> Nametree a -> f (Nametree b)
traverseNametreeWithName f =
  go []
  where
    go names (Nametree x xs) =
      Nametree <$> f names x <*> Map.traverseWithKey (\name -> go (name : names)) xs

-- | 'flattenNametree' organizes a nametree like
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
flattenNametree ::
  forall terms types.
  (Ord terms, Ord types) =>
  Nametree (Defns (Map NameSegment terms) (Map NameSegment types)) ->
  Defns (BiMultimap terms Name) (BiMultimap types Name)
flattenNametree =
  go []
  where
    go ::
      [NameSegment] ->
      Nametree (Defns (Map NameSegment terms) (Map NameSegment types)) ->
      Defns (BiMultimap terms Name) (BiMultimap types Name)
    go prefix (Nametree defns0 children) =
      foldr step defns1 (Map.toList children)
      where
        step ::
          (NameSegment, Nametree (Defns (Map NameSegment terms) (Map NameSegment types))) ->
          Defns (BiMultimap terms Name) (BiMultimap types Name) ->
          Defns (BiMultimap terms Name) (BiMultimap types Name)
        step (name, child) (Defns accTerms accTypes) =
          let Defns childTerms childTypes = go (name : prefix) child
           in -- These unions are safe because the input nametree had unconflicted names
              Defns (BiMultimap.unsafeUnion accTerms childTerms) (BiMultimap.unsafeUnion accTypes childTypes)

        defns1 :: Defns (BiMultimap terms Name) (BiMultimap types Name)
        defns1 =
          Defns
            { terms =
                BiMultimap.fromRange
                  ( Map.mapKeysMonotonic
                      (\name -> Name.fromReverseSegments (name :| prefix))
                      (defns0 ^. #terms)
                  ),
              types = BiMultimap.fromRange (Map.mapKeysMonotonic Name.fromSegment (defns0 ^. #types))
            }

unflattenNametree ::
  forall terms types.
  Ord terms =>
  Ord types =>
  Defns (BiMultimap terms Name) (BiMultimap types Name) ->
  Nametree (Defns (Map NameSegment terms) (Map NameSegment types))
unflattenNametree defns0 =
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
        Nametree (Defns (Map NameSegment terms) (Map NameSegment types))
      unflatten a b =
        let (curr, children) = unflattenLevel a b
            finalChildren = fmap (uncurry unflatten) children
         in Nametree curr finalChildren
   in unflatten inputTerms inputTypes

-- | Definitions (terms and types) in a namespace.
data Defns terms types = Defns
  { terms :: !terms,
    types :: !types
  }
  deriving stock (Generic, Show)
  deriving (Semigroup) via GenericSemigroupMonoid (Defns terms types)
