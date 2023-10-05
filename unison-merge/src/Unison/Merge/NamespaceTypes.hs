module Unison.Merge.NamespaceTypes
  ( Defns (..),
    DefnsA,
    DefnsB,
    NamespaceTree,
    flattenNamespaceTree,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.List.NonEmpty (pattern (:|))
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

-- haha rename or delete
type DefnsA terms types =
  Defns (BiMultimap terms Name) (BiMultimap types Name)

-- haha rename or delete
type DefnsB terms types =
  Defns (Map Name terms) (Map Name types)

-- | A namespace tree has values, and a collection of children namespace trees keyed by name segment.
type NamespaceTree a =
  Cofree (Map NameSegment) a

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
