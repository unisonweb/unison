module Unison.Namer
  ( Namer,
    nameTerm,
    nameType,
    mapNamer,
    makeNamer,
    makeHqNamer,
  )
where

import Data.Set qualified as Set
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Names3 (Names3 (..))
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Util.Defns (Defns (..))

-- | A "namer" associates a set of names with a referent / type reference.
newtype Namer n
  = Namer (Defns (Referent -> Set n) (TypeReference -> Set n))

nameTerm :: Namer n -> Referent -> Set n
nameTerm (Namer namer) =
  namer.terms

nameType :: Namer n -> TypeReference -> Set n
nameType (Namer namer) =
  namer.types

mapNamer :: (Ord b) => (a -> b) -> Namer a -> Namer b
mapNamer f (Namer namer) =
  Namer
    Defns
      { terms = Set.map f . namer.terms,
        types = Set.map f . namer.types
      }

-- | Make a "namer" out of a collection of names, ignoring conflicted names. That is, if references #foo and #bar are
-- both associated with name "baz", then the returned namer maps #foo too "baz" (not "baz"#foo) and #bar to "baz" (not
-- "baz"#bar).
makeNamer :: Names3 -> Namer Name
makeNamer names =
  Namer
    Defns
      { terms =
          \ref ->
            Set.unions
              [ Names.namesForReferent names.local ref,
                Names.namesForReferent names.directDeps ref,
                Names.namesForReferent names.indirectDeps ref
              ],
        types =
          \ref ->
            Set.unions
              [ Names.namesForReference names.local ref,
                Names.namesForReference names.directDeps ref,
                Names.namesForReference names.indirectDeps ref
              ]
      }

-- | Make a "namer" out of a collection of names, respecting conflicted names. That is, if references #foo and #bar are
-- both associated with name "baz", then the returned namer maps #foo too "baz"#foo and #bar to "baz"#bar, but otherwise
-- if a reference #qux has a single name "qux", then the returned namer maps #qux to "qux" (not "qux"#qux).
makeHqNamer :: Int -> Names3 -> Namer (HQ'.HashQualified Name)
makeHqNamer hashLen names =
  -- Hash-qualifying separately is fine because names.local/names.directDeps/names.indirectDeps are disjoin
  Namer
    Defns
      { terms =
          \ref ->
            -- Hash-qualifying separately is fine because names.local/names.directDeps/names.indirectDeps are disjoin
            Set.unions
              [ NamesWithHistory.termName hashLen ref names.local,
                NamesWithHistory.termName hashLen ref names.directDeps,
                NamesWithHistory.termName hashLen ref names.indirectDeps
              ],
        types =
          \ref ->
            Set.unions
              [ NamesWithHistory.typeName hashLen ref names.local,
                NamesWithHistory.typeName hashLen ref names.directDeps,
                NamesWithHistory.typeName hashLen ref names.indirectDeps
              ]
      }
