{-# LANGUAGE RecordWildCards #-}

module Unison.NamesWithHistory where

import Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.ConstructorType as CT
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names (..))
import qualified Unison.Names as Names
import Unison.Prelude
import Unison.Reference as Reference
import Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.Util.List as List
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R

-- | NamesWithHistory contains two sets of 'Names',
-- One represents names which are currently assigned,
-- the other represents names which no longer apply, perhaps they've been deleted, or the term
-- was updated and the name points elsewhere now.
data NamesWithHistory = NamesWithHistory
  { -- | currentNames represent references which are named in the current version of the namespace.
    currentNames :: Names.Names,
    -- | oldNames represent things which no longer have names in the current version of the
    -- codebase, but which may have previously had names. This may allow us to show more helpful
    -- context to users rather than just a hash.
    oldNames :: Names.Names
  }

instance Semigroup NamesWithHistory where
  NamesWithHistory cur1 old1 <> NamesWithHistory cur2 old2 =
    NamesWithHistory (cur1 <> old1) (cur2 <> old2)

instance Monoid NamesWithHistory where
  mempty = NamesWithHistory mempty mempty

fromCurrentNames :: Names -> NamesWithHistory
fromCurrentNames n = NamesWithHistory {currentNames = n, oldNames = mempty}

filterTypes :: (Name -> Bool) -> Names -> Names
filterTypes = Names.filterTypes

-- Simple 2 way diff, has the property that:
--  addedNames (diff0 n1 n2) == removedNames (diff0 n2 n1)
--
-- `addedNames` are names in `n2` but not `n1`
-- `removedNames` are names in `n1` but not `n2`
diff :: Names -> Names -> Diff
diff n1 n2 = Diff n1 added removed
  where
    added =
      Names
        (terms n2 `R.difference` terms n1)
        (types n2 `R.difference` types n1)
    removed =
      Names
        (terms n1 `R.difference` terms n2)
        (types n1 `R.difference` types n2)

data Diff = Diff
  { originalNames :: Names,
    addedNames :: Names,
    removedNames :: Names
  }

-- Add `n1` to `currentNames`, shadowing anything with the same name and
-- moving shadowed definitions into `oldNames` so they can can still be
-- referenced hash qualified.
push :: Names -> NamesWithHistory -> NamesWithHistory
push n0 ns = NamesWithHistory (unionLeft0 n1 cur) (oldNames ns <> shadowed)
  where
    n1 = suffixify0 n0
    cur = currentNames ns
    shadowed = Names terms' types'
      where
        terms' = R.dom (terms n1) R.<| (terms cur `R.difference` terms n1)
        types' = R.dom (types n1) R.<| (types cur `R.difference` types n1)
    unionLeft0 :: Names -> Names -> Names
    unionLeft0 n1 n2 = Names terms' types'
      where
        terms' = terms n1 <> R.subtractDom (R.dom $ terms n1) (terms n2)
        types' = types n1 <> R.subtractDom (R.dom $ types n1) (types n2)
    -- For all names in `ns`, (ex: foo.bar.baz), generate the list of suffixes
    -- of that name [[foo.bar.baz], [bar.baz], [baz]]. Any suffix which uniquely
    -- refers to a single definition is added as an alias
    --
    -- If `Names` were more like a `[Names]`, then `push` could just cons
    -- onto the list and we could get rid of all this complex logic. The
    -- complexity here is that we have to "bake the shadowing" into a single
    -- Names, taking into account suffix-based name resolution.
    --
    -- We currently have `oldNames`, but that controls an unrelated axis, which
    -- is whether names are hash qualified or not.
    suffixify0 :: Names -> Names
    suffixify0 ns = ns <> suffixNs
      where
        suffixNs = Names (R.fromList uniqueTerms) (R.fromList uniqueTypes)
        terms' = List.multimap [(n, ref) | (n0, ref) <- R.toList (terms ns), n <- Name.suffixes n0]
        types' = List.multimap [(n, ref) | (n0, ref) <- R.toList (types ns), n <- Name.suffixes n0]
        uniqueTerms = [(n, ref) | (n, nubOrd -> [ref]) <- Map.toList terms']
        uniqueTypes = [(n, ref) | (n, nubOrd -> [ref]) <- Map.toList types']

-- if I push an existing name, the pushed reference should be the thing
-- if I push a different name for the same thing, i suppose they should coexist
-- thus, `unionLeftName`.
shadowing :: Names -> NamesWithHistory -> NamesWithHistory
shadowing prio (NamesWithHistory current old) =
  NamesWithHistory (prio `Names.unionLeft` current) (current <> old)

-- Find all types whose name has a suffix matching the provided `HashQualified`,
-- returning types with relative names if they exist, and otherwise
-- returning types with absolute names.
lookupRelativeHQType :: HashQualified Name -> NamesWithHistory -> Set Reference
lookupRelativeHQType hq ns@NamesWithHistory {..} =
  let rs = lookupHQType hq ns
      keep r = any (not . Name.isAbsolute) (R.lookupRan r (Names.types currentNames))
   in case Set.filter keep rs of
        rs'
          | Set.null rs' -> rs
          | otherwise -> rs'

lookupRelativeHQType' :: HQ'.HashQualified Name -> NamesWithHistory -> Set Reference
lookupRelativeHQType' =
  lookupRelativeHQType . HQ'.toHQ

-- | Find all types whose name has a suffix matching the provided 'HashQualified'.
lookupHQType :: HashQualified Name -> NamesWithHistory -> Set Reference
lookupHQType =
  lookupHQRef Names.types Reference.isPrefixOf

-- | Find all types whose name has a suffix matching the provided 'HashQualified''. See 'lookupHQType'.
lookupHQType' :: HQ'.HashQualified Name -> NamesWithHistory -> Set Reference
lookupHQType' =
  lookupHQType . HQ'.toHQ

hasTermNamed :: Name -> NamesWithHistory -> Bool
hasTermNamed n ns = not (Set.null $ lookupHQTerm (HQ.NameOnly n) ns)

hasTypeNamed :: Name -> NamesWithHistory -> Bool
hasTypeNamed n ns = not (Set.null $ lookupHQType (HQ.NameOnly n) ns)

-- Find all terms whose name has a suffix matching the provided `HashQualified`,
-- returning terms with relative names if they exist, and otherwise
-- returning terms with absolute names.
lookupRelativeHQTerm :: HashQualified Name -> NamesWithHistory -> Set Referent
lookupRelativeHQTerm hq ns@NamesWithHistory {..} =
  let rs = lookupHQTerm hq ns
      keep r = any (not . Name.isAbsolute) (R.lookupRan r (Names.terms currentNames))
   in case Set.filter keep rs of
        rs'
          | Set.null rs' -> rs
          | otherwise -> rs'

lookupRelativeHQTerm' :: HQ'.HashQualified Name -> NamesWithHistory -> Set Referent
lookupRelativeHQTerm' =
  lookupRelativeHQTerm . HQ'.toHQ

-- | Find all terms whose name has a suffix matching the provided 'HashQualified'.
--
-- If the hash-qualified name does not include a hash, then only current names are searched. Otherwise, old names are
-- searched, too, if searching current names produces no hits.
lookupHQTerm :: HashQualified Name -> NamesWithHistory -> Set Referent
lookupHQTerm =
  lookupHQRef Names.terms Referent.isPrefixOf

-- | Find all terms whose name has a suffix matching the provided 'HashQualified''. See 'lookupHQTerm'.
lookupHQTerm' :: HQ'.HashQualified Name -> NamesWithHistory -> Set Referent
lookupHQTerm' =
  lookupHQTerm . HQ'.toHQ

-- Helper that unifies looking up a set of references/referents by a hash-qualified suffix.
--
-- See 'lookupHQTerm', 'lookupHQType' for monomorphic versions.
lookupHQRef ::
  forall r.
  Ord r =>
  -- | A projection of types or terms from a Names.
  (Names -> Relation Name r) ->
  -- | isPrefixOf, for references or referents
  (ShortHash -> r -> Bool) ->
  -- | The name to look up
  HashQualified Name ->
  NamesWithHistory ->
  Set r
lookupHQRef which isPrefixOf hq NamesWithHistory {currentNames, oldNames} =
  case hq of
    HQ.NameOnly n -> Name.searchByRankedSuffix n currentRefs
    HQ.HashQualified n sh -> matches currentRefs `orIfEmpty` matches oldRefs
      where
        matches :: Relation Name r -> Set r
        matches ns =
          Set.filter (isPrefixOf sh) (Name.searchByRankedSuffix n ns)
    HQ.HashOnly sh -> matches currentRefs `orIfEmpty` matches oldRefs
      where
        matches :: Relation Name r -> Set r
        matches ns =
          Set.filter (isPrefixOf sh) (R.ran ns)
  where
    currentRefs = which currentNames
    oldRefs = which oldNames

    -- (xs `orIfEmpty` ys) returns xs if it's non-empty, otherwise ys
    orIfEmpty :: Set a -> Set a -> Set a
    orIfEmpty xs ys =
      if Set.null xs then ys else xs

-- If `r` is in "current" names, look up each of its names, and hash-qualify
-- them if they are conflicted names.  If `r` isn't in "current" names, look up
-- each of its "old" names and hash-qualify them.
typeName :: Int -> Reference -> NamesWithHistory -> Set (HQ'.HashQualified Name)
typeName length r NamesWithHistory {..} =
  if R.memberRan r . Names.types $ currentNames
    then
      Set.map
        (\n -> if isConflicted n then hq n else HQ'.fromName n)
        (R.lookupRan r . Names.types $ currentNames)
    else Set.map hq (R.lookupRan r . Names.types $ oldNames)
  where
    hq n = HQ'.take length (HQ'.fromNamedReference n r)
    isConflicted n = R.manyDom n (Names.types currentNames)

-- List of names for a referent, longer names (by number of segments) first.
termNamesByLength :: Int -> Referent -> NamesWithHistory -> [HQ'.HashQualified Name]
termNamesByLength length r ns =
  sortOn len (toList $ termName length r ns)
  where
    len (HQ'.NameOnly n) = Name.countSegments n
    len (HQ'.HashQualified n _) = Name.countSegments n

-- The longest term name (by segment count) for a `Referent`.
longestTermName :: Int -> Referent -> NamesWithHistory -> HQ.HashQualified Name
longestTermName length r ns =
  case reverse (termNamesByLength length r ns) of
    [] -> HQ.take length (HQ.fromReferent r)
    (h : _) -> Name.convert h

termName :: Int -> Referent -> NamesWithHistory -> Set (HQ'.HashQualified Name)
termName length r NamesWithHistory {..} =
  if R.memberRan r . Names.terms $ currentNames
    then
      Set.map
        (\n -> if isConflicted n then hq n else HQ'.fromName n)
        (R.lookupRan r . Names.terms $ currentNames)
    else Set.map hq (R.lookupRan r . Names.terms $ oldNames)
  where
    hq n = HQ'.take length (HQ'.fromNamedReferent n r)
    isConflicted n = R.manyDom n (Names.terms currentNames)

-- Set HashQualified -> Branch m -> Action' m v Names
-- Set HashQualified -> Branch m -> Free (Command m i v) Names
-- Set HashQualified -> Branch m -> Command m i v Names
-- populate historical names
lookupHQPattern ::
  HQ.HashQualified Name ->
  CT.ConstructorType ->
  NamesWithHistory ->
  Set ConstructorReference
lookupHQPattern hq ctt names =
  Set.fromList
    [ r
      | Referent.Con r ct <- toList $ lookupHQTerm hq names,
        ct == ctt
    ]

-- | Given a mapping from name to qualified name, update a `Names`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `Names`,
-- the alias `Some` will map to that same constructor and shadow
-- anything else that is currently called `Some`.
--
-- Only affects `currentNames`.
importing :: [(Name, Name)] -> NamesWithHistory -> NamesWithHistory
importing shortToLongName ns =
  ns {currentNames = Names.importing shortToLongName (currentNames ns)}
