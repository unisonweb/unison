{-# LANGUAGE RecordWildCards #-}

module Unison.NamesWithHistory where

import Unison.Prelude

import Control.Lens (view, _4)
import Data.List.Extra (nubOrd, sort)
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.Reference as Reference
import Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import Unison.Util.Relation (Relation)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Unison.Name as Name
import qualified Unison.Names as Names
import qualified Unison.Util.List as List
import qualified Unison.Util.Relation as R
import qualified Unison.ConstructorType as CT
import Unison.Names (UnqualifiedNames, pattern UnqualifiedNames)

data NamesWithHistory = NamesWithHistory
  { -- | currentNames represent references which are named in the current version of the namespace.
    currentNames :: Names.UnqualifiedNames,
    -- | oldNames represent things which no longer have names in the current version of the
    -- codebase, but which may have previously had names. This may allow us to show more helpful
    -- context to users rather than just a hash.
    oldNames :: Names.UnqualifiedNames
  }
  deriving (Show)

filterTypes :: (Name -> Bool) -> UnqualifiedNames -> UnqualifiedNames
filterTypes = Names.filterTypes

-- Simple 2 way diff, has the property that:
--  addedNames (diff0 n1 n2) == removedNames (diff0 n2 n1)
--
-- `addedNames` are names in `n2` but not `n1`
-- `removedNames` are names in `n1` but not `n2`
diff0 :: UnqualifiedNames -> UnqualifiedNames -> Diff
diff0 n1 n2 = Diff n1 added removed where
  added = UnqualifiedNames (terms0 n2 `R.difference` terms0 n1)
                 (types0 n2 `R.difference` types0 n1)
  removed = UnqualifiedNames (terms0 n1 `R.difference` terms0 n2)
                   (types0 n1 `R.difference` types0 n2)

data Diff =
  Diff { originalNames :: UnqualifiedNames
       , addedNames    :: UnqualifiedNames
       , removedNames  :: UnqualifiedNames
       } deriving Show

isEmptyDiff :: Diff -> Bool
isEmptyDiff d = isEmpty0 (addedNames d) && isEmpty0 (removedNames d)

isEmpty0 :: UnqualifiedNames -> Bool
isEmpty0 n = R.null (terms0 n) && R.null (types0 n)

-- Add `n1` to `currentNames`, shadowing anything with the same name and
-- moving shadowed definitions into `oldNames` so they can can still be
-- referenced hash qualified.
push :: UnqualifiedNames -> NamesWithHistory -> NamesWithHistory
push n0 ns = NamesWithHistory (unionLeft0 n1 cur) (oldNames ns <> shadowed) where
  n1 = suffixify0 n0
  cur = currentNames ns
  shadowed = names0 terms' types' where
    terms' = R.dom (terms0 n1) R.<| (terms0 cur `R.difference` terms0 n1)
    types' = R.dom (types0 n1) R.<| (types0 cur `R.difference` types0 n1)
  unionLeft0 :: UnqualifiedNames -> UnqualifiedNames -> UnqualifiedNames
  unionLeft0 n1 n2 = names0 terms' types' where
    terms' = terms0 n1 <> R.subtractDom (R.dom $ terms0 n1) (terms0 n2)
    types' = types0 n1 <> R.subtractDom (R.dom $ types0 n1) (types0 n2)
  -- For all names in `ns`, (ex: foo.bar.baz), generate the list of suffixes
  -- of that name [[foo.bar.baz], [bar.baz], [baz]]. Any suffix which uniquely
  -- refers to a single definition is added as an alias
  --
  -- If `Names` were more like a `[UnqualifiedNames]`, then `push` could just cons
  -- onto the list and we could get rid of all this complex logic. The
  -- complexity here is that we have to "bake the shadowing" into a single
  -- UnqualifiedNames, taking into account suffix-based name resolution.
  --
  -- We currently have `oldNames`, but that controls an unrelated axis, which
  -- is whether names are hash qualified or not.
  suffixify0 :: UnqualifiedNames -> UnqualifiedNames
  suffixify0 ns = ns <> suffixNs
    where
    suffixNs = names0 (R.fromList uniqueTerms) (R.fromList uniqueTypes)
    terms = List.multimap [ (n,ref) | (n0,ref) <- R.toList (terms0 ns), n <- Name.suffixes n0 ]
    types = List.multimap [ (n,ref) | (n0,ref) <- R.toList (types0 ns), n <- Name.suffixes n0 ]
    uniqueTerms = [ (n,ref) | (n, nubOrd -> [ref]) <- Map.toList terms ]
    uniqueTypes = [ (n,ref) | (n, nubOrd -> [ref]) <- Map.toList types ]

unionLeft0 :: UnqualifiedNames -> UnqualifiedNames -> UnqualifiedNames
unionLeft0 = Names.unionLeft

unionLeftName0 :: UnqualifiedNames -> UnqualifiedNames -> UnqualifiedNames
unionLeftName0 = Names.unionLeftName

map0 :: (Name -> Name) -> UnqualifiedNames -> UnqualifiedNames
map0 f (Names.Names terms types) = Names.Names terms' types' where
  terms' = R.mapDom f terms
  types' = R.mapDom f types

names0 :: Relation Name Referent -> Relation Name Reference -> UnqualifiedNames
names0 = Names.Names

types0 :: UnqualifiedNames -> Relation Name Reference
types0 = Names.types

terms0 :: UnqualifiedNames -> Relation Name Referent
terms0 = Names.terms

-- if I push an existing name, the pushed reference should be the thing
-- if I push a different name for the same thing, i suppose they should coexist
-- thus, `unionLeftName0`.
shadowing :: UnqualifiedNames -> NamesWithHistory -> NamesWithHistory
shadowing prio (NamesWithHistory current old) =
  NamesWithHistory (prio `unionLeftName0` current) (current <> old)

makeAbsolute0 :: UnqualifiedNames -> UnqualifiedNames
makeAbsolute0 = map0 Name.makeAbsolute

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
  -- | A projection of types or terms from a UnqualifiedNames.
  (UnqualifiedNames -> Relation Name r) ->
  -- | isPrefixOf, for references or referents
  (ShortHash -> r -> Bool) ->
  -- | The name to look up
  HashQualified Name ->
  NamesWithHistory ->
  Set r
lookupHQRef which isPrefixOf hq NamesWithHistory {currentNames, oldNames} =
  case hq of
    HQ.NameOnly n -> Name.searchBySuffix n currentRefs
    HQ.HashQualified n sh -> matches currentRefs `orIfEmpty` matches oldRefs
      where
        matches :: Relation Name r -> Set r
        matches ns =
          Set.filter (isPrefixOf sh) (Name.searchBySuffix n ns)
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
typeName length r NamesWithHistory{..} =
  if R.memberRan r . Names.types $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ'.fromName n)
               (R.lookupRan r . Names.types $ currentNames)
  else Set.map hq (R.lookupRan r . Names.types $ oldNames)
  where hq n = HQ'.take length (HQ'.fromNamedReference n r)
        isConflicted n = R.manyDom n (Names.types currentNames)

-- List of names for a referent, longer names (by number of segments) first.
termNamesByLength :: Int -> Referent -> NamesWithHistory -> [HQ'.HashQualified Name]
termNamesByLength length r ns =
  sortOn len (toList $ termName length r ns)
  where len (HQ'.NameOnly n) = Name.countSegments n
        len (HQ'.HashQualified n _) = Name.countSegments n

-- The longest term name (by segment count) for a `Referent`.
longestTermName :: Int -> Referent -> NamesWithHistory -> HQ.HashQualified Name
longestTermName length r ns =
  case reverse (termNamesByLength length r ns) of
    [] -> HQ.take length (HQ.fromReferent r)
    (h : _) -> Name.convert h

termName :: Int -> Referent -> NamesWithHistory -> Set (HQ'.HashQualified Name)
termName length r NamesWithHistory{..} =
  if R.memberRan r . Names.terms $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ'.fromName n)
               (R.lookupRan r . Names.terms $ currentNames)
  else Set.map hq (R.lookupRan r . Names.terms $ oldNames)
  where hq n = HQ'.take length (HQ'.fromNamedReferent n r)
        isConflicted n = R.manyDom n (Names.terms currentNames)

suffixedTypeName :: Int -> Reference -> NamesWithHistory -> [HQ.HashQualified Name]
suffixedTermName :: Int -> Referent -> NamesWithHistory -> [HQ.HashQualified Name]
(suffixedTermName,suffixedTypeName) =
  ( suffixedName termName (Names.terms . currentNames) HQ'.fromNamedReferent
  , suffixedName typeName (Names.types . currentNames) HQ'.fromNamedReference )
  where
  suffixedName fallback getRel hq' length r ns@(getRel -> rel) =
    if R.memberRan r rel
    then go $ toList (R.lookupRan r rel)
    else sort $ map Name.convert $ Set.toList (fallback length r ns)
    where
      -- Orders names, using these criteria, in this order:
      -- 1. NameOnly comes before HashQualified,
      -- 2. Shorter names (in terms of segment count) come before longer ones
      -- 3. If same on attributes 1 and 2, compare alphabetically
      go :: [Name] -> [HashQualified Name]
      go fqns = map (view _4) . sort $ map f fqns where
        f fqn = let
          n' = Name.shortestUniqueSuffix fqn r rel
          isHQ'd = R.manyDom fqn rel -- it is conflicted
          hq n = HQ'.take length (hq' n r)
          hqn = Name.convert $ if isHQ'd then hq n' else HQ'.fromName n'
          in (isHQ'd, Name.countSegments fqn, Name.isAbsolute n', hqn)

-- Set HashQualified -> Branch m -> Action' m v Names
-- Set HashQualified -> Branch m -> Free (Command m i v) Names
-- Set HashQualified -> Branch m -> Command m i v Names
-- populate historical names
lookupHQPattern
  :: HQ.HashQualified Name
  -> CT.ConstructorType
  -> NamesWithHistory
  -> Set (Reference, Int)
lookupHQPattern hq ctt names = Set.fromList
  [ (r, cid)
    | Referent.Con r cid ct <- toList $ lookupHQTerm hq names
    , ct == ctt
    ]

-- Finds all the constructors for the given type in the `UnqualifiedNames`
constructorsForType0 :: Reference -> UnqualifiedNames -> [(Name,Referent)]
constructorsForType0 r ns = let
  -- rather than searching all of names, we use the known possible forms
  -- that the constructors can take
  possibleDatas =   [ Referent.Con r cid CT.Data | cid <- [0..] ]
  possibleEffects = [ Referent.Con r cid CT.Effect | cid <- [0..] ]
  trim [] = []
  trim (h:t) = case R.lookupRan h (terms0 ns) of
    s | Set.null s -> []
      | otherwise  -> [ (n,h) | n <- toList s ] ++ trim t
  in trim possibleEffects ++ trim possibleDatas

-- Given a mapping from name to qualified name, update a `Names`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `Names`,
-- the alias `Some` will map to that same constructor and shadow
-- anything else that is currently called `Some`.
--
-- Only affects `currentNames`.
importing :: [(Name, Name)] -> NamesWithHistory -> NamesWithHistory
importing shortToLongName ns =
  ns { currentNames = importing0 shortToLongName (currentNames ns) }

importing0 :: [(Name, Name)] -> UnqualifiedNames -> UnqualifiedNames
importing0 shortToLongName ns =
  Names.Names
    (foldl' go (terms0 ns) shortToLongName)
    (foldl' go (types0 ns) shortToLongName)
  where
  go :: (Ord r) => Relation Name r -> (Name, Name) -> Relation Name r
  go m (shortname, qname) = case Name.searchBySuffix qname m of
    s | Set.null s -> m
      | otherwise -> R.insertManyRan shortname s (R.deleteDom shortname m)

-- Converts a wildcard import into a list of explicit imports, of the form
-- [(suffix, full)]. Example: if `io` contains two functions, `foo` and
-- `bar`, then `expandWildcardImport io` will produce
-- `[(foo, io.foo), (bar, io.bar)]`.
expandWildcardImport :: Name -> UnqualifiedNames -> [(Name,Name)]
expandWildcardImport prefix ns =
  [ (suffix, full) | Just (suffix,full) <- go <$> R.toList (terms0 ns) ] <>
  [ (suffix, full) | Just (suffix,full) <- go <$> R.toList (types0 ns) ]
  where
  go (full, _) = do
    -- running example:
    --   prefix = Int
    --   full = builtin.Int.negate
    rem <- Name.suffixFrom prefix full
    -- rem = Int.negate
    suffix <- Name.stripNamePrefix prefix rem
    -- suffix = negate
    pure (suffix, full)

-- Deletes from the `n0 : UnqualifiedNames` any definitions whose names
-- are in `ns`. Does so using logarithmic time lookups,
-- traversing only `ns`.
--
-- See usage in `FileParser` for handling precendence of symbol
-- resolution where local names are preferred to codebase names.
shadowTerms0 :: [Name] -> UnqualifiedNames -> UnqualifiedNames
shadowTerms0 ns n0 = names0 terms' (types0 n0)
  where
  terms' = foldl' go (terms0 n0) ns
  go ts name = R.deleteDom name ts
