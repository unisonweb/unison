{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Names3 where

import Unison.Prelude

import Control.Lens (view, _4)
import Data.List.Extra (nubOrd, sort)
import Unison.HashQualified (HashQualified)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.Reference as Reference
import Unison.Referent as Referent
import Unison.Util.Relation (Relation)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Unison.Name as Name
import qualified Unison.Names2
import qualified Unison.Names2 as Names
import qualified Unison.Util.List as List
import qualified Unison.Util.Relation as R
import qualified Unison.ConstructorType as CT

data Names = Names { currentNames :: Names0, oldNames :: Names0 } deriving Show

type Names0 = Unison.Names2.Names0
pattern Names0 :: Relation n Referent -> Relation n Reference -> Names.Names' n
pattern Names0 terms types = Unison.Names2.Names terms types

filterTypes :: (Name -> Bool) -> Names0 -> Names0
filterTypes = Unison.Names2.filterTypes

-- Simple 2 way diff, has the property that:
--  addedNames (diff0 n1 n2) == removedNames (diff0 n2 n1)
--
-- `addedNames` are names in `n2` but not `n1`
-- `removedNames` are names in `n1` but not `n2`
diff0 :: Names0 -> Names0 -> Diff
diff0 n1 n2 = Diff n1 added removed where
  added = Names0 (terms0 n2 `R.difference` terms0 n1)
                 (types0 n2 `R.difference` types0 n1)
  removed = Names0 (terms0 n1 `R.difference` terms0 n2)
                   (types0 n1 `R.difference` types0 n2)

data Diff =
  Diff { originalNames :: Names0
       , addedNames    :: Names0
       , removedNames  :: Names0
       } deriving Show

isEmptyDiff :: Diff -> Bool
isEmptyDiff d = isEmpty0 (addedNames d) && isEmpty0 (removedNames d)

isEmpty0 :: Names0 -> Bool
isEmpty0 n = R.null (terms0 n) && R.null (types0 n)

-- Add `n1` to `currentNames`, shadowing anything with the same name and
-- moving shadowed definitions into `oldNames` so they can can still be
-- referenced hash qualified.
push :: Names0 -> Names -> Names
push n0 ns = Names (unionLeft0 n1 cur) (oldNames ns <> shadowed) where
  n1 = suffixify0 n0
  cur = currentNames ns
  shadowed = names0 terms' types' where
    terms' = R.dom (terms0 n1) R.<| (terms0 cur `R.difference` terms0 n1)
    types' = R.dom (types0 n1) R.<| (types0 cur `R.difference` types0 n1)
  unionLeft0 :: Names0 -> Names0 -> Names0
  unionLeft0 n1 n2 = names0 terms' types' where
    terms' = terms0 n1 <> R.subtractDom (R.dom $ terms0 n1) (terms0 n2)
    types' = types0 n1 <> R.subtractDom (R.dom $ types0 n1) (types0 n2)
  -- For all names in `ns`, (ex: foo.bar.baz), generate the list of suffixes
  -- of that name [[foo.bar.baz], [bar.baz], [baz]]. Any suffix which uniquely
  -- refers to a single definition is added as an alias
  --
  -- If `Names` were more like a `[Names0]`, then `push` could just cons
  -- onto the list and we could get rid of all this complex logic. The
  -- complexity here is that we have to "bake the shadowing" into a single
  -- Names0, taking into account suffix-based name resolution.
  --
  -- We currently have `oldNames`, but that controls an unrelated axis, which
  -- is whether names are hash qualified or not.
  suffixify0 :: Names0 -> Names0
  suffixify0 ns = ns <> suffixNs
    where
    suffixNs = names0 (R.fromList uniqueTerms) (R.fromList uniqueTypes)
    terms = List.multimap [ (n,ref) | (n0,ref) <- R.toList (terms0 ns), n <- Name.suffixes n0 ]
    types = List.multimap [ (n,ref) | (n0,ref) <- R.toList (types0 ns), n <- Name.suffixes n0 ]
    uniqueTerms = [ (n,ref) | (n, nubOrd -> [ref]) <- Map.toList terms ]
    uniqueTypes = [ (n,ref) | (n, nubOrd -> [ref]) <- Map.toList types ]

unionLeft0 :: Names0 -> Names0 -> Names0
unionLeft0 = Unison.Names2.unionLeft

unionLeftName0 :: Names0 -> Names0 -> Names0
unionLeftName0 = Unison.Names2.unionLeftName

map0 :: (Name -> Name) -> Names0 -> Names0
map0 f (Names.Names terms types) = Names.Names terms' types' where
  terms' = R.mapDom f terms
  types' = R.mapDom f types

names0 :: Relation Name Referent -> Relation Name Reference -> Names0
names0 = Unison.Names2.Names

types0 :: Names0 -> Relation Name Reference
types0 = Names.types

terms0 :: Names0 -> Relation Name Referent
terms0 = Names.terms

-- if I push an existing name, the pushed reference should be the thing
-- if I push a different name for the same thing, i suppose they should coexist
-- thus, `unionLeftName0`.
shadowing :: Names0 -> Names -> Names
shadowing prio (Names current old) =
  Names (prio `unionLeftName0` current) (current <> old)

makeAbsolute0:: Names0 -> Names0
makeAbsolute0 = map0 Name.makeAbsolute

-- Find all types whose name has a suffix matching the provided `HashQualified`,
-- returning types with relative names if they exist, and otherwise
-- returning types with absolute names.
lookupRelativeHQType :: HashQualified Name -> Names -> Set Reference
lookupRelativeHQType hq ns@Names{..} = let
  rs = lookupHQType hq ns
  keep r = any (not . Name.isAbsolute) (R.lookupRan r (Names.types currentNames))
  in case Set.filter keep rs of
       rs' | Set.null rs' -> rs
           | otherwise    -> rs'

-- Find all types whose name has a suffix matching the provided `HashQualified`.
lookupHQType :: HashQualified Name -> Names -> Set Reference
lookupHQType hq Names{..} = case hq of
  HQ.NameOnly n -> Name.searchBySuffix n (Names.types currentNames)
  HQ.HashQualified n sh -> case matches sh (Names.types currentNames) of
    s | (not . null) s -> s
      | otherwise -> matches sh (Names.types oldNames)
    where
    matches sh ns =
      Set.filter (Reference.isPrefixOf sh) (Name.searchBySuffix n ns)
  HQ.HashOnly sh -> case matches sh currentNames of
    s | (not . null) s -> s
      | otherwise -> matches sh oldNames
    where
    matches sh ns = Set.filter (Reference.isPrefixOf sh) (R.ran $ Names.types ns)

hasTermNamed :: Name -> Names -> Bool
hasTermNamed n ns = not (Set.null $ lookupHQTerm (HQ.NameOnly n) ns)

hasTypeNamed :: Name -> Names -> Bool
hasTypeNamed n ns = not (Set.null $ lookupHQType (HQ.NameOnly n) ns)

-- Find all terms whose name has a suffix matching the provided `HashQualified`,
-- returning terms with relative names if they exist, and otherwise
-- returning terms with absolute names.
lookupRelativeHQTerm :: HashQualified Name -> Names -> Set Referent
lookupRelativeHQTerm hq ns@Names{..} = let
  rs = lookupHQTerm hq ns
  keep r = any (not . Name.isAbsolute) (R.lookupRan r (Names.terms currentNames))
  in case Set.filter keep rs of
       rs' | Set.null rs' -> rs
           | otherwise    -> rs'

-- Find all terms whose name has a suffix matching the provided `HashQualified`.
lookupHQTerm :: HashQualified Name -> Names -> Set Referent
lookupHQTerm hq Names{..} = case hq of
  HQ.NameOnly n -> Name.searchBySuffix n (Names.terms currentNames)
  HQ.HashQualified n sh -> case matches sh (Names.terms currentNames) of
    s | (not . null) s -> s
      | otherwise -> matches sh (Names.terms oldNames)
    where
    matches sh ns =
      Set.filter (Referent.isPrefixOf sh) (Name.searchBySuffix n ns)
  HQ.HashOnly sh -> case matches sh currentNames of
    s | (not . null) s -> s
      | otherwise -> matches sh oldNames
    where
    matches sh ns = Set.filter (Referent.isPrefixOf sh) (R.ran $ Names.terms ns)

-- If `r` is in "current" names, look up each of its names, and hash-qualify
-- them if they are conflicted names.  If `r` isn't in "current" names, look up
-- each of its "old" names and hash-qualify them.
typeName :: Int -> Reference -> Names -> Set (HQ'.HashQualified Name)
typeName length r Names{..} =
  if R.memberRan r . Names.types $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ'.fromName n)
               (R.lookupRan r . Names.types $ currentNames)
  else Set.map hq (R.lookupRan r . Names.types $ oldNames)
  where hq n = HQ'.take length (HQ'.fromNamedReference n r)
        isConflicted n = R.manyDom n (Names.types currentNames)

-- List of names for a referent, longer names (by number of segments) first.
termNamesByLength :: Int -> Referent -> Names -> [HQ'.HashQualified Name]
termNamesByLength length r ns =
  sortOn len (toList $ termName length r ns)
  where len (HQ'.NameOnly n) = Name.countSegments n
        len (HQ'.HashQualified n _) = Name.countSegments n

-- The longest term name (by segment count) for a `Referent`.
longestTermName :: Int -> Referent -> Names -> HQ.HashQualified Name
longestTermName length r ns =
  case reverse (termNamesByLength length r ns) of
    [] -> HQ.take length (HQ.fromReferent r)
    (h : _) -> Name.convert h

termName :: Int -> Referent -> Names -> Set (HQ'.HashQualified Name)
termName length r Names{..} =
  if R.memberRan r . Names.terms $ currentNames
  then Set.map (\n -> if isConflicted n then hq n else HQ'.fromName n)
               (R.lookupRan r . Names.terms $ currentNames)
  else Set.map hq (R.lookupRan r . Names.terms $ oldNames)
  where hq n = HQ'.take length (HQ'.fromNamedReferent n r)
        isConflicted n = R.manyDom n (Names.terms currentNames)

suffixedTypeName :: Int -> Reference -> Names -> [HQ.HashQualified Name]
suffixedTermName :: Int -> Referent -> Names -> [HQ.HashQualified Name]
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
  -> Names
  -> Set (Reference, Int)
lookupHQPattern hq ctt names = Set.fromList
  [ (r, cid)
    | Referent.Con r cid ct <- toList $ lookupHQTerm hq names
    , ct == ctt
    ]

-- Finds all the constructors for the given type in the `Names0`
constructorsForType0 :: Reference -> Names0 -> [(Name,Referent)]
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
importing :: [(Name, Name)] -> Names -> Names
importing shortToLongName ns =
  ns { currentNames = importing0 shortToLongName (currentNames ns) }

importing0 :: [(Name, Name)] -> Names0 -> Names0
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
expandWildcardImport :: Name -> Names0 -> [(Name,Name)]
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

-- Deletes from the `n0 : Names0` any definitions whose names
-- are in `ns`. Does so using logarithmic time lookups,
-- traversing only `ns`.
--
-- See usage in `FileParser` for handling precendence of symbol
-- resolution where local names are preferred to codebase names.
shadowSuffixedTerms0 :: [Name] -> Names0 -> Names0
shadowSuffixedTerms0 ns n0 = names0 terms' (types0 n0)
  where
  terms' = foldl' go (terms0 n0) ns
  go ts name = R.deleteDom name ts
