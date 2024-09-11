{-# LANGUAGE RecordWildCards #-}

-- FIXME this module used to define a NamesWithHistory type, but does no longer. This API should be moved over to
-- Unison.Names

module Unison.NamesWithHistory
  ( diff,
    lookupHQType,
    lookupHQTerm,
    lookupRelativeHQType,
    lookupRelativeHQType',
    lookupRelativeHQTerm,
    lookupRelativeHQTerm',
    hasTypeNamed,
    typeName,
    termNamesByLength,
    longestTermName,
    termName,
    lookupHQPattern,
    Diff (..),
    SearchType (..),
  )
where

import Data.Set qualified as Set
import Unison.ConstructorReference (ConstructorReference)
import Unison.ConstructorType qualified as CT
import Unison.HashQualified (HashQualified)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Reference as Reference
import Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as R
import Unison.Util.Relation qualified as Relation

-- | Whether to search for exact matches or to find definitions by a suffix of their name.
data SearchType
  = IncludeSuffixes
  | ExactName
  deriving (Eq, Ord, Show)

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
  deriving (Show)

-- Find all types whose name has a suffix matching the provided `HashQualified`,
-- returning types with relative names if they exist, and otherwise
-- returning types with absolute names.
lookupRelativeHQType :: SearchType -> HashQualified Name -> Names -> Set TypeReference
lookupRelativeHQType searchType hq ns =
  let rs = lookupHQType searchType hq ns
      keep r = any (not . Name.isAbsolute) (R.lookupRan r (Names.types ns))
   in case Set.filter keep rs of
        rs'
          | Set.null rs' -> rs
          | otherwise -> rs'

lookupRelativeHQType' :: SearchType -> HQ'.HashQualified Name -> Names -> Set TypeReference
lookupRelativeHQType' searchType =
  lookupRelativeHQType searchType . HQ'.toHQ

-- | Find all types whose name has a suffix matching the provided 'HashQualified'.
lookupHQType :: SearchType -> HashQualified Name -> Names -> Set TypeReference
lookupHQType searchType =
  lookupHQRef searchType Names.types Reference.isPrefixOf

hasTypeNamed :: SearchType -> Name -> Names -> Bool
hasTypeNamed searchType n ns = not (Set.null $ lookupHQType searchType (HQ.NameOnly n) ns)

-- Find all terms whose name has a suffix matching the provided `HashQualified`,
-- returning terms with relative names if they exist, and otherwise
-- returning terms with absolute names.
lookupRelativeHQTerm :: SearchType -> HashQualified Name -> Names -> Set Referent
lookupRelativeHQTerm searchType hq ns =
  let rs = lookupHQTerm searchType hq ns
      keep r = any (not . Name.isAbsolute) (R.lookupRan r (Names.terms ns))
   in case Set.filter keep rs of
        rs'
          | Set.null rs' -> rs
          | otherwise -> rs'

lookupRelativeHQTerm' :: SearchType -> HQ'.HashQualified Name -> Names -> Set Referent
lookupRelativeHQTerm' searchType =
  lookupRelativeHQTerm searchType . HQ'.toHQ

-- | Find all terms whose name has a suffix matching the provided 'HashQualified'.
lookupHQTerm :: SearchType -> HashQualified Name -> Names -> Set Referent
lookupHQTerm searchType =
  lookupHQRef searchType Names.terms Referent.isPrefixOf

-- Helper that unifies looking up a set of references/referents by a hash-qualified suffix.
--
-- See 'lookupHQTerm', 'lookupHQType' for monomorphic versions.
lookupHQRef ::
  forall r.
  (Ord r) =>
  SearchType ->
  -- | A projection of types or terms from a Names.
  (Names -> Relation Name r) ->
  -- | isPrefixOf, for references or referents
  (ShortHash -> r -> Bool) ->
  -- | The name to look up
  HashQualified Name ->
  Names ->
  Set r
lookupHQRef searchType which isPrefixOf hq names =
  case hq of
    HQ.NameOnly n -> doSearch n refs
    HQ.HashQualified n sh -> matches refs
      where
        matches :: Relation Name r -> Set r
        matches ns =
          Set.filter (isPrefixOf sh) (doSearch n ns)
    HQ.HashOnly sh -> matches refs
      where
        matches :: Relation Name r -> Set r
        matches ns =
          Set.filter (isPrefixOf sh) (R.ran ns)
  where
    doSearch = case searchType of
      IncludeSuffixes -> Name.searchByRankedSuffix
      ExactName -> Relation.lookupDom
    refs = which names

-- Look up a ref's names, and hash-qualify them if they are conflicted.
typeName :: Int -> Reference -> Names -> Set (HQ'.HashQualified Name)
typeName length r names =
  Set.map
    (\n -> if isConflicted n then hq n else HQ'.fromName n)
    (R.lookupRan r . Names.types $ names)
  where
    hq n = HQ'.take length (HQ'.fromNamedReference n r)
    isConflicted n = R.manyDom n (Names.types names)

-- List of names for a referent, longer names (by number of segments) first.
termNamesByLength :: Int -> Referent -> Names -> [HQ'.HashQualified Name]
termNamesByLength length r ns =
  sortOn len (toList $ termName length r ns)
  where
    len (HQ'.NameOnly n) = Name.countSegments n
    len (HQ'.HashQualified n _) = Name.countSegments n

-- The longest term name (by segment count) for a `Referent`.
longestTermName :: Int -> Referent -> Names -> HQ.HashQualified Name
longestTermName length r ns =
  case reverse (termNamesByLength length r ns) of
    [] -> HQ.take length (HQ.fromReferent r)
    (h : _) -> HQ'.toHQ h

termName :: Int -> Referent -> Names -> Set (HQ'.HashQualified Name)
termName length r names =
  Set.map
    (\n -> if isConflicted n then hq n else HQ'.fromName n)
    (R.lookupRan r . Names.terms $ names)
  where
    hq n = HQ'.take length (HQ'.fromNamedReferent n r)
    isConflicted n = R.manyDom n (Names.terms names)

lookupHQPattern ::
  SearchType ->
  HQ.HashQualified Name ->
  CT.ConstructorType ->
  Names ->
  Set ConstructorReference
lookupHQPattern searchType hq ctt names =
  Set.fromList
    [ r
      | Referent.Con r ct <- toList $ lookupHQTerm searchType hq names,
        ct == ctt
    ]
