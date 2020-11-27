{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Names2
  ( Names0,
    Names' (Names),
    Names,
    addTerm,
    addType,
    allReferences,
    conflicts,
    contains,
    difference,
    filter,
    filterByHQs,
    filterBySHs,
    filterTypes,
    hqName,
    hqTermName,
    hqTypeName,
    hqTermName',
    hqTypeName',
    _hqTermName,
    _hqTypeName,
    _hqTermAliases,
    _hqTypeAliases,
    names0ToNames,
    prefix0,
    restrictReferences,
    refTermsNamed,
    terms,
    types,
    termReferences,
    termReferents,
    typeReferences,
    termsNamed,
    typesNamed,
    unionLeft,
    unionLeftName,
    namesForReference,
    namesForReferent,
  )
where

import qualified Data.Set as Set
import Unison.HashQualified' (HashQualified)
import qualified Unison.HashQualified' as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as R
import Prelude hiding (filter)

-- This will support the APIs of both PrettyPrintEnv and the old Names.
-- For pretty-printing, we need to look up names for References; they may have
-- some hash-qualification, depending on the context.
-- For parsing (both .u files and command-line args)
data Names' n = Names
  { terms :: Relation n Referent,
    types :: Relation n Reference
  }
  deriving (Eq, Ord)

type Names = Names' HashQualified

type Names0 = Names' Name

names0ToNames :: Names0 -> Names
names0ToNames names0 = Names terms' types'
  where
    terms' = R.map doTerm (terms names0)
    types' = R.map doType (types names0)
    length = numHashChars names0
    doTerm (n, r) =
      if Set.size (R.lookupDom n (terms names0)) > 1
        then (HQ.take length $ HQ.fromNamedReferent n r, r)
        else (HQ.NameOnly n, r)
    doType (n, r) =
      if Set.size (R.lookupDom n (types names0)) > 1
        then (HQ.take length $ HQ.fromNamedReference n r, r)
        else (HQ.NameOnly n, r)

termReferences, typeReferences, allReferences :: Names' n -> Set Reference
termReferences Names {..} = Set.map Referent.toReference $ R.ran terms
typeReferences Names {..} = R.ran types
allReferences n = termReferences n <> typeReferences n

termReferents :: Names' n -> Set Referent
termReferents Names {..} = R.ran terms

restrictReferences :: Ord n => Set Reference -> Names' n -> Names' n
restrictReferences refs Names {..} = Names terms' types'
  where
    terms' = R.filterRan ((`Set.member` refs) . Referent.toReference) terms
    types' = R.filterRan (`Set.member` refs) types

-- | Guide to unionLeft*
-- Is it ok to create new aliases for parsing?
--    Sure.
--
-- Is it ok to create name conflicts for parsing?
--    It's okay but not great. The user will have to hash-qualify to disambiguate.
--
-- Is it ok to create new aliases for pretty-printing?
--    Not helpful, we need to choose a name to show.
--    We'll just have to choose one at random if there are aliases.
-- Is it ok to create name conflicts for pretty-printing?
--    Still okay but not great.  The pretty-printer will have to hash-qualify
--    to disambiguate.
--
-- Thus, for parsing:
--       unionLeftName is good if the name `n` on the left is the only `n` the
--           user will want to reference.  It allows the rhs to add aliases.
--       unionLeftRef allows new conflicts but no new aliases.  Lame?
--       (<>) is ok for parsing if we expect to add some conflicted names,
--           e.g. from history
--
-- For pretty-printing:
--       Probably don't want to add new aliases, unless we don't know which
--       `Names` is higher priority.  So if we do have a preferred `Names`,
--       don't use `unionLeftName` or (<>).
--       You don't want to create new conflicts either if you have a preferred
--       `Names`.  So in this case, don't use `unionLeftRef` either.
--       I guess that leaves `unionLeft`.
--
-- Not sure if the above is helpful or correct!

-- unionLeft two Names, including new aliases, but excluding new name conflicts.
-- e.g. unionLeftName [foo -> #a, bar -> #a, cat -> #c]
--                    [foo -> #b, baz -> #c]
--                  = [foo -> #a, bar -> #a, baz -> #c, cat -> #c)]
-- Btw, it's ok to create name conflicts for parsing environments, if you don't
-- mind disambiguating.
unionLeftName :: Ord n => Names' n -> Names' n -> Names' n
unionLeftName = unionLeft' $ const . R.memberDom

-- unionLeft two Names, including new name conflicts, but excluding new aliases.
-- e.g. unionLeftRef [foo -> #a, bar -> #a, cat -> #c]
--                   [foo -> #b, baz -> #c]
--                 = [foo -> #a, bar -> #a, foo -> #b, cat -> #c]
_unionLeftRef :: Ord n => Names' n -> Names' n -> Names' n
_unionLeftRef = unionLeft' $ const R.memberRan

-- unionLeft two Names, but don't create new aliases or new name conflicts.
-- e.g. unionLeft [foo -> #a, bar -> #a, cat -> #c]
--                [foo -> #b, baz -> #c]
--              = [foo -> #a, bar -> #a, cat -> #c]
unionLeft :: Ord n => Names' n -> Names' n -> Names' n
unionLeft = unionLeft' go
  where
    go n r acc = R.memberDom n acc || R.memberRan r acc

-- implementation detail of the above
unionLeft' ::
  Ord n =>
  (forall a b. (Ord a, Ord b) => a -> b -> Relation a b -> Bool) ->
  Names' n ->
  Names' n ->
  Names' n
unionLeft' p a b = Names terms' types'
  where
    terms' = foldl' go (terms a) (R.toList $ terms b)
    types' = foldl' go (types a) (R.toList $ types b)
    go :: (Ord a, Ord b) => Relation a b -> (a, b) -> Relation a b
    go acc (n, r) = if p n r acc then acc else R.insert n r acc

-- could move this to a read-only field in Names
-- todo: kill this function and pass thru an Int from the codebase, I suppose
numHashChars :: Names' n -> Int
numHashChars b = lenFor hashes
  where
    lenFor _hashes = 3
    hashes = foldl' f (foldl' g mempty (R.ran $ types b)) (R.ran $ terms b)
    g s r = Set.insert r s
    f s r = Set.insert (Referent.toReference r) s

termsNamed :: Ord n => Names' n -> n -> Set Referent
termsNamed = flip R.lookupDom . terms

refTermsNamed :: Ord n => Names' n -> n -> Set Reference
refTermsNamed names n =
  Set.fromList [r | Referent.Ref r <- toList $ termsNamed names n]

typesNamed :: Ord n => Names' n -> n -> Set Reference
typesNamed = flip R.lookupDom . types

namesForReferent :: Names' n -> Referent -> Set n
namesForReferent names r = R.lookupRan r (terms names)

namesForReference :: Names' n -> Reference -> Set n
namesForReference names r = R.lookupRan r (types names)

termAliases :: Ord n => Names' n -> n -> Referent -> Set n
termAliases names n r = Set.delete n $ namesForReferent names r

typeAliases :: Ord n => Names' n -> n -> Reference -> Set n
typeAliases names n r = Set.delete n $ namesForReference names r

addType :: Ord n => n -> Reference -> Names' n -> Names' n
addType n r = (<> fromTypes [(n, r)])

addTerm :: Ord n => n -> Referent -> Names' n -> Names' n
addTerm n r = (<> fromTerms [(n, r)])

-- | Like hqTermName and hqTypeName, but considers term and type names to
-- conflict with each other (so will hash-qualify if there is e.g. both a term
-- and a type named "foo").
--
-- This is useful in contexts such as printing branch diffs. Example:
--
--     - Deletes:
--
--       foo
--       foo
--
-- We want to append the hash regardless of whether or not one is a term and the
-- other is a type.
hqName :: Ord n => Names' n -> n -> Either Reference Referent -> HQ.HashQualified' n
hqName b n = \case
  Left r -> if ambiguous then _hqTypeName' b n r else HQ.fromName n
  Right r -> if ambiguous then _hqTermName' b n r else HQ.fromName n
  where
    ambiguous = Set.size (termsNamed b n) + Set.size (typesNamed b n) > 1

-- Conditionally apply hash qualifier to term name.
-- Should be the same as the input name if the Names0 is unconflicted.
hqTermName :: Ord n => Int -> Names' n -> n -> Referent -> HQ.HashQualified' n
hqTermName hqLen b n r =
  if Set.size (termsNamed b n) > 1
    then hqTermName' hqLen n r
    else HQ.fromName n

hqTypeName :: Ord n => Int -> Names' n -> n -> Reference -> HQ.HashQualified' n
hqTypeName hqLen b n r =
  if Set.size (typesNamed b n) > 1
    then hqTypeName' hqLen n r
    else HQ.fromName n

_hqTermName :: Ord n => Names' n -> n -> Referent -> HQ.HashQualified' n
_hqTermName b n r =
  if Set.size (termsNamed b n) > 1
    then _hqTermName' b n r
    else HQ.fromName n

_hqTypeName :: Ord n => Names' n -> n -> Reference -> HQ.HashQualified' n
_hqTypeName b n r =
  if Set.size (typesNamed b n) > 1
    then _hqTypeName' b n r
    else HQ.fromName n

_hqTypeAliases ::
  Ord n => Names' n -> n -> Reference -> Set (HQ.HashQualified' n)
_hqTypeAliases b n r = Set.map (flip (_hqTypeName b) r) (typeAliases b n r)

_hqTermAliases :: Ord n => Names' n -> n -> Referent -> Set (HQ.HashQualified' n)
_hqTermAliases b n r = Set.map (flip (_hqTermName b) r) (termAliases b n r)

-- Unconditionally apply hash qualifier long enough to distinguish all the
-- References in this Names0.
hqTermName' :: Int -> n -> Referent -> HQ.HashQualified' n
hqTermName' hqLen n r =
  HQ.take hqLen $ HQ.fromNamedReferent n r

hqTypeName' :: Int -> n -> Reference -> HQ.HashQualified' n
hqTypeName' hqLen n r =
  HQ.take hqLen $ HQ.fromNamedReference n r

_hqTermName' :: Names' n -> n -> Referent -> HQ.HashQualified' n
_hqTermName' b n r =
  HQ.take (numHashChars b) $ HQ.fromNamedReferent n r

_hqTypeName' :: Names' n -> n -> Reference -> HQ.HashQualified' n
_hqTypeName' b n r =
  HQ.take (numHashChars b) $ HQ.fromNamedReference n r

fromTerms :: Ord n => [(n, Referent)] -> Names' n
fromTerms ts = Names (R.fromList ts) mempty

fromTypes :: Ord n => [(n, Reference)] -> Names' n
fromTypes ts = Names mempty (R.fromList ts)

prefix0 :: Name -> Names0 -> Names0
prefix0 n (Names terms types) = Names terms' types'
  where
    terms' = R.mapDom (Name.joinDot n) terms
    types' = R.mapDom (Name.joinDot n) types

filter :: Ord n => (n -> Bool) -> Names' n -> Names' n
filter f (Names terms types) = Names (R.filterDom f terms) (R.filterDom f types)

-- currently used for filtering before a conditional `add`
filterByHQs :: Set HashQualified -> Names0 -> Names0
filterByHQs hqs Names {..} = Names terms' types'
  where
    terms' = R.filter f terms
    types' = R.filter g types
    f (n, r) = any (HQ.matchesNamedReferent n r) hqs
    g (n, r) = any (HQ.matchesNamedReference n r) hqs

filterBySHs :: Set ShortHash -> Names0 -> Names0
filterBySHs shs Names {..} = Names terms' types'
  where
    terms' = R.filter f terms
    types' = R.filter g types
    f (_n, r) = any (`SH.isPrefixOf` Referent.toShortHash r) shs
    g (_n, r) = any (`SH.isPrefixOf` Reference.toShortHash r) shs

filterTypes :: Ord n => (n -> Bool) -> Names' n -> Names' n
filterTypes f (Names terms types) = Names terms (R.filterDom f types)

difference :: Ord n => Names' n -> Names' n -> Names' n
difference a b =
  Names
    (R.difference (terms a) (terms b))
    (R.difference (types a) (types b))

contains :: Names' n -> Reference -> Bool
contains names r =
  -- this check makes `contains` O(n) instead of O(log n)
  (Set.member r . Set.map Referent.toReference . R.ran) (terms names)
    || R.memberRan r (types names)

-- | filters out everything from the domain except what's conflicted
conflicts :: Ord n => Names' n -> Names' n
conflicts Names {..} = Names (R.filterManyDom terms) (R.filterManyDom types)

instance Ord n => Semigroup (Names' n) where (<>) = mappend

instance Ord n => Monoid (Names' n) where
  mempty = Names mempty mempty
  Names e1 t1 `mappend` Names e2 t2 =
    Names (e1 <> e2) (t1 <> t2)

instance Show n => Show (Names' n) where
  show (Names terms types) =
    "Terms:\n"
      ++ foldMap (\(n, r) -> "  " ++ show n ++ " -> " ++ show r ++ "\n") (R.toList terms)
      ++ "\n"
      ++ "Types:\n"
      ++ foldMap (\(n, r) -> "  " ++ show n ++ " -> " ++ show r ++ "\n") (R.toList types)
      ++ "\n"
