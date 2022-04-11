{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Names
  ( Names (..),
    addTerm,
    addType,
    labeledReferences,
    conflicts,
    contains,
    difference,
    filter,
    filterByHQs,
    filterBySHs,
    filterTypes,
    map,
    makeAbsolute,
    fuzzyFind,
    hqName,
    hqTermName,
    hqTypeName,
    hqTermName',
    hqTypeName',
    _hqTermName,
    _hqTypeName,
    _hqTermAliases,
    _hqTypeAliases,
    mapNames,
    prefix0,
    restrictReferences,
    refTermsNamed,
    termReferences,
    termReferents,
    typeReferences,
    termsNamed,
    typesNamed,
    unionLeft,
    unionLeftName,
    namesForReference,
    namesForReferent,
    shadowTerms,
    importing,
    constructorsForType,
    expandWildcardImport,
    isEmpty,
    hashQualifyTypesRelation,
    hashQualifyTermsRelation,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.FuzzyFind as FZF
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
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
import qualified Unison.Util.Relation as Relation
import Prelude hiding (filter, map)
import qualified Prelude

-- This will support the APIs of both PrettyPrintEnv and the old Names.
-- For pretty-printing, we need to look up names for References.
-- For parsing (both .u files and command-line args)
data Names = Names
  { terms :: Relation Name Referent,
    types :: Relation Name Reference
  }
  deriving (Eq, Ord)

instance Semigroup (Names) where (<>) = mappend

instance Monoid (Names) where
  mempty = Names mempty mempty
  Names e1 t1 `mappend` Names e2 t2 =
    Names (e1 <> e2) (t1 <> t2)

instance Show (Names) where
  show (Names terms types) =
    "Terms:\n"
      ++ foldMap (\(n, r) -> "  " ++ show n ++ " -> " ++ show r ++ "\n") (R.toList terms)
      ++ "\n"
      ++ "Types:\n"
      ++ foldMap (\(n, r) -> "  " ++ show n ++ " -> " ++ show r ++ "\n") (R.toList types)
      ++ "\n"

isEmpty :: Names -> Bool
isEmpty n = R.null (terms n) && R.null (types n)

map :: (Name -> Name) -> Names -> Names
map f (Names {terms, types}) = Names terms' types'
  where
    terms' = R.mapDom f terms
    types' = R.mapDom f types

makeAbsolute :: Names -> Names
makeAbsolute = map Name.makeAbsolute

-- Finds names that are supersequences of all the given strings, ordered by
-- score and grouped by name.
fuzzyFind ::
  [String] ->
  Names ->
  [(FZF.Alignment, Name, Set (Either Referent Reference))]
fuzzyFind query names =
  fmap flatten
    . fuzzyFinds (Name.toString . fst) query
    . Prelude.filter prefilter
    . Map.toList
    -- `mapMonotonic` is safe here and saves a log n factor
    $ (Set.mapMonotonic Left <$> R.toMultimap (terms names))
      <> (Set.mapMonotonic Right <$> R.toMultimap (types names))
  where
    lowerqueryt = Text.toLower . Text.pack <$> query
    -- For performance, case-insensitive substring matching as a pre-filter
    -- This finds fewer matches than subsequence matching, but is
    -- (currently) way faster even on large name sets.
    prefilter (Name.toText -> name, _) = case lowerqueryt of
      -- Special cases here just to help optimizer, since
      -- not sure if `all` will get sufficiently unrolled for
      -- Text fusion to work out.
      [q] -> q `Text.isInfixOf` lowername
      [q1, q2] -> q1 `Text.isInfixOf` lowername && q2 `Text.isInfixOf` lowername
      query -> all (`Text.isInfixOf` lowername) query
      where
        lowername = Text.toLower name
    flatten (a, (b, c)) = (a, b, c)
    fuzzyFinds :: (a -> String) -> [String] -> [a] -> [(FZF.Alignment, a)]
    fuzzyFinds f query d =
      d
        >>= ( \s ->
                toList $
                  (,s)
                    <$> foldl'
                      (\a q -> (<>) <$> a <*> FZF.bestMatch q (f s))
                      (Just mempty)
                      query
            )

termReferences, typeReferences :: Names -> Set Reference
termReferences Names {..} = Set.map Referent.toReference $ R.ran terms
typeReferences Names {..} = R.ran types

-- | Collect all references in the given Names, tagged with their type.
labeledReferences :: Names -> Set LabeledDependency
labeledReferences Names {..} =
  Set.map LD.typeRef (Relation.ran types)
    <> Set.map LD.referent (Relation.ran terms)

termReferents :: Names -> Set Referent
termReferents Names {..} = R.ran terms

restrictReferences :: Set Reference -> Names -> Names
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
unionLeftName :: Names -> Names -> Names
unionLeftName = unionLeft' $ const . R.memberDom

-- unionLeft two Names, including new name conflicts, but excluding new aliases.
-- e.g. unionLeftRef [foo -> #a, bar -> #a, cat -> #c]
--                   [foo -> #b, baz -> #c]
--                 = [foo -> #a, bar -> #a, foo -> #b, cat -> #c]
_unionLeftRef :: Names -> Names -> Names
_unionLeftRef = unionLeft' $ const R.memberRan

-- unionLeft two Names, but don't create new aliases or new name conflicts.
-- e.g. unionLeft [foo -> #a, bar -> #a, cat -> #c]
--                [foo -> #b, baz -> #c]
--              = [foo -> #a, bar -> #a, cat -> #c]
unionLeft :: Names -> Names -> Names
unionLeft = unionLeft' go
  where
    go n r acc = R.memberDom n acc || R.memberRan r acc

-- implementation detail of the above
unionLeft' ::
  (forall a b. (Ord a, Ord b) => a -> b -> Relation a b -> Bool) ->
  Names ->
  Names ->
  Names
unionLeft' p a b = Names terms' types'
  where
    terms' = foldl' go (terms a) (R.toList $ terms b)
    types' = foldl' go (types a) (R.toList $ types b)
    go :: (Ord a, Ord b) => Relation a b -> (a, b) -> Relation a b
    go acc (n, r) = if p n r acc then acc else R.insert n r acc

-- | TODO: get this from database. For now it's a constant.
numHashChars :: Int
numHashChars = 3

termsNamed :: Names -> Name -> Set Referent
termsNamed = flip R.lookupDom . terms

refTermsNamed :: Names -> Name -> Set Reference
refTermsNamed names n =
  Set.fromList [r | Referent.Ref r <- toList $ termsNamed names n]

typesNamed :: Names -> Name -> Set Reference
typesNamed = flip R.lookupDom . types

namesForReferent :: Names -> Referent -> Set Name
namesForReferent names r = R.lookupRan r (terms names)

namesForReference :: Names -> Reference -> Set Name
namesForReference names r = R.lookupRan r (types names)

termAliases :: Names -> Name -> Referent -> Set Name
termAliases names n r = Set.delete n $ namesForReferent names r

typeAliases :: Names -> Name -> Reference -> Set Name
typeAliases names n r = Set.delete n $ namesForReference names r

addType :: Name -> Reference -> Names -> Names
addType n r = (<> fromTypes [(n, r)])

addTerm :: Name -> Referent -> Names -> Names
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
hqName :: Names -> Name -> Either Reference Referent -> HQ'.HashQualified Name
hqName b n = \case
  Left r -> if ambiguous then _hqTypeName' b n r else HQ'.fromName n
  Right r -> if ambiguous then _hqTermName' b n r else HQ'.fromName n
  where
    ambiguous = Set.size (termsNamed b n) + Set.size (typesNamed b n) > 1

-- Conditionally apply hash qualifier to term name.
-- Should be the same as the input name if the Names is unconflicted.
hqTermName :: Int -> Names -> Name -> Referent -> HQ'.HashQualified Name
hqTermName hqLen b n r =
  if Set.size (termsNamed b n) > 1
    then hqTermName' hqLen n r
    else HQ'.fromName n

hqTypeName :: Int -> Names -> Name -> Reference -> HQ'.HashQualified Name
hqTypeName hqLen b n r =
  if Set.size (typesNamed b n) > 1
    then hqTypeName' hqLen n r
    else HQ'.fromName n

_hqTermName :: Names -> Name -> Referent -> HQ'.HashQualified Name
_hqTermName b n r =
  if Set.size (termsNamed b n) > 1
    then _hqTermName' b n r
    else HQ'.fromName n

_hqTypeName :: Names -> Name -> Reference -> HQ'.HashQualified Name
_hqTypeName b n r =
  if Set.size (typesNamed b n) > 1
    then _hqTypeName' b n r
    else HQ'.fromName n

_hqTypeAliases :: Names -> Name -> Reference -> Set (HQ'.HashQualified Name)
_hqTypeAliases b n r = Set.map (flip (_hqTypeName b) r) (typeAliases b n r)

_hqTermAliases :: Names -> Name -> Referent -> Set (HQ'.HashQualified Name)
_hqTermAliases b n r = Set.map (flip (_hqTermName b) r) (termAliases b n r)

-- Unconditionally apply hash qualifier long enough to distinguish all the
-- References in this Names.
hqTermName' :: Int -> Name -> Referent -> HQ'.HashQualified Name
hqTermName' hqLen n r =
  HQ'.take hqLen $ HQ'.fromNamedReferent n r

hqTypeName' :: Int -> Name -> Reference -> HQ'.HashQualified Name
hqTypeName' hqLen n r =
  HQ'.take hqLen $ HQ'.fromNamedReference n r

_hqTermName' :: Names -> Name -> Referent -> HQ'.HashQualified Name
_hqTermName' _b n r =
  HQ'.take numHashChars $ HQ'.fromNamedReferent n r

_hqTypeName' :: Names -> Name -> Reference -> HQ'.HashQualified Name
_hqTypeName' _b n r =
  HQ'.take numHashChars $ HQ'.fromNamedReference n r

fromTerms :: [(Name, Referent)] -> Names
fromTerms ts = Names (R.fromList ts) mempty

fromTypes :: [(Name, Reference)] -> Names
fromTypes ts = Names mempty (R.fromList ts)

-- | Map over each name in a 'Names'.
mapNames :: (Name -> Name) -> Names -> Names
mapNames f Names {terms, types} =
  Names
    { terms = R.mapDom f terms,
      types = R.mapDom f types
    }

-- | @prefix0 n ns@ prepends @n@ to each name in @ns@.
--
-- Precondition: every name in @ns@ is relative.
prefix0 :: Name -> Names -> Names
prefix0 n =
  mapNames (Name.joinDot n)

filter :: (Name -> Bool) -> Names -> Names
filter f (Names terms types) = Names (R.filterDom f terms) (R.filterDom f types)

-- currently used for filtering before a conditional `add`
filterByHQs :: Set (HQ'.HashQualified Name) -> Names -> Names
filterByHQs hqs Names {..} = Names terms' types'
  where
    terms' = R.filter f terms
    types' = R.filter g types
    f (n, r) = any (HQ'.matchesNamedReferent n r) hqs
    g (n, r) = any (HQ'.matchesNamedReference n r) hqs

filterBySHs :: Set ShortHash -> Names -> Names
filterBySHs shs Names {..} = Names terms' types'
  where
    terms' = R.filter f terms
    types' = R.filter g types
    f (_n, r) = any (`SH.isPrefixOf` Referent.toShortHash r) shs
    g (_n, r) = any (`SH.isPrefixOf` Reference.toShortHash r) shs

filterTypes :: (Name -> Bool) -> Names -> Names
filterTypes f (Names terms types) = Names terms (R.filterDom f types)

difference :: Names -> Names -> Names
difference a b =
  Names
    (R.difference (terms a) (terms b))
    (R.difference (types a) (types b))

contains :: Names -> Reference -> Bool
contains names =
  -- We want to compute `termsReferences` only once, if `contains` is partially applied to a `Names`, and called over
  -- and over for different references. GHC would probably float `termsReferences` out without the explicit lambda, but
  -- it's written like this just to be sure.
  \r -> Set.member r termsReferences || R.memberRan r (types names)
  where
    -- this check makes `contains` O(n) instead of O(log n)
    termsReferences :: Set Reference
    termsReferences =
      Set.map Referent.toReference (R.ran (terms names))

-- | filters out everything from the domain except what's conflicted
conflicts :: Names -> Names
conflicts Names {..} = Names (R.filterManyDom terms) (R.filterManyDom types)

-- Deletes from the `n0 : Names` any definitions whose names
-- are in `ns`. Does so using logarithmic time lookups,
-- traversing only `ns`.
--
-- See usage in `FileParser` for handling precendence of symbol
-- resolution where local names are preferred to codebase names.
shadowTerms :: [Name] -> Names -> Names
shadowTerms ns n0 = Names terms' (types n0)
  where
    terms' = foldl' go (terms n0) ns
    go ts name = R.deleteDom name ts

-- | Given a mapping from name to qualified name, update a `Names`,
-- so for instance if the input has [(Some, Optional.Some)],
-- and `Optional.Some` is a constructor in the input `Names`,
-- the alias `Some` will map to that same constructor and shadow
-- anything else that is currently called `Some`.
importing :: [(Name, Name)] -> Names -> Names
importing shortToLongName ns =
  Names
    (foldl' go (terms ns) shortToLongName)
    (foldl' go (types ns) shortToLongName)
  where
    go :: (Ord r) => Relation Name r -> (Name, Name) -> Relation Name r
    go m (shortname, qname) = case Name.searchByRankedSuffix qname m of
      s
        | Set.null s -> m
        | otherwise -> R.insertManyRan shortname s (R.deleteDom shortname m)

-- | Converts a wildcard import into a list of explicit imports, of the form
-- [(suffix, full)]. Example: if `io` contains two functions, `foo` and
-- `bar`, then `expandWildcardImport io` will produce
-- `[(foo, io.foo), (bar, io.bar)]`.
expandWildcardImport :: Name -> Names -> [(Name, Name)]
expandWildcardImport prefix ns =
  [(suffix, full) | Just (suffix, full) <- go <$> R.toList (terms ns)]
    <> [(suffix, full) | Just (suffix, full) <- go <$> R.toList (types ns)]
  where
    go :: (Name, a) -> Maybe (Name, Name)
    go (full, _) = do
      -- running example:
      --   prefix = Int
      --   full = builtin.Int.negate
      rem <- Name.suffixFrom prefix full
      -- rem = Int.negate
      suffix <- Name.stripNamePrefix prefix rem
      -- suffix = negate
      pure (suffix, full)

-- Finds all the constructors for the given type in the `Names`
constructorsForType :: Reference -> Names -> [(Name, Referent)]
constructorsForType r ns =
  let -- rather than searching all of names, we use the known possible forms
      -- that the constructors can take
      possibleDatas = [Referent.Con (ConstructorReference r cid) CT.Data | cid <- [0 ..]]
      possibleEffects = [Referent.Con (ConstructorReference r cid) CT.Effect | cid <- [0 ..]]
      trim [] = []
      trim (h : t) = case R.lookupRan h (terms ns) of
        s
          | Set.null s -> []
          | otherwise -> [(n, h) | n <- toList s] ++ trim t
   in trim possibleEffects ++ trim possibleDatas

hashQualifyTermsRelation :: R.Relation Name Referent -> R.Relation (HQ.HashQualified Name) Referent
hashQualifyTermsRelation = hashQualifyRelation HQ.fromNamedReferent

hashQualifyTypesRelation :: R.Relation Name Reference -> R.Relation (HQ.HashQualified Name) Reference
hashQualifyTypesRelation = hashQualifyRelation HQ.fromNamedReference

hashQualifyRelation :: Ord r => (Name -> r -> HQ.HashQualified Name) -> R.Relation Name r -> R.Relation (HQ.HashQualified Name) r
hashQualifyRelation fromNamedRef rel = R.map go rel
  where
    go (n, r) =
      if Set.size (R.lookupDom n rel) > 1
        then (HQ.take numHashChars $ fromNamedRef n r, r)
        else (HQ.NameOnly n, r)
