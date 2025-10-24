module Unison.HashQualifiedPrime
  ( HashQualified (..),
    HQSegment,
    toHQ,
    HashOrHQ,
    fromHQ,
    toName,
    nameLength,
    take,
    toHash,
    toTextWith,
    fromNamedReferent,
    fromNamedReference,
    fromName,
    matchesNamedReferent,
    matchesNamedReference,
    requalify,
    searchBySuffix,
    filterBySuffix,
    searchUnconflictedBySuffix,
    filterUnconflictedBySuffix,
  )
where

import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Prelude hiding (take)

-- | Like Unison.HashQualified, but doesn't support a HashOnly variant
data HashQualified n
  = NameOnly n
  | HashQualified n ShortHash
  deriving stock (Eq, Functor, Generic, Foldable, Ord, Show, Traversable)

type HQSegment = HashQualified NameSegment

toHQ :: HashQualified n -> HQ.HashQualified n
toHQ = \case
  NameOnly n -> HQ.NameOnly n
  HashQualified n sh -> HQ.HashQualified n sh

type HashOrHQ n = Either ShortHash (HashQualified n)

-- | If the 'HQ.HashQualified' is just a 'ShortHash', return it on the 'Left', otherwise return a `HashQualified` on the
--  `Right`.
fromHQ :: HQ.HashQualified n -> HashOrHQ n
fromHQ = \case
  HQ.NameOnly n -> Right $ NameOnly n
  HQ.HashQualified n sh -> Right $ HashQualified n sh
  HQ.HashOnly sh -> Left sh

toName :: HashQualified n -> n
toName = \case
  NameOnly name -> name
  HashQualified name _ -> name

nameLength :: (Name -> Text) -> HashQualified Name -> Int
nameLength nameToText = Text.length . toTextWith nameToText

take :: Int -> HashQualified n -> HashQualified n
take i = \case
  n@(NameOnly _) -> n
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.shortenTo i s)

toHash :: HashQualified n -> Maybe ShortHash
toHash = \case
  NameOnly _ -> Nothing
  HashQualified _ sh -> Just sh

toTextWith :: (n -> Text) -> HashQualified n -> Text
toTextWith f = \case
  NameOnly name -> f name
  HashQualified name hash -> f name <> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromName :: n -> HashQualified n
fromName = NameOnly

matchesNamedReferent :: (Eq n) => n -> Referent -> HashQualified n -> Bool
matchesNamedReferent n r = \case
  NameOnly n' -> n' == n
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Referent.toShortHash r

matchesNamedReference :: (Eq n) => n -> Reference -> HashQualified n -> Bool
matchesNamedReference n r = \case
  NameOnly n' -> n' == n
  HashQualified n' sh -> n' == n && sh `Reference.isPrefixOf` r

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified Name -> Referent -> HashQualified Name
requalify hq r = case hq of
  NameOnly n -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r

-- | Like 'Name.searchBySuffix', but uses a hash-qualified name to search instead.
--
-- The name *and* the hash are used to determine whether something is an exact match. For example, in namespace
-- {foo#foo, hello.foo#bar}, searching for foo#bar will return the singleton set {hello.foo#bar}, because even though
-- there is an exact name match on foo, its hash doesn't match so we fall back to "suffix" matches. This probably isn't
-- a very important detail in practice, but the other possible implementation (do name-only search, *then* filter result
-- down to matching hashes) seems worse.
searchBySuffix :: forall ref. (Ord ref) => (ref -> ShortHash) -> HashQualified Name -> Relation Name ref -> Set ref
searchBySuffix _ (NameOnly name) rel = Name.searchBySuffix name rel
searchBySuffix refHash (HashQualified name hash) rel
  | Set.null exactMatches = suffixMatches
  | otherwise = exactMatches
  where
    exactMatches :: Set ref
    exactMatches =
      keepMatchingHashes (Relation.lookupDom name rel)

    suffixMatches :: Set ref
    suffixMatches =
      keepMatchingHashes (Relation.searchDom (Name.compareSuffix name) rel)

    keepMatchingHashes :: Set ref -> Set ref
    keepMatchingHashes =
      Set.filter \ref -> hash `SH.isPrefixOf` refHash ref

-- | Like 'searchBySuffix', but also keeps the names around.
filterBySuffix ::
  forall ref.
  (Ord ref) =>
  (ref -> ShortHash) ->
  HashQualified Name ->
  Relation Name ref ->
  Relation Name ref
filterBySuffix _ (NameOnly name) rel = Name.filterBySuffix name rel
filterBySuffix refHash (HashQualified name hash) rel
  | Relation.null exactMatches = suffixMatches
  | otherwise = exactMatches
  where
    exactMatches :: Relation Name ref
    exactMatches =
      matches name (Relation.lookupDom name rel)

    suffixMatches :: Relation Name ref
    suffixMatches =
      Relation.searchDomG matches (Name.compareSuffix name) rel

    matches :: Name -> Set ref -> Relation Name ref
    matches name =
      Set.filter hashMatches
        >>> Set.NonEmpty.nonEmptySet
        >>> maybe Relation.empty (Relation.singletonSet name)

    hashMatches :: ref -> Bool
    hashMatches ref =
      hash `SH.isPrefixOf` refHash ref

searchUnconflictedBySuffix ::
  forall ref.
  (Ord ref) =>
  (ref -> ShortHash) ->
  HashQualified Name ->
  BiMultimap ref Name ->
  Set ref
searchUnconflictedBySuffix _ (NameOnly name) m = Name.searchUnconflictedBySuffix name m
searchUnconflictedBySuffix refHash (HashQualified name hash) m =
  maybe suffixMatches Set.singleton exactMatch
  where
    exactMatch :: Maybe ref
    exactMatch = do
      ref <- BiMultimap.lookupRan name m
      guard (hash `SH.isPrefixOf` refHash ref)
      Just ref

    suffixMatches :: Set ref
    suffixMatches =
      m
        & BiMultimap.searchRan (\ref _ -> Set.singleton ref) (Name.compareSuffix name)
        & Set.filter \ref -> hash `SH.isPrefixOf` refHash ref

filterUnconflictedBySuffix ::
  forall ref.
  (Ord ref) =>
  (ref -> ShortHash) ->
  HashQualified Name ->
  BiMultimap ref Name ->
  BiMultimap ref Name
filterUnconflictedBySuffix _ (NameOnly name) m = Name.filterUnconflictedBySuffix name m
filterUnconflictedBySuffix refHash (HashQualified name hash) m =
  maybe suffixMatches (\ref -> BiMultimap.singleton ref name) exactMatch
  where
    exactMatch :: Maybe ref
    exactMatch = do
      ref <- BiMultimap.lookupRan name m
      guard (hashMatches ref)
      Just ref

    suffixMatches :: BiMultimap ref Name
    suffixMatches =
      BiMultimap.searchrRan f BiMultimap.empty (Name.compareSuffix name) m
      where
        f :: ref -> Name -> BiMultimap ref Name -> BiMultimap ref Name
        f ref name acc
          | hashMatches ref = BiMultimap.insert ref name acc
          | otherwise = acc

    hashMatches :: ref -> Bool
    hashMatches ref =
      hash `SH.isPrefixOf` refHash ref

instance (Name.Alphabetical n) => Name.Alphabetical (HashQualified n) where
  compareAlphabetical (NameOnly n) (NameOnly n2) = Name.compareAlphabetical n n2
  -- NameOnly comes first
  compareAlphabetical NameOnly {} HashQualified {} = LT
  compareAlphabetical HashQualified {} NameOnly {} = GT
  compareAlphabetical (HashQualified n sh) (HashQualified n2 sh2) = Name.compareAlphabetical n n2 <> compare sh sh2
