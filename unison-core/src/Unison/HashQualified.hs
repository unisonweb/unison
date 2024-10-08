module Unison.HashQualified where

import Unison.ConstructorReference (ConstructorReference)
import Unison.ConstructorReference qualified as ConstructorReference
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Prelude hiding (fromString)
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH
import Prelude hiding (take)

data HashQualified n
  = NameOnly n
  | HashOnly ShortHash
  | HashQualified n ShortHash
  deriving stock (Eq, Foldable, Ord, Traversable, Functor, Show, Generic)

stripNamespace :: Name -> HashQualified Name -> HashQualified Name
stripNamespace namespace hq = case hq of
  NameOnly name -> NameOnly $ strip name
  HashQualified name sh -> HashQualified (strip name) sh
  ho -> ho
  where
    strip name =
      fromMaybe name $ Name.stripNamePrefix namespace name

toName :: HashQualified n -> Maybe n
toName = \case
  NameOnly name -> Just name
  HashQualified name _ -> Just name
  HashOnly _ -> Nothing

toHash :: HashQualified n -> Maybe ShortHash
toHash = \case
  NameOnly _ -> Nothing
  HashQualified _ sh -> Just sh
  HashOnly sh -> Just sh

-- partial: assumes either a name or hash is provided (or both)
fromNameHash :: Maybe Name -> Maybe ShortHash -> HashQualified Name
fromNameHash n h = case n of
  Just name -> case h of
    Just hash -> HashQualified name hash
    Nothing -> NameOnly name
  Nothing -> case h of
    Just hash -> HashOnly hash
    Nothing -> error "bad HQ construction"

take :: Int -> HashQualified n -> HashQualified n
take i = \case
  n@(NameOnly _) -> n
  HashOnly s -> HashOnly (SH.shortenTo i s)
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.shortenTo i s)

toTextWith :: (n -> Text) -> HashQualified n -> Text
toTextWith f = \case
  NameOnly name -> f name
  HashQualified name hash -> f name <> SH.toText hash
  HashOnly hash -> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromReferent :: Referent -> HashQualified Name
fromReferent = HashOnly . Referent.toShortHash

fromReference :: Reference -> HashQualified Name
fromReference = HashOnly . Reference.toShortHash

fromPattern :: ConstructorReference -> HashQualified Name
fromPattern r = HashOnly $ ConstructorReference.toShortHash r

fromName :: n -> HashQualified n
fromName = NameOnly

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified Name -> Referent -> HashQualified Name
requalify hq r = case hq of
  NameOnly n -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r
  HashOnly _ -> fromReferent r

instance (Name.Alphabetical n) => Name.Alphabetical (HashQualified n) where
  -- Ordered alphabetically, based on the name. Hashes come last.
  compareAlphabetical a b =
    case (toName a, toName b) of
      (Just n, Just n2) -> Name.compareAlphabetical n n2
      (Nothing, Just _) -> GT
      (Just _, Nothing) -> LT
      (Nothing, Nothing) -> EQ
      <> case (toHash a, toHash b) of
        (Nothing, Nothing) -> EQ
        (Nothing, Just _) -> LT -- prefer NameOnly to HashQualified
        (Just _, Nothing) -> GT
        (Just sh, Just sh2) -> compare sh sh2
