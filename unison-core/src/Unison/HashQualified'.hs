{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified' where

import qualified Data.Text as Text
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import Prelude hiding (take)

data HashQualified' n = NameOnly n | HashQualified n ShortHash
  deriving (Eq, Functor)

type HQSegment = HashQualified' NameSegment

type HashQualified = HashQualified' Name

toHQ :: HashQualified' n -> HQ.HashQualified' n
toHQ = \case
  NameOnly n -> HQ.NameOnly n
  HashQualified n sh -> HQ.HashQualified n sh

fromHQ :: HQ.HashQualified' n -> Maybe (HashQualified' n)
fromHQ = \case
  HQ.NameOnly n -> Just $ NameOnly n
  HQ.HashQualified n sh -> Just $ HashQualified n sh
  HQ.HashOnly {} -> Nothing

-- Like fromHQ, but turns hashes into hash-qualified empty names
fromHQ' :: Monoid n => HQ.HashQualified' n -> HashQualified' n
fromHQ' = \case
  HQ.NameOnly n -> NameOnly n
  HQ.HashQualified n sh -> HashQualified n sh
  HQ.HashOnly h -> HashQualified mempty h

toName :: HashQualified' n -> n
toName = \case
  NameOnly name -> name
  HashQualified name _ -> name

nameLength :: HashQualified' Name -> Int
nameLength = Text.length . toText

take :: Int -> HashQualified' n -> HashQualified' n
take i = \case
  n@(NameOnly _) -> n
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.take i s)

toNameOnly :: HashQualified' n -> HashQualified' n
toNameOnly = fromName . toName

toHash :: HashQualified' n -> Maybe ShortHash
toHash = \case
  NameOnly _ -> Nothing
  HashQualified _ sh -> Just sh

toString :: Show n => HashQualified' n -> String
toString = Text.unpack . toText

-- Parses possibly-hash-qualified into structured type.
fromText :: Text -> Maybe HashQualified
fromText t = case Text.breakOn "#" t of
  (name, "") ->
    Just $ NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
  (name, hash) ->
    HashQualified (Name.unsafeFromText name) <$> SH.fromText hash

unsafeFromText :: Text -> HashQualified
unsafeFromText txt = fromMaybe msg (fromText txt)
  where
    msg = error ("HashQualified'.unsafeFromText " <> show txt)

fromString :: String -> Maybe HashQualified
fromString = fromText . Text.pack

toText :: Show n => HashQualified' n -> Text
toText = \case
  NameOnly name -> Text.pack (show name)
  HashQualified name hash -> Text.pack (show name) <> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified' n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified' n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromName :: n -> HashQualified' n
fromName = NameOnly

matchesNamedReferent :: Eq n => n -> Referent -> HashQualified' n -> Bool
matchesNamedReferent n r = \case
  NameOnly n' -> n' == n
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Referent.toShortHash r

matchesNamedReference :: Eq n => n -> Reference -> HashQualified' n -> Bool
matchesNamedReference n r = \case
  NameOnly n' -> n' == n
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Reference.toShortHash r

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified -> Referent -> HashQualified
requalify hq r = case hq of
  NameOnly n -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r

instance Ord n => Ord (HashQualified' n) where
  compare a b = case compare (toName a) (toName b) of
    EQ -> compare (toHash a) (toHash b)
    o -> o

instance IsString HashQualified where
  fromString = unsafeFromText . Text.pack

instance Show n => Show (HashQualified' n) where
  show = Text.unpack . toText
