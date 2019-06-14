{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified where

import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Prelude                 hiding ( take )
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.ShortHash               ( ShortHash )
import qualified Unison.ShortHash              as SH
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var

data HashQualified' n
  = NameOnly n | HashOnly ShortHash | HashQualified n ShortHash
  deriving (Eq, Ord)

type HashQualified = HashQualified' Name

stripNamespace :: Text -> HashQualified -> HashQualified
stripNamespace namespace hq = case hq of
  NameOnly name         -> NameOnly $ strip name
  HashQualified name sh -> HashQualified (strip name) sh
  ho                    -> ho
 where
  strip name =
    fromMaybe name $ Name.stripPrefix (Name.Name $ namespace <> ".") name

toName :: HashQualified' n -> Maybe n
toName = \case
  NameOnly name        -> Just name
  HashQualified name _ -> Just name
  HashOnly _           -> Nothing

hasName :: HashQualified -> Bool
hasName = isJust . toName

toHash :: HashQualified -> Maybe ShortHash
toHash = \case
  NameOnly _         -> Nothing
  HashQualified _ sh -> Just sh
  HashOnly sh        -> Just sh

-- partial: assumes either a name or hash is provided (or both)
fromNameHash :: Maybe Name -> Maybe ShortHash -> HashQualified
fromNameHash n h = case n of
  Just name -> case h of
    Just hash -> HashQualified name hash
    Nothing -> NameOnly name
  Nothing -> case h of
    Just hash -> HashOnly hash
    Nothing -> error "bad HQ construction"

take :: Int -> HashQualified' n -> HashQualified' n
take i = \case
  n@(NameOnly _)    -> n
  HashOnly s        -> HashOnly (SH.take i s)
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.take i s)

toString :: Show n => HashQualified' n -> String
toString = Text.unpack . toText

fromString :: String -> HashQualified
fromString = fromText . Text.pack

-- Parses possibly-hash-qualified into structured type.
-- Won't crash, but also doesn't validate against base58 or the codebase.
fromText :: Text -> HashQualified
fromText t = case Text.breakOn "#" t of
  (name, ""  ) -> NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
  (""  , hash) -> HashOnly (SH.unsafeFromText hash)   -- safe bc breakOn #
  (name, hash) ->
    HashQualified (Name.unsafeFromText name) (SH.unsafeFromText hash)

toText :: Show n => HashQualified' n -> Text
toText = \case
  NameOnly name           -> Text.pack (show name)
  HashQualified name hash -> Text.pack (show name) <> SH.toText hash
  HashOnly ref            -> Text.pack (show ref)

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified' n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified' n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromReferent :: Referent -> HashQualified
fromReferent = HashOnly . Referent.toShortHash

fromReference :: Reference -> HashQualified
fromReference = HashOnly . Reference.toShortHash

fromName :: n -> HashQualified' n
fromName = NameOnly

fromVar :: Var v => v -> HashQualified
fromVar = fromText . Var.name

toVar :: Var v => HashQualified -> v
toVar = Var.named . toText

-- todo: find this logic elsewhere and replace with call to this
matchesNamedReferent :: Name -> Referent -> HashQualified -> Bool
matchesNamedReferent n r = \case
  NameOnly n' -> n' == n
  HashOnly sh -> sh `SH.isPrefixOf` Referent.toShortHash r
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Referent.toShortHash r

matchesNamedReference :: Name -> Reference -> HashQualified -> Bool
matchesNamedReference n r = \case
  NameOnly n' -> n' == n
  HashOnly sh -> sh `SH.isPrefixOf` Reference.toShortHash r
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Reference.toShortHash r

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified -> Referent -> HashQualified
requalify hq r = case hq of
  NameOnly n        -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r
  HashOnly _        -> fromReferent r

instance IsString HashQualified where
  fromString = fromText . Text.pack

instance Show n => Show (HashQualified' n) where
  show = Text.unpack . toText
