{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified' where

import           Data.Maybe                     ( fromMaybe )
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
import qualified Unison.HashQualified          as HQ

data HashQualified' n = NameOnly n | HashQualified n ShortHash
  deriving (Eq, Ord)

type HashQualified = HashQualified' Name

stripNamespace :: Text -> HashQualified -> HashQualified
stripNamespace namespace hq = case hq of
  NameOnly name         -> NameOnly $ strip name
  HashQualified name sh -> HashQualified (strip name) sh
 where
  strip name =
    fromMaybe name $ Name.stripPrefix (Name.Name $ namespace <> ".") name

toHQ :: HashQualified' n -> HQ.HashQualified' n
toHQ = \case
  NameOnly n -> HQ.NameOnly n
  HashQualified n sh -> HQ.HashQualified n sh

toName :: HashQualified' n -> n
toName = \case
  NameOnly name        ->  name
  HashQualified name _ ->  name

toHash :: HashQualified -> Maybe ShortHash
toHash = \case
  NameOnly _         -> Nothing
  HashQualified _ sh -> Just sh

toString :: Show n => HashQualified' n -> String
toString = Text.unpack . toText

toText :: Show n => HashQualified' n -> Text
toText = \case
  NameOnly name           -> Text.pack (show name)
  HashQualified name hash -> Text.pack (show name) <> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified' n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified' n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromName :: n -> HashQualified' n
fromName = NameOnly

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified -> Referent -> HashQualified
requalify hq r = case hq of
  NameOnly n        -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r

instance Show n => Show (HashQualified' n) where
  show = Text.unpack . toText