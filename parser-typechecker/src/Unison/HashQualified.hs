{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified where

import           Data.String      (IsString, fromString)
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Prelude          hiding (take)
import           Unison.Name      (Name)
import qualified Unison.Name      as Name
import           Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import           Unison.Referent  (Referent)
import qualified Unison.Referent  as Referent
import           Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import           Unison.Var       (Var)
import qualified Unison.Var       as Var

data HashQualified
  = NameOnly Name | HashOnly ShortHash | HashQualified Name ShortHash
  deriving (Eq, Ord)

toName :: HashQualified -> Maybe Name
toName = \case
  NameOnly name -> Just name
  HashQualified name _ -> Just name
  HashOnly _ -> Nothing

take :: Int -> HashQualified -> HashQualified
take i = \case
  n@(NameOnly _) -> n
  HashOnly s -> HashOnly (SH.take i s)
  HashQualified n s -> HashQualified n (SH.take i s)

toString :: HashQualified -> String
toString = Text.unpack . toText

fromString :: String -> HashQualified
fromString = fromText . Text.pack

-- Parses possibly-hash-qualified into structured type.
-- Won't crash, but also doesn't validate against base58 or the codebase.
fromText :: Text -> HashQualified
fromText t =
  case Text.breakOn "#" t of
    ("", "")     -> error "don't give me that" -- a hash mark with nothing else
    (name, "")   -> NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
    ("", hash)   -> HashOnly (SH.fromText hash)
    (name, hash) -> HashQualified (Name.unsafeFromText name) (SH.fromText hash)

toText :: HashQualified -> Text
toText = \case
  NameOnly name -> Name.toText name
  HashQualified name hash -> Name.toText name <> SH.toText hash
  HashOnly ref -> Text.pack (show ref)

fromNamedReferent :: Referent -> Name -> HashQualified
fromNamedReferent r n =
  HashQualified n (Referent.toShortHash r)

fromNamedReference :: Reference -> Name -> HashQualified
fromNamedReference r n =
  HashQualified n (Reference.toShortHash r)

fromReferent :: Referent -> HashQualified
fromReferent = HashOnly . Referent.toShortHash

fromReference :: Reference -> HashQualified
fromReference = HashOnly . Reference.toShortHash

-- fromFullReference :: Reference -> HashQualified
-- fromFullReference = HashOnly . Text.pack . (\('#':t) -> t) . show

fromName :: Name -> HashQualified
fromName n = NameOnly n

fromVar :: Var v => v -> HashQualified
fromVar = fromText . Var.name

toVar :: Var v => HashQualified -> v
toVar = Var.named . toText

instance IsString HashQualified where
  fromString = fromText . Text.pack

instance Show HashQualified where
  show = Text.unpack . toText
