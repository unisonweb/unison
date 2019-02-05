{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified where

import           Data.String        (IsString, fromString)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Unison.Name        (Name)
import qualified Unison.Name        as Name
import           Unison.Reference   (Reference)
import qualified Unison.Reference   as Reference
import           Unison.Referent    (Referent)
import qualified Unison.Referent    as Referent
import           Unison.Var  (Var)
import qualified Unison.Var  as Var

data HashQualified
  -- todo: Let HashOnly take Reference instead of Text?
  --       This requires being able to parse a Reference in
  = NameOnly Name | HashOnly Referent | HashQualified Name Text
  deriving (Eq, Ord)

toName :: HashQualified -> Maybe Name
toName = \case
  NameOnly name -> Just name
  HashQualified name _ -> Just name
  HashOnly _ -> Nothing

hashSeparator :: Text
hashSeparator = "#"

toString :: HashQualified -> String
toString = Text.unpack . toText

fromString :: String -> HashQualified
fromString = fromText . Text.pack

-- parses possibly-hash-qualified into structured type
fromText :: Text -> HashQualified
fromText t =
  case Text.breakOn hashSeparator t of
    ("", "")     -> error "don't give me that"
    (name, "")   -> NameOnly (Name.unsafeFromText name)
    ("", hash)   -> HashOnly (Referent.unsafeFromText hash)
    (name, hash) -> HashQualified (Name.unsafeFromText name) hash

toText :: HashQualified -> Text
toText = \case
  NameOnly name -> Name.toText name
  HashQualified name hash -> Name.toText name <> hash
  HashOnly ref -> Text.pack (show ref)

forReferent :: Referent -> Int -> Name -> HashQualified
forReferent r len n =
  HashQualified n . Text.pack $ Referent.showShort len r

forReference :: Reference -> Int -> Name -> HashQualified
forReference r len n =
  HashQualified n . Text.pack $ Reference.showShort len r

fromReferent :: Referent -> HashQualified
fromReferent = HashOnly

fromReference :: Reference -> HashQualified
fromReference = HashOnly . Referent.Ref

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
