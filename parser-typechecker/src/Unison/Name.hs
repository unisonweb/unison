{-# LANGUAGE OverloadedStrings   #-}

module Unison.Name
  ( Name(..)
  , fromString
  , isPrefixOf
  , joinDot
  , stripPrefix
  , stripPrefixes
  , toString
  , toVar
  , unqualified
  , unqualified'
  , unsafeFromText
  , fromVar
  )
where

import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var

newtype Name = Name { toText :: Text } deriving (Eq, Ord)

unsafeFromText :: Text -> Name
unsafeFromText t =
  if Text.any (== '#') t then error $ "not a name: " <> show t else Name t

toVar :: Var v => Name -> v
toVar (Name t) = Var.named t

fromVar :: Var v => v -> Name
fromVar = unsafeFromText . Var.name

toString :: Name -> String
toString = Text.unpack . toText

isPrefixOf :: Name -> Name -> Bool
a `isPrefixOf` b = toText a `Text.isPrefixOf` toText b

stripPrefix :: Name -> Name -> Maybe Name
stripPrefix prefix name =
  Name <$> Text.stripPrefix (toText prefix) (toText name)

stripPrefixes :: Name -> Name
stripPrefixes = unsafeFromText . last . Text.splitOn "." . toText

joinDot :: Name -> Name -> Name
joinDot n1 n2 = Name $ toText n1 <> "." <> toText n2

unqualified :: Name -> Name
unqualified = unsafeFromText . unqualified' . toText

unqualified' :: Text -> Text
unqualified' = last . Text.splitOn "."

instance Show Name where
  show = toString

instance IsString Name where
  fromString = unsafeFromText . Text.pack

instance H.Hashable Name where
  tokens s = [H.Text (toText s)]
