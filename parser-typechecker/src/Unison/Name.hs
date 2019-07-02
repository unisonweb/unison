{-# LANGUAGE OverloadedStrings   #-}

module Unison.Name
  ( Name(..)
  , fromString
  , isPrefixOf
  , joinDot
  , stripNamePrefix
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

-- stripTextPrefix a.b. a.b.c = Just c
-- stripTextPrefix a.b  a.b.c = Just .c;  you probably don't want to do this
-- stripTextPrefix x.y. a.b.c = Nothing
-- stripTextPrefix "" a.b.c = undefined
_stripTextPrefix :: Text -> Name -> Maybe Name
_stripTextPrefix prefix name =
  Name <$> Text.stripPrefix prefix (toText name)

-- stripNamePrefix a.b  a.b.c = Just c
-- stripNamePrefix a.b. a.b.c = undefined, "a.b." isn't a valid name IMO
-- stripNamePrefix x.y  a.b.c = Nothing, x.y isn't a prefix of a.b.c
-- stripNamePrefix "" a.b.c = undefined, "" isn't a valid name IMO
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix prefix name =
  Name <$> Text.stripPrefix (toText prefix <> ".") (toText name)

-- a.b.c.d -> d
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
