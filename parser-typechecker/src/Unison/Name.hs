module Unison.Name (Name(..), unsafeFromText, toString, fromString, toVar, unsafeFromVar) where

import           Data.String (IsString, fromString)
import           Data.Text   (Text)
import qualified Data.Text   as Text
import qualified Unison.Hashable as H
import           Unison.Var  (Var)
import qualified Unison.Var  as Var

newtype Name = Name { toText :: Text } deriving (Eq, Ord)

unsafeFromText :: Text -> Name
unsafeFromText t =
  if Text.any (=='#') t then error $ "not a name: " <> show t
  else Name t

toVar :: Var v => Name -> v
toVar (Name t) = Var.named t

unsafeFromVar :: Var v => v -> Name
unsafeFromVar = unsafeFromText . Var.name

toString :: Name -> String
toString = Text.unpack . toText

instance Show Name where
  show = toString

instance IsString Name where
  fromString = unsafeFromText . Text.pack

instance H.Hashable Name where
  tokens s = [H.Text (toText s)]
