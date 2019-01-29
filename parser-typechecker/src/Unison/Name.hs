module Unison.Name (Name(..), fromText, toString, fromString, toVar, fromVar) where

import           Data.String (IsString, fromString)
import           Data.Text   (Text)
import qualified Data.Text   as Text
import qualified Unison.Hashable as H
import           Unison.Var  (Var)
import qualified Unison.Var  as Var

newtype Name = Name { _toText :: Text } deriving (Eq, Ord)

fromText :: Text -> Name
fromText t =
  if Text.any (=='#') t then error $ "not a name: " <> show t
  else Name t

toVar :: Var v => Name -> v
toVar (Name t) = Var.named t

fromVar :: Var v => v -> Name
fromVar = fromText . Var.name

toString :: Name -> String
toString = Text.unpack . _toText

instance Show Name where
  show = show . _toText

instance IsString Name where
  fromString = fromText . Text.pack

instance H.Hashable Name where
  tokens s = [H.Text (_toText s)]
