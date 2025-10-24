module U.Codebase.Preferences
  ( AuthorName,
    PreferencesKey (..),
    allKeys,
    mkAuthorName,
    unAuthorName,
    keyToText,
    keyFromText,
    allKeysText,
  )
where

import Data.Text qualified as Text
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

data PreferencesKey = AuthorNameKey
  deriving stock (Eq, Enum, Bounded)

instance Show PreferencesKey where
  show k = Text.unpack . keyToText $ k

allKeys :: [PreferencesKey]
allKeys = [minBound .. maxBound]

allKeysText :: [Text]
allKeysText = keyToText <$> allKeys

keyToText :: PreferencesKey -> Text
keyToText = \case
  AuthorNameKey -> "author.name"

keyFromText :: Text -> Maybe PreferencesKey
keyFromText t = case t of
  "author.name" -> Just AuthorNameKey
  _ -> Nothing

instance Sqlite.ToField PreferencesKey where
  toField AuthorNameKey = Sqlite.toField (keyToText AuthorNameKey)

mkAuthorName :: Text -> Either Text AuthorName
mkAuthorName name
  | Text.null (Text.strip name) = Left "Author name cannot be empty."
  | Text.length name > 100 = Left "Author name cannot exceed 100 characters."
  | otherwise = Right (AuthorName name)

newtype AuthorName = AuthorName {unAuthorName :: Text}
  deriving stock (Eq, Show)
  deriving newtype (Sqlite.ToField, Sqlite.FromField)
