module U.Codebase.Preferences
  ( AuthorName,
    PreferencesKey (..),
    mkAuthorName,
    unAuthorName,
  )
where

import Data.Text qualified as Text
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

data PreferencesKey = AuthorNameKey

instance Sqlite.ToField PreferencesKey where
  toField AuthorNameKey = Sqlite.toField ("author.name" :: Text)

mkAuthorName :: Text -> Either Text AuthorName
mkAuthorName name
  | Text.null (Text.strip name) = Left "Author name cannot be empty."
  | Text.length name > 100 = Left "Author name cannot exceed 100 characters."
  | otherwise = Right (AuthorName name)

newtype AuthorName = AuthorName {unAuthorName :: Text}
  deriving stock (Eq, Show)
