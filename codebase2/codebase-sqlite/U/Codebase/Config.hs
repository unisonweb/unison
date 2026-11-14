module U.Codebase.Config
  ( AuthorName,
    ConfigKey (..),
    allKeys,
    mkAuthorName,
    unAuthorName,
    keyToText,
    keyFromText,
    allKeysText,
    commandAliasesKeyPrefix,
    parseCommandAliases,
  )
where

import Data.Char (isSpace)
import Data.Text qualified as Text
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

data ConfigKey
  = AuthorNameKey
  | -- | Extra names (aliases) for the UCM command with this name, e.g. @alias.namespace@.
    CommandAliasesKey Text
  deriving stock (Eq, Ord)

instance Show ConfigKey where
  show k = Text.unpack . keyToText $ k

-- | All of the config keys that don't take a parameter.
allKeys :: [ConfigKey]
allKeys = [AuthorNameKey]

-- | Descriptions of all of the config keys, for help and error messages.
allKeysText :: [Text]
allKeysText = (keyToText <$> allKeys) ++ [commandAliasesKeyPrefix <> "<command>"]

-- | The prefix of a 'CommandAliasesKey'.
commandAliasesKeyPrefix :: Text
commandAliasesKeyPrefix = "alias."

keyToText :: ConfigKey -> Text
keyToText = \case
  AuthorNameKey -> "author.name"
  CommandAliasesKey command -> commandAliasesKeyPrefix <> command

keyFromText :: Text -> Maybe ConfigKey
keyFromText t = case t of
  "author.name" -> Just AuthorNameKey
  _
    | Just command <- Text.stripPrefix commandAliasesKeyPrefix t,
      not (Text.null command) ->
        Just (CommandAliasesKey command)
    | otherwise -> Nothing

-- | Parse the value of a 'CommandAliasesKey': a list of aliases separated by spaces and/or commas.
parseCommandAliases :: Text -> [Text]
parseCommandAliases =
  filter (not . Text.null) . Text.split (\c -> c == ',' || isSpace c)

instance Sqlite.ToField ConfigKey where
  toField = Sqlite.toField . keyToText

mkAuthorName :: Text -> Either Text AuthorName
mkAuthorName name
  | Text.null (Text.strip name) = Left "Author name cannot be empty."
  | Text.length name > 100 = Left "Author name cannot exceed 100 characters."
  | otherwise = Right (AuthorName name)

newtype AuthorName = AuthorName {unAuthorName :: Text}
  deriving stock (Eq, Show)
  deriving newtype (Sqlite.ToField, Sqlite.FromField)
