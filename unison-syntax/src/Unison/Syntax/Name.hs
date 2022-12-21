{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for Name (to/from string types).
module Unison.Syntax.Name
  ( fromText,
    fromTextEither,
    unsafeFromString,
    unsafeFromText,
    unsafeFromVar,
    toString,
    toText,
    toVar,
  )
where

import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Unison.Name.Internal (Name (Name))
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Var (Var)
import qualified Unison.Var as Var

instance IsString Name where
  fromString =
    unsafeFromString

-- | Convert a name to a string representation.
toString :: Name -> String
toString =
  Text.unpack . toText

-- | Convert a name to a string representation.
toText :: Name -> Text
toText (Name pos (x0 :| xs)) =
  build (buildPos pos <> foldr step mempty xs <> NameSegment.toTextBuilder x0)
  where
    step :: NameSegment -> Text.Builder -> Text.Builder
    step x acc =
      acc <> NameSegment.toTextBuilder x <> "."

    build :: Text.Builder -> Text
    build =
      Text.Lazy.toStrict . Text.Builder.toLazyText

    buildPos :: Position -> Text.Builder
    buildPos = \case
      Absolute -> "."
      Relative -> ""

-- | Convert a name to a string representation, then parse that as a var.
toVar :: Var v => Name -> v
toVar =
  Var.named . toText

-- | Parse a name from a string literal.
--
-- Performs very minor validation (a name can't be empty, nor contain a '#' character [at least currently?]) but makes
-- no attempt at rejecting bogus names like "foo...bar...baz".
fromText :: Text -> Maybe Name
fromText = eitherToMaybe . fromTextEither

-- | Parse a name from a string literal.
--
-- Performs very minor validation (a name can't be empty, nor contain a '#' character [at least currently?]) but makes
-- no attempt at rejecting bogus names like "foo...bar...baz".
fromTextEither :: Text -> Either Text Name
fromTextEither = \case
  "" -> Left "empty name"
  "." -> Right $ Name Relative ("." :| [])
  ".." -> Right $ Name Absolute ("." :| [])
  name
    | Text.any (== '#') name -> Left ("not a name: " <> tShow name)
    | Text.head name == '.' -> Name Absolute <$> (go (Text.tail name))
    | otherwise -> Name Relative <$> go name
  where
    go :: Text -> Either Text (List.NonEmpty NameSegment)
    go name =
      if ".." `Text.isSuffixOf` name
        then Right $ "." :| split (Text.dropEnd 2 name)
        else case split name of
          [] -> Left "empty name"
          s : ss -> Right $ s :| ss

    split :: Text -> [NameSegment]
    split =
      reverse . map NameSegment . Text.split (== '.')

-- | Unsafely parse a name from a string literal.
-- See 'unsafeFromText'.
unsafeFromString :: String -> Name
unsafeFromString =
  unsafeFromText . Text.pack

-- | Unsafely parse a name from a string literal.
--
-- Performs very minor validation (a name can't be empty, nor contain a '#' character [at least currently?]) but makes
-- no attempt at rejecting bogus names like "foo...bar...baz".
unsafeFromText :: HasCallStack => Text -> Name
unsafeFromText = either (error . Text.unpack) id . fromTextEither

-- | Unsafely parse a name from a var, by first rendering the var as a string.
--
-- See 'unsafeFromText'.
unsafeFromVar :: Var v => v -> Name
unsafeFromVar =
  unsafeFromText . Var.name
