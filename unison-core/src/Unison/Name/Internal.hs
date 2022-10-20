-- | The private Unison.Name innards. Prefer importing Unison.Name instead, unless you need the data constructor of
-- Name.
module Unison.Name.Internal
  ( Name (..),
    toText,
    fromTextEither,
    unsafeFromString,
    unsafeFromText,
  )
where

import Data.List.NonEmpty (pattern (:|))
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical, compareAlphabetical)

-- | A name is an absolute-or-relative non-empty list of name segments.
data Name
  = -- A few example names:
    --
    --   "foo.bar"  --> Name Relative ["bar", "foo"]
    --   ".foo.bar" --> Name Absolute ["bar", "foo"]
    --   "|>.<|"    --> Name Relative ["<|", "|>"]
    --   "."        --> Name Relative ["."]
    --   ".."       --> Name Absolute ["."]
    --
    Name
      -- whether the name is positioned absolutely (to some arbitrary root namespace), or relatively
      Position
      -- the name segments in reverse order
      (List.NonEmpty NameSegment)
  deriving stock (Eq, Generic)

instance Alphabetical Name where
  compareAlphabetical n1 n2 =
    compareAlphabetical (toText n1) (toText n2)

instance IsString Name where
  fromString =
    unsafeFromString

instance Ord Name where
  compare (Name p0 ss0) (Name p1 ss1) =
    compare ss0 ss1 <> compare p0 p1

instance Show Name where
  show =
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
