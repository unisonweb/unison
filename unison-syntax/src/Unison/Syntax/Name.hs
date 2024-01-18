{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities related to the parsing and printing of names using the default syntax.
module Unison.Syntax.Name
  ( -- * String conversions
    unsafeFromString,
    toString,
    fromText,
    fromTextEither,
    unsafeFromText,
    toText,
    unsafeFromVar,
    toVar,

    -- * Name parsers
    ParseErr (..),
    nameP,

    -- * Name classifiers
    isSymboly,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as Monad
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Text.Megaparsec (ParsecT)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.Name qualified as Name (fromSegments, lastSegment, makeAbsolute)
import Unison.Name.Internal (Name (Name))
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.NameSegment qualified as NameSegment
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token)
import Unison.Syntax.NameSegment (segmentStartChar)
import Unison.Syntax.NameSegment qualified as NameSegment (isSymboly, symbolyP, wordyP)
import Unison.Var (Var)
import Unison.Var qualified as Var

------------------------------------------------------------------------------------------------------------------------
-- String conversions

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
toVar :: (Var v) => Name -> v
toVar =
  Var.named . toText

-- | Parse a name from a string literal.
--
-- Performs very minor validation (a name can't be empty, nor contain a '#' character [at least currently?]) but makes
-- no attempt at rejecting bogus names like "foo...bar...baz".
fromText :: Text -> Maybe Name
fromText =
  eitherToMaybe . fromTextEither

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
unsafeFromText :: (HasCallStack) => Text -> Name
unsafeFromText =
  either (error . Text.unpack) id . fromTextEither

-- | Unsafely parse a name from a var, by first rendering the var as a string.
--
-- See 'unsafeFromText'.
unsafeFromVar :: (Var v) => v -> Name
unsafeFromVar =
  unsafeFromText . Var.name

------------------------------------------------------------------------------------------------------------------------
-- Name parsers

data ParseErr
  = ReservedOperator !Text
  | ReservedWord !Text
  deriving stock (Eq, Ord)

nameP :: forall m. Monad m => ParsecT (Token ParseErr) [Char] m Name
nameP =
  P.try do
    leadingDot <- isJust <$> P.optional (P.char '.')
    name <- Name.fromSegments <$> Monad.sepBy1 segmentP separatorP
    pure (if leadingDot then Name.makeAbsolute name else name)
  where
    segmentP :: ParsecT (Token ParseErr) [Char] m NameSegment
    segmentP =
      P.withParsecT (fmap ReservedOperator) NameSegment.symbolyP
        <|> P.withParsecT (fmap ReservedWord) NameSegment.wordyP

    -- The separator between segments is just a dot, but we don't want to commit to parsing another segment unless the
    -- character after the dot can begin a segment.
    --
    -- This allows (for example) the "a." in "forall a. a -> a" to successfully parse as an identifier "a" followed by
    -- the reserved symbol ".", rathern than fail to parse as an identifier, because it looks like the prefix of some
    -- "a.b" that stops in the middle.
    separatorP :: Ord e => ParsecT e [Char] m Char
    separatorP =
      P.try do
        c <- P.char '.'
        P.lookAhead (P.satisfy segmentStartChar)
        pure c

------------------------------------------------------------------------------------------------------------------------
-- Name classifiers

isSymboly :: Name -> Bool
isSymboly =
  NameSegment.isSymboly . Name.lastSegment
