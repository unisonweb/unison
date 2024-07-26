{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities related to the parsing and printing of names using the default syntax.
module Unison.Syntax.Name
  ( -- * String conversions
    parseText,
    parseTextEither,
    unsafeParseText,
    toText,
    unsafeParseVar,
    parseVar,
    toVar,

    -- * Name parsers
    nameP,
    relativeNameP,

    -- * Name classifiers
    isSymboly,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as Monad
import Data.List.NonEmpty (pattern (:|))
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
import Unison.NameSegment (NameSegment)
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token)
import Unison.Syntax.NameSegment (segmentStartChar)
import Unison.Syntax.NameSegment qualified as NameSegment
  ( ParseErr,
    isSymboly,
    renderParseErr,
    segmentP,
    toEscapedTextBuilder,
  )
import Unison.Var (Var)
import Unison.Var qualified as Var

------------------------------------------------------------------------------------------------------------------------
-- String conversions

-- | Parse a name from a string literal.
parseText :: Text -> Maybe Name
parseText =
  eitherToMaybe . parseTextEither

-- | Parse a name from a string literal.
parseTextEither :: Text -> Either Text Name
parseTextEither s =
  P.runParser (P.withParsecT (fmap NameSegment.renderParseErr) nameP <* P.eof) "" (Text.unpack s)
    & mapLeft (Text.pack . P.errorBundlePretty)

-- | Unsafely parse a name from a string literal.
unsafeParseText :: (HasCallStack) => Text -> Name
unsafeParseText =
  either (error . Text.unpack) id . parseTextEither

-- | Convert a name to a string representation.
toText :: Name -> Text
toText (Name pos (x0 :| xs)) =
  build (buildPos pos <> foldr step mempty xs <> NameSegment.toEscapedTextBuilder x0)
  where
    step :: NameSegment -> Text.Builder -> Text.Builder
    step x acc =
      acc <> NameSegment.toEscapedTextBuilder x <> "."

    build :: Text.Builder -> Text
    build =
      Text.Lazy.toStrict . Text.Builder.toLazyText

    buildPos :: Position -> Text.Builder
    buildPos = \case
      Absolute -> "."
      Relative -> ""

-- | Parse a name from a var, by first rendering the var as a string.
parseVar :: (Var v) => v -> Maybe Name
parseVar =
  parseText . Var.name

-- | Unsafely parse a name from a var, by first rendering the var as a string.
--
-- See 'unsafeFromText'.
unsafeParseVar :: (Var v) => v -> Name
unsafeParseVar =
  unsafeParseText . Var.name

-- | Convert a name to a string representation, then parse that as a var.
toVar :: (Var v) => Name -> v
toVar =
  Var.named . toText

------------------------------------------------------------------------------------------------------------------------
-- Name parsers

-- | A name parser.
nameP :: (Monad m) => ParsecT (Token NameSegment.ParseErr) [Char] m Name
nameP =
  P.try do
    leadingDot <- isJust <$> P.optional (P.char '.')
    name <- relativeNameP
    pure (if leadingDot then Name.makeAbsolute name else name)

-- | A relative name parser.
relativeNameP :: forall m. (Monad m) => ParsecT (Token NameSegment.ParseErr) [Char] m Name
relativeNameP = do
  Name.fromSegments <$> Monad.sepBy1 NameSegment.segmentP separatorP
  where
    -- The separator between segments is just a dot, but we don't want to commit to parsing another segment unless the
    -- character after the dot can begin a segment.
    --
    -- This allows (for example) the "a." in "forall a. a -> a" to successfully parse as an identifier "a" followed by
    -- the reserved symbol ".", rathern than fail to parse as an identifier, because it looks like the prefix of some
    -- "a.b" that stops in the middle.
    separatorP :: (Ord e) => ParsecT e [Char] m Char
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
