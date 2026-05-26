{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities related to the parsing and printing of names using the default syntax.
module Unison.Syntax.Name
  ( -- * String conversions
    parseText,
    parseTextEither,
    unsafeParseText,
    toText,
    toTextParens,
    unsafeParseVar,
    parseVar,
    toVar,

    -- * Name parsers
    nameP,
    relativeNameP,

    -- * Escaping helpers
    escapeReservedSegments,

    -- * Name classifiers
    isSymboly,
  )
where

import Control.Monad.Combinators.NonEmpty qualified as Monad
import Data.List.NonEmpty (pattern (:|))
import Data.Set qualified as Set
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
import Unison.Syntax.ReservedWords qualified as ReservedWords
import Unison.Var (Var)
import Unison.Var qualified as Var

------------------------------------------------------------------------------------------------------------------------
-- String conversions

-- | Parse a name from a string literal. Reserved-word segments
-- (e.g. @class@, @given@) are accepted unescaped: this entry point
-- is used to deserialize names from the codebase / namespace, where
-- the strict source-code keyword reservation does not apply.
parseText :: Text -> Maybe Name
parseText =
  eitherToMaybe . parseTextEither

-- | Parse a name from a string literal.
parseTextEither :: Text -> Either Text Name
parseTextEither s =
  let attempt t =
        P.runParser (P.withParsecT (fmap NameSegment.renderParseErr) nameP <* P.eof) "" (Text.unpack t)
          & mapLeft (Text.pack . P.errorBundlePretty)
   in case attempt s of
        Right name -> Right name
        Left err ->
          -- Retry with reserved-word segments escaped by backticks.
          -- A name stored in the namespace can have a segment that
          -- collides with a Unison keyword (e.g. a definition called
          -- @class@). The source-code parser rejects bare reserved
          -- words, but a serialized name is not source code.
          let escaped = escapeReservedSegments s
           in if escaped == s then Left err else attempt escaped

-- | Wrap every '.'-separated segment that matches a Unison keyword in
-- backticks, leaving non-keyword segments and segment separators
-- untouched. Idempotent on already-escaped input (already-backticked
-- segments contain no bare keyword).
escapeReservedSegments :: Text -> Text
escapeReservedSegments t =
  Text.intercalate "." (map escapeSeg (Text.splitOn "." t))
  where
    escapeSeg s
      | Set.member s ReservedWords.keywords = "`" <> s <> "`"
      | otherwise = s

-- | Unsafely parse a name from a string literal.
unsafeParseText :: (HasCallStack) => Text -> Name
unsafeParseText =
  either (error . Text.unpack) id . parseTextEither

-- | Convert a name to a string representation.
toText :: Name -> Text
toText =
  toText1 False

-- | Like 'toText', but surrounds symboly names with parens.
toTextParens :: Name -> Text
toTextParens =
  toText1 True

toText1 :: Bool -> Name -> Text
toText1 parensIfSymboly (Name pos (x0 :| xs)) =
  (foldr step prefix xs <> NameSegment.toEscapedTextBuilder x0 <> suffix)
    & Text.Builder.toLazyText
    & Text.Lazy.toStrict
  where
    step :: NameSegment -> Text.Builder -> Text.Builder
    step x acc =
      acc <> NameSegment.toEscapedTextBuilder x <> "."

    parens :: Bool
    parens =
      parensIfSymboly && NameSegment.isSymboly x0

    prefix :: Text.Builder
    prefix =
      case (pos, parens) of
        (Absolute, False) -> "."
        (Absolute, True) -> "(."
        (Relative, False) -> mempty
        (Relative, True) -> "("

    suffix :: Text.Builder
    suffix =
      case parens of
        False -> mempty
        True -> ")"

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
