-- | Utilities related to the parsing and printing of name segments using the default syntax.
module Unison.Syntax.NameSegment
  ( -- * String conversions
    toEscapedText,
    toEscapedTextBuilder,
    parseText,
    unsafeParseText,

    -- * Name segment parsers
    isSymboly,

    -- * Name segment classifiers
    ParseErr (..),
    renderParseErr,
    segmentP,
    symbolyP,
    wordyP,

    -- * Character classifiers
    segmentStartChar,
    symbolyIdChar,
    wordyIdStartChar,
    wordyIdChar,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Text.Megaparsec (ParsecT)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token (..), posP)
import Unison.Syntax.ReservedWords (keywords, reservedOperators)

------------------------------------------------------------------------------------------------------------------------
-- String conversions

-- | Convert a name segment to escaped text, for display purposes.
--
-- > toEscapedText (unsafeFromText ".~") = "`.~`"
toEscapedText :: NameSegment -> Text
toEscapedText segment@(NameSegment text)
  | shouldEscape = "`" <> text <> "`"
  | otherwise = text
  where
    shouldEscape =
      if isSymboly segment
        then isReservedOperator || symbolNeedsEscaping
        else isKeyword
    isKeyword = Set.member text keywords
    isReservedOperator = Set.member text reservedOperators
    symbolNeedsEscaping = not (Text.all symbolyIdChar text)

toEscapedTextBuilder :: NameSegment -> Text.Builder
toEscapedTextBuilder =
  Text.Builder.fromText . toEscapedText

-- | Parse text as a name segment.
--
-- > parseText "foo" = Right (NameSegment "foo")
-- > parseText ".~" = Left ...
-- > parseText "`.~`" = Right (NameSegment ".~")
parseText :: Text -> Either Text NameSegment
parseText text =
  case P.runParser (P.withParsecT (fmap renderParseErr) (segmentP <* P.eof)) "" (Text.unpack text) of
    Left err -> Left (Text.pack (P.errorBundlePretty err))
    Right segment -> Right segment

-- | Parse text as a name segment.
unsafeParseText :: Text -> NameSegment
unsafeParseText =
  either (error . Text.unpack) id . parseText

------------------------------------------------------------------------------------------------------------------------
-- Name segment parsers

data ParseErr
  = ReservedOperator !Text
  | ReservedWord !Text
  deriving stock (Eq, Ord)

renderParseErr :: ParseErr -> Text
renderParseErr = \case
  ReservedOperator s -> "reserved operator: " <> s
  ReservedWord s -> "reserved word: " <> s

segmentP :: Monad m => ParsecT (Token ParseErr) [Char] m NameSegment
segmentP =
  P.withParsecT (fmap ReservedOperator) symbolyP
    <|> P.withParsecT (fmap ReservedWord) wordyP

-- | A symboly name segment parser, which consists only of symboly characters.
--
-- A symboly name segment can optionally be escaped by surrounding it with backticks, which expands the list of allowed
-- symbols to include these three: . ( )
--
-- Throws the parsed name segment as an error if it's unescaped and reserved, e.g. "=".
symbolyP :: ParsecT (Token Text) [Char] m NameSegment
symbolyP = do
  start <- posP
  asum
    [ do
        _ <- P.try (P.lookAhead (P.char '`' *> P.satisfy escapedSymbolyIdChar))
        escapeP (segmentP (description escapedSymbolyIdChars) escapedSymbolyIdChar),
      do
        symbol <- segmentP (description symbolyIdChars) symbolyIdChar
        check start symbol
        pure symbol
    ]
  where
    segmentP name predicate =
      NameSegment . Text.pack <$> P.takeWhile1P (Just name) predicate

    check start (NameSegment symbol) =
      when (Set.member symbol reservedOperators) do
        end <- posP
        P.customFailure (Token symbol start end)

    description valid =
      "operator (valid characters: " ++ Set.toList valid ++ ")"

-- | A wordy name segment parser, which consists only of wordy characters.
--
-- Throws the parsed name segment as an error if it's an unescaped keyword, e.g. "match".
wordyP :: ParsecT (Token Text) [Char] m NameSegment
wordyP = do
  start <- posP
  asum
    [ do
        _ <- P.try (P.lookAhead (P.char '`' *> P.satisfy wordyIdStartChar))
        escapeP unescaped,
      do
        word <- unescaped
        check start word
        pure word
    ]
  where
    unescaped = do
      ch <- P.satisfy wordyIdStartChar
      rest <- P.takeWhileP (Just wordyMsg) wordyIdChar
      pure (NameSegment (Text.pack (ch : rest)))

    check start (NameSegment word) =
      when (Set.member word keywords) do
        end <- posP
        P.customFailure (Token word start end)

    wordyMsg = "identifier (ex: abba1, snake_case, .foo.bar#xyz, or 🌻)"

escapeP :: ParsecT (Token Text) [Char] m a -> ParsecT (Token Text) [Char] m a
escapeP parser =
  P.char '`' *> parser <* P.char '`'

------------------------------------------------------------------------------------------------------------------------
-- Character classifiers

isSymboly :: NameSegment -> Bool
isSymboly =
  not . wordyIdStartChar . Text.head . NameSegment.toUnescapedText

------------------------------------------------------------------------------------------------------------------------
-- Character classifiers

segmentStartChar :: Char -> Bool
segmentStartChar c =
  wordyIdStartChar c || symbolyIdChar c || c == '`' -- backtick starts an escaped segment

symbolyIdChar :: Char -> Bool
symbolyIdChar =
  (`Set.member` symbolyIdChars)

-- | The set of characters allowed in an unescaped symboly identifier.
symbolyIdChars :: Set Char
symbolyIdChars =
  Set.fromList "!$%^&*-=+<>~\\/|:"

escapedSymbolyIdChar :: Char -> Bool
escapedSymbolyIdChar =
  (`Set.member` escapedSymbolyIdChars)

-- | The set of characters allowed in an escaped symboly identifier.
escapedSymbolyIdChars :: Set Char
escapedSymbolyIdChars =
  Set.fromList ".()" <> symbolyIdChars

wordyIdStartChar :: Char -> Bool
wordyIdStartChar ch =
  Char.isAlpha ch || isEmoji ch || ch == '_'

wordyIdChar :: Char -> Bool
wordyIdChar ch =
  Char.isAlphaNum ch || isEmoji ch || ch == '_' || ch == '!' || ch == '\''

isEmoji :: Char -> Bool
isEmoji c =
  c >= '\x1F300' && c <= '\x1FAFF'
