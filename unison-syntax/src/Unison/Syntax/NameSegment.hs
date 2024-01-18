-- | Utilities related to the parsing and printing of name segments using the default syntax.
module Unison.Syntax.NameSegment
  ( -- * String conversions
    unsafeFromText,

    -- * Name segment parsers
    symbolyP,

    -- * Character classifiers
    symbolyIdChar,
    reservedSymbolySegments,
  )
where

import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec (ParsecT)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude

------------------------------------------------------------------------------------------------------------------------
-- String conversions

-- | Convert a text to a name segment, when the text is known to be a valid name segment.
unsafeFromText :: Text -> NameSegment
unsafeFromText =
  NameSegment

------------------------------------------------------------------------------------------------------------------------
-- Name segment parsers

-- type P = P.ParsecT (Token Err) String (S.State ParsingEnv)

-- | A symboly name segment parser, which consists only of symboly characters.
--
-- A symboly name segment can optionally be escaped by surrounding it with backticks. Thus, there are two different
-- syntaxes for the ++ operator, for example:
--
--   1. ++
--   2. `++`
--
-- The only difference is that the literal dot character (.) is allowed in escaped segments, but not unescaped segments.
-- Thus, there is only one syntax for the .~ operator:
--
--   1. `.~`
--
-- The backticks of escaped symboly segments are not present in the data itself, i.e. the string "`.~`" corresponds
-- to the data NameSegment ".~".
--
-- Returns @Left@ if the symboly name segment is reserved, e.g. "="
symbolyP :: Ord e => ParsecT e [Char] s (Either Text NameSegment)
symbolyP = do
  string <- unescaped <|> escaped
  let text = Text.pack string
  pure
    if Set.member text reservedSymbolySegments
      then Left text
      else Right (NameSegment text)
  where
    unescaped =
      P.takeWhile1P (Just (description symbolyIdChars)) symbolyIdChar

    escaped = do
      _ <- P.char '`'
      s <- P.takeWhile1P (Just (description escapedSymbolyIdChars)) escapedSymbolyIdChar
      _ <- P.char '`'
      pure s

    description valid =
      "operator (valid characters: " ++ Set.toList valid ++ ")"

------------------------------------------------------------------------------------------------------------------------
-- Character classifiers

symbolyIdChar :: Char -> Bool
symbolyIdChar =
  (`Set.member` symbolyIdChars)

-- | The set of characters allowed in an unescaped symboly identifier.
symbolyIdChars :: Set Char
symbolyIdChars = Set.fromList "!$%^&*-=+<>~\\/|:"

escapedSymbolyIdChar :: Char -> Bool
escapedSymbolyIdChar = (`Set.member` escapedSymbolyIdChars)

-- | The set of characters allowed in an escaped symboly identifier.
escapedSymbolyIdChars :: Set Char
escapedSymbolyIdChars = Set.insert '.' symbolyIdChars

reservedSymbolySegments :: Set Text
reservedSymbolySegments =
  Set.fromList
    [ "=",
      "->",
      ":",
      "&&",
      "||",
      "|",
      "!",
      "'",
      "==>"
    ]
