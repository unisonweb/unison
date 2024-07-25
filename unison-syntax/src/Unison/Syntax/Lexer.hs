{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This currently contains a mix of general lexing utilities and identifier-y lexers.
module Unison.Syntax.Lexer
  ( Token (..),
    Line,
    Column,
    Err (..),
    Pos (..),
    touches,

    -- * Character classifiers
    wordyIdChar,
    wordyIdStartChar,
    symbolyIdChar,

    -- * new exports
    BlockName,
    Layout,
    ParsingEnv (..),
    P,
    local,
    parseFailure,
    space,
    lit,
    err,
    commitAfter2,
    (<+>),
    some',
    someTill',
    sepBy1',
    separated,
    wordySep,
    identifierP,
    wordyIdSegP,
    shortHashP,
    topBlockName,
    pop,
    typeOrAbilityAlt,
    typeModifiersAlt,
    inc,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.State qualified as S
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as CP
import Text.Megaparsec.Char.Lexer qualified as LP
import Text.Megaparsec.Error qualified as EP
import Text.Megaparsec.Internal qualified as PI
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Lexer.Pos (Column, Line, Pos (Pos), column, line)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Parser.Ann (Annotated (..))
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Lexer.Token (Token (..), posP)
import Unison.Syntax.Name qualified as Name (nameP)
import Unison.Syntax.NameSegment (symbolyIdChar, wordyIdChar, wordyIdStartChar)
import Unison.Syntax.NameSegment qualified as NameSegment (ParseErr (..), wordyP)
import Unison.Syntax.ReservedWords (typeModifiers, typeOrAbility)
import Unison.Syntax.ShortHash qualified as ShortHash (shortHashP)

instance (Annotated a) => Annotated (Cofree f a) where
  ann (a :< _) = ann a

type BlockName = String

type Layout = [(BlockName, Column)]

data ParsingEnv = ParsingEnv
  { -- | layout stack
    layout :: !Layout,
    -- | `Just b` if a block of type `b` is being opened
    opening :: Maybe BlockName,
    -- | are we inside a construct that uses layout?
    inLayout :: Bool,
    -- | Use a stack to remember the parent section and allow docSections within docSections.
    -- - 1 means we are inside a # Heading 1
    parentSections :: [Int],
    -- | 4 means we are inside a list starting at the fourth column
    parentListColumn :: Int
  }
  deriving (Show)

type P = P.ParsecT (Token Err) String (S.State ParsingEnv)

local :: (ParsingEnv -> ParsingEnv) -> P a -> P a
local f p = do
  env0 <- S.get
  S.put (f env0)
  e <- P.observing p
  S.put env0
  case e of
    Left e -> parseFailure e
    Right a -> pure a

parseFailure :: EP.ParseError [Char] (Token Err) -> P a
parseFailure e = PI.ParsecT $ \s _ _ _ eerr -> eerr e s

data Err
  = ReservedWordyId String
  | InvalidSymbolyId String
  | ReservedSymbolyId String
  | InvalidShortHash String
  | InvalidBytesLiteral String
  | InvalidHexLiteral
  | InvalidOctalLiteral
  | Both Err Err
  | MissingFractional String -- ex `1.` rather than `1.04`
  | MissingExponent String -- ex `1e` rather than `1e3`
  | UnknownLexeme
  | TextLiteralMissingClosingQuote String
  | InvalidEscapeCharacter Char
  | LayoutError
  | CloseWithoutMatchingOpen String String -- open, close
  | UnexpectedDelimiter String
  | UnexpectedTokens String -- Catch-all for all other lexer errors, representing some unexpected tokens.
  deriving stock (Eq, Ord, Show) -- richer algebra

space :: P ()
space =
  LP.space
    CP.space1
    (fold <|> LP.skipLineComment "--")
    (LP.skipBlockCommentNested "{-" "-}")
  where
    fold = P.try $ lit "---" *> P.takeRest *> pure ()

lit :: String -> P String
lit = P.try . LP.symbol (pure ())

-- Committed failure
err :: Pos -> Err -> P x
err start t = do
  stop <- posP
  -- This consumes a character and therefore produces committed failure,
  -- so `err s t <|> p2` won't try `p2`
  _ <- void P.anySingle <|> P.eof
  P.customFailure (Token t start stop)

{-
commitAfter :: P a -> (a -> P b) -> P b
commitAfter a f = do
  a <- P.try a
  f a
-}

commitAfter2 :: P a -> P b -> (a -> b -> P c) -> P c
commitAfter2 a b f = do
  (a, b) <- P.try $ liftA2 (,) a b
  f a b

infixl 2 <+>

(<+>) :: (Monoid a) => P a -> P a -> P a
p1 <+> p2 = do a1 <- p1; a2 <- p2; pure (a1 <> a2)

-- | Like `P.some`, but returns an actual `NonEmpty`.
some' :: P a -> P (NonEmpty a)
some' p = liftA2 (:|) p $ many p

-- | Like `P.someTill`, but returns an actual `NonEmpty`.
someTill' :: P a -> P end -> P (NonEmpty a)
someTill' p end = liftA2 (:|) p $ P.manyTill p end

-- | Like `P.sepBy1`, but returns an actual `NonEmpty`.
sepBy1' :: P a -> P sep -> P (NonEmpty a)
sepBy1' p sep = liftA2 (:|) p . many $ sep *> p

separated :: (Char -> Bool) -> P a -> P a
separated ok p = P.try $ p <* P.lookAhead (void (P.satisfy ok) <|> P.eof)

wordySep :: Char -> Bool
wordySep c = isSpace c || not (wordyIdChar c)

-- An identifier is a non-empty dot-delimited list of segments, with an optional leading dot, where each segment is
-- symboly (comprised of only symbols) or wordy (comprised of only alphanums).
--
-- Examples:
--
--   foo
--   .foo.++.doc
--   `.`.`..`     (This is a two-segment identifier without a leading dot: "." then "..")
identifierP :: P (HQ'.HashQualified Name)
identifierP = do
  P.label "identifier (ex: abba1, snake_case, .foo.bar#xyz, .foo.++#xyz, or 🌻)" do
    name <- PI.withParsecT (fmap nameSegmentParseErrToErr) Name.nameP
    P.optional shortHashP <&> \case
      Nothing -> HQ'.fromName name
      Just shorthash -> HQ'.HashQualified name shorthash
  where
    nameSegmentParseErrToErr :: NameSegment.ParseErr -> Err
    nameSegmentParseErrToErr = \case
      NameSegment.ReservedOperator s -> ReservedSymbolyId (Text.unpack s)
      NameSegment.ReservedWord s -> ReservedWordyId (Text.unpack s)

wordyIdSegP :: P NameSegment
wordyIdSegP =
  PI.withParsecT (fmap (ReservedWordyId . Text.unpack)) NameSegment.wordyP

shortHashP :: P ShortHash
shortHashP =
  PI.withParsecT (fmap (InvalidShortHash . Text.unpack)) ShortHash.shortHashP

-- `True` if the tokens are adjacent, with no space separating the two
touches :: Token a -> Token b -> Bool
touches (end -> t) (start -> t2) =
  line t == line t2 && column t == column t2

-- todo: make Layout a NonEmpty
topBlockName :: Layout -> Maybe BlockName
topBlockName [] = Nothing
topBlockName ((name, _) : _) = Just name

pop :: [a] -> [a]
pop = drop 1

typeOrAbilityAlt :: (Alternative f) => (Text -> f a) -> f a
typeOrAbilityAlt f =
  asum $ map f (toList typeOrAbility)

typeModifiersAlt :: (Alternative f) => (Text -> f a) -> f a
typeModifiersAlt f =
  asum $ map f (toList typeModifiers)

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)

instance EP.ShowErrorComponent (Token Err) where
  showErrorComponent (Token err _ _) = go err
    where
      go = \case
        UnexpectedTokens msg -> msg
        CloseWithoutMatchingOpen open close -> "I found a closing " <> close <> " but no matching " <> open <> "."
        Both e1 e2 -> go e1 <> "\n" <> go e2
        LayoutError -> "Indentation error"
        TextLiteralMissingClosingQuote s -> "This text literal missing a closing quote: " <> excerpt s
        e -> show e
      excerpt s = if length s < 15 then s else take 15 s <> "..."
