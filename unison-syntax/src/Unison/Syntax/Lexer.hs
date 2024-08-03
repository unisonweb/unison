-- | This currently contains a mix of general lexing utilities and identifier-y lexers.
module Unison.Syntax.Lexer
  ( Token (..),
    Line,
    Column,
    Pos (..),
    touches,

    -- * Character classifiers
    wordyIdChar,
    wordyIdStartChar,
    symbolyIdChar,

    -- * other utils
    local,
    space,
    lit,
    commitAfter2,
    (<+>),
    some',
    someTill',
    sepBy1',
    separated,
    wordySep,
    pop,
    typeOrAbilityAlt,
    inc,
  )
where

import Control.Monad.State qualified as S
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as CP
import Text.Megaparsec.Char.Lexer qualified as LP
import Unison.Lexer.Pos (Column, Line, Pos (Pos), column, line)
import Unison.Prelude
import Unison.Syntax.Lexer.Token (Token (..))
import Unison.Syntax.NameSegment (symbolyIdChar, wordyIdChar, wordyIdStartChar)
import Unison.Syntax.ReservedWords (typeOrAbility)

local :: (P.MonadParsec e s' m, S.MonadState s m) => (s -> s) -> m a -> m a
local f p = do
  env0 <- S.get
  S.put (f env0)
  e <- P.observing p
  S.put env0
  case e of
    Left e -> P.parseError e
    Right a -> pure a

space :: (P.MonadParsec e String m) => m ()
space =
  LP.space
    CP.space1
    (fold <|> LP.skipLineComment "--")
    (LP.skipBlockCommentNested "{-" "-}")
  where
    fold = P.try $ lit "---" *> P.takeRest *> pure ()

lit :: (P.MonadParsec e String m) => String -> m String
lit = P.try . LP.symbol (pure ())

commitAfter2 :: (P.MonadParsec e s m) => m a -> m b -> (a -> b -> m c) -> m c
commitAfter2 a b f = do
  (a, b) <- P.try $ liftA2 (,) a b
  f a b

infixl 2 <+>

(<+>) :: (Applicative f, Monoid a) => f a -> f a -> f a
(<+>) = liftA2 (<>)

-- | Like `P.some`, but returns an actual `NonEmpty`.
some' :: (P.MonadParsec e s m) => m a -> m (NonEmpty a)
some' p = liftA2 (:|) p $ many p

-- | Like `P.someTill`, but returns an actual `NonEmpty`.
someTill' :: (P.MonadParsec e s m) => m a -> m end -> m (NonEmpty a)
someTill' p end = liftA2 (:|) p $ P.manyTill p end

-- | Like `P.sepBy1`, but returns an actual `NonEmpty`.
sepBy1' :: (P.MonadParsec e s m) => m a -> m sep -> m (NonEmpty a)
sepBy1' p sep = liftA2 (:|) p . many $ sep *> p

separated :: (P.MonadParsec e s m) => (P.Token s -> Bool) -> m a -> m a
separated ok p = P.try $ p <* P.lookAhead (void (P.satisfy ok) <|> P.eof)

wordySep :: Char -> Bool
wordySep c = isSpace c || not (wordyIdChar c)

-- `True` if the tokens are adjacent, with no space separating the two
touches :: Token a -> Token b -> Bool
touches (end -> t) (start -> t2) =
  line t == line t2 && column t == column t2

pop :: [a] -> [a]
pop = drop 1

typeOrAbilityAlt :: (Alternative f) => (Text -> f a) -> f a
typeOrAbilityAlt f =
  asum $ map f (toList typeOrAbility)

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)
