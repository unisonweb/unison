module Unison.Syntax.Lexer.Token
  ( Token (..),
    tokenP,
    posP,
  )
where

import Text.Megaparsec (ParsecT, TraversableStream)
import Text.Megaparsec qualified as P
import Unison.Lexer.Pos (Pos (Pos))

data Token a = Token
  { payload :: a,
    start :: !Pos,
    end :: !Pos
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative Token where
  pure a = Token a (Pos 0 0) (Pos 0 0)
  Token f start _ <*> Token a _ end = Token (f a) start end

tokenP :: (Ord e, TraversableStream s) => ParsecT e s m a -> ParsecT e s m (Token a)
tokenP p = do
  start <- posP
  payload <- p
  end <- posP
  pure Token {payload, start, end}

posP :: (Ord e, TraversableStream s) => ParsecT e s m Pos
posP = do
  p <- P.getSourcePos
  pure (Pos (P.unPos (P.sourceLine p)) (P.unPos (P.sourceColumn p)))
