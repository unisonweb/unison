module Unison.Syntax.Lexer.Token
  ( Token (..),
    tokenP,
    posP,
  )
where

import Data.Text qualified as Text
import Text.Megaparsec (MonadParsec, TraversableStream)
import Text.Megaparsec qualified as P
import Unison.Lexer.Pos (Pos (Pos))
import Unison.Parser.Ann (Ann (Ann), Annotated (..))
import Unison.Prelude

data Token a = Token
  { payload :: a,
    start :: !Pos,
    end :: !Pos
  }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Annotated (Token a) where
  ann (Token _ s e) = Ann s e

instance Applicative Token where
  pure a = Token a (Pos 0 0) (Pos 0 0)
  Token f start _ <*> Token a _ end = Token (f a) start end

-- This instance is odd, but useful.
--
-- The lexer prefers to throw custom errors as `Token Err`. It also calls out to other parsers (like the name segment
-- parser) that don't know about `Err`, but throw custom errors as `Token Something` for the lexer to inject into
-- `Token Err`.
--
-- ...then there are yet more callers of these other parsers that don't want an annoying `Token Something`, they just
-- want a simple string error message.
--
-- So, the flow aided by this instance is roughly:
--
--   1. Run some parser, using `withParsecT` as necessary to unify the potentially-different `Token Something` errors
--      as a `Token Text`.
--   2. `prettyErrorBundle` that thing.
instance P.ShowErrorComponent (Token Text) where
  showErrorComponent = Text.unpack . payload

tokenP :: (Ord e, TraversableStream s, MonadParsec e s m) => m a -> m (Token a)
tokenP p = do
  start <- posP
  payload <- p
  end <- posP
  pure Token {payload, start, end}

posP :: (Ord e, TraversableStream s, MonadParsec e s m) => m Pos
posP = do
  p <- P.getSourcePos
  pure (Pos (P.unPos (P.sourceLine p)) (P.unPos (P.sourceColumn p)))
