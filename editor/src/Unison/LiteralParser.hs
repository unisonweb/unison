module Unison.LiteralParser where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Prelude hiding (takeWhile)
import Unison.Node.MemNode (V)
import Unison.Parser
import Unison.Term (Term)
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Unison.Term as E

term :: Parser (Term V)
term =
  msum [ E.lit . E.Text . Text.pack <$> quotedString
       , E.blank <$ char '_'
       , E.num <$> floatingPoint ]

digits :: Parser String
digits = takeWhile Char.isDigit

floatingPoint :: Parser Double
floatingPoint = do
  d <- digits
  rest <- optional (void (char '.') *> digits)
  pure $ read d + fromMaybe 0.0 (read <$> rest)

quotedString :: Parser String
quotedString = char '\"' *> takeWhile (\c -> c /= '\"') <* optional (char '\"')

