module Unison.TermSearchboxParser where

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

term :: Parser [Term V]
term =
  msum
    [ single . E.lit . E.Text . Text.pack <$> quotedString
    , single E.blank <$ char '_'
    , single . E.num <$> floatingPoint
    , [E.vector [E.blank]] <$ (char '[' *> char '_' *> char ']')
    , [E.vector []] <$ (char '[' *> char ']')
    , [E.vector [E.blank]] <$ (char '[' *> char '_')
    , [E.vector [], E.vector [E.blank]] <$ char '['
    , single <$> letRecIntro
    , single <$> letIntro
    , single <$> lambda ]
  where
  single x = [x]

digits :: Parser String
digits = takeWhile Char.isDigit

digits1 :: Parser String
digits1 = (:) <$> one Char.isDigit <*> digits

floatingPoint :: Parser Double
floatingPoint = do
  d <- digits1
  rest <- optional (void (char '.') *> ((++) <$> pure "0." <*> (fromMaybe "0" <$> optional digits1)))
  pure $ read d + fromMaybe 0.0 (read <$> rest)

quotedString :: Parser String
quotedString = char '\"' *> takeWhile (\c -> c /= '\"') <* optional (char '\"')

lambda :: Parser (Term V)
lambda = do
  v <- token identifier <* (optional (char '-') <* optional (char '>'))
  pure $ E.lam' [Text.pack v] E.blank

letIntro :: Parser (Term V)
letIntro = do
  _ <- token (string "let")
  v <- token identifier <* optional (string "=")
  pure $ E.let1' [(Text.pack v, E.blank)] E.blank

letRecIntro :: Parser (Term V)
letRecIntro = do
  -- matches letr, let-rec, let rec
  _ <- token (string "let") *> optional (string "-") *> string "r" *> optional (string "ec")
  v <- token identifier <* optional (string "=")
  pure $ E.let1' [(Text.pack v, E.blank)] E.blank
