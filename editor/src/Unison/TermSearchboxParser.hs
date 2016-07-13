{-# Language OverloadedStrings #-}

module Unison.TermSearchboxParser where

-- todo - convert this module to use predictive, lookahead parser

import Control.Applicative
import Control.Monad
import Data.Maybe
import Prelude hiding (takeWhile)
import Unison.Node.MemNode (V)
import Unison.Parser
import Unison.Term (Term)
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Unison.Term as E
import qualified Unison.Var as Var

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
    , intro ]
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

intro :: Parser [Term V]
intro = do
  let sym = (Var.named . Text.pack <$> token (identifier [])) <|> pure (Var.named "_")
  let lam v = E.lam v E.blank
  let let' v = E.let1 [(v, E.blank)] E.blank
  let letr' v = E.letRec [(v, E.blank)] E.blank
  o <- optional $
    msum [ lam <$> (token (char '\\') *> sym)
         , letr' <$> (token (string "letr") *> sym)
         , letr' <$> (token (string "let rec") *> sym)
         , let' <$> (token (string "let") *> sym) ]
  pure $ case o of
    Nothing -> []
    Just e -> [e]
