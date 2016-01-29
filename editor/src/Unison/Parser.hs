module Unison.Parser where

import Control.Applicative
import Control.Monad
import Data.List hiding (takeWhile)
import Data.Maybe
import Prelude hiding (takeWhile)
import qualified Data.Char as Char
import qualified Prelude

newtype Parser a = Parser { run :: String -> Result a }

string :: String -> Parser String
string s = Parser $ \input -> if isPrefixOf s input then Succeed s (length s) else Fail [] False

char :: Char -> Parser Char
char c = Parser $ \input -> if listToMaybe input == Just c then Succeed c 1 else Fail [] False

one :: (Char -> Bool) -> Parser Char
one f = Parser $ \s -> case s of
  (h:_) | f h -> Succeed h 1
  _ -> Fail [] False

identifier :: Parser String
identifier = nonempty (takeWhile ok) where
  ok '[' = False
  ok ']' = False
  ok '}' = False
  ok '{' = False
  ok '"' = False
  ok ' ' = False
  ok ';' = False
  ok _   = True

token :: Parser a -> Parser a
token p = p <* whitespace

takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = Parser $ \s ->
  let hd = Prelude.takeWhile f s
  in Succeed hd (length hd)

whitespace :: Parser ()
whitespace = void $ takeWhile Char.isSpace

nonempty :: Parser a -> Parser a
nonempty p = Parser $ \s -> case run p s of
  Succeed _ 0 -> Fail [] False
  ok -> ok

scope :: String -> Parser a -> Parser a
scope s p = Parser $ \input -> case run p input of
  Fail e b -> Fail (s:e) b
  ok -> ok

commit :: Parser a -> Parser a
commit p = Parser $ \input -> case run p input of
  Fail e _ -> Fail e True
  ok -> ok

data Result a
  = Fail [String] Bool
  | Succeed a Int

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance Monad Parser where
  return a = Parser $ \_ -> Succeed a 0
  Parser p >>= f = Parser $ \s -> case p s of
    Succeed a n -> case run (f a) (drop n s) of
      Succeed b m -> Succeed b (n+m)
      err -> err
    Fail e b -> Fail e b

instance MonadPlus Parser where
  mzero = Parser $ \_ -> Fail [] False
  mplus p1 p2 = Parser $ \s -> case run p1 s of
    Fail _ False -> run p2 s
    ok -> ok
