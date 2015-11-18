module Unison.Parser where

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe

newtype Parser a = Parser { run :: String -> Result a }

string :: String -> Parser String
string s = Parser $ \input -> if isPrefixOf s input then Succeed s (length s) else Fail [] False

char :: Char -> Parser Char
char c = Parser $ \input -> if listToMaybe input == Just c then Succeed c 1 else Fail [] False

takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = Parser $ \s ->
  let hd = Prelude.takeWhile f s
  in Succeed hd (length hd)

nonempty :: Parser a -> Parser a
nonempty p = Parser $ \s -> case run p s of
  Succeed _ 0 -> Fail [] False
  ok -> ok

scope :: String -> Parser a -> Parser a
scope s p = Parser $ \s -> case run p s of
  Fail e b -> Fail (s:e) b
  ok -> ok

commit :: Parser a -> Parser a
commit p = Parser $ \s -> case run p s of
  Fail e b -> Fail e True
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
    Succeed a n -> run (f a) (drop n s)
    Fail e b -> Fail e b

instance MonadPlus Parser where
  mzero = Parser $ \_ -> Fail [] False
  mplus p1 p2 = Parser $ \s -> case run p1 s of
    Fail e False -> run p2 s
    ok -> ok
