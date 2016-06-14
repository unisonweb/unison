module Unison.Parser where

import Control.Applicative
import Control.Monad
import Data.List hiding (takeWhile)
import Data.Maybe
import Prelude hiding (takeWhile)
import qualified Data.Char as Char
import qualified Prelude

newtype Parser a = Parser { run :: String -> Result a }

unsafeRun :: Parser a -> String -> a
unsafeRun p s = case toEither $ run p s of
  Right a -> a
  Left e -> error ("Parse error:\n" ++ e)


string :: String -> Parser String
string s = Parser $ \input ->
  if s `isPrefixOf` input then Succeed s (length s) False
  else Fail [] False

char :: Char -> Parser Char
char c = Parser $ \input ->
  if listToMaybe input == Just c then Succeed c 1 False
  else Fail [] False

one :: (Char -> Bool) -> Parser Char
one f = Parser $ \s -> case s of
  (h:_) | f h -> Succeed h 1 False
  _ -> Fail [] False

identifier :: Parser String
identifier = takeWhile1 (`notElem` "\"\n .[]{}:;()")

constrainedIdentifier :: [String -> Bool] -> Parser String
constrainedIdentifier tests = do
  i <- identifier
  guard (all ($ i) tests)
  pure i

token :: Parser a -> Parser a
token p = p <* many (whitespace1 <|> haskellLineComment)

haskellLineComment :: Parser ()
haskellLineComment = void $ string "--" *> takeWhile (/= '\n')

lineErrorUnless :: String -> Parser a -> Parser a
lineErrorUnless s p = commitFail $ Parser $ \input -> case run p input of
    Fail e b -> Fail (s:m:e) b
      where m = "near \'" ++ Prelude.takeWhile (/= '\n') input ++ "\'"
    ok -> ok

parenthesized :: Parser a -> Parser a
parenthesized p = lp *> body <* rp
  where
    lp = token (char '(')
    body = p
    rp = lineErrorUnless "missing )" $ token (char ')')

takeWhile :: (Char -> Bool) -> Parser String
takeWhile f = Parser $ \s ->
  let hd = Prelude.takeWhile f s
  in Succeed hd (length hd) False

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 f = Parser $ \s ->
  let hd = Prelude.takeWhile f s
  in if null hd then Fail ["takeWhile1 empty: " ++ take 20 s] False
     else Succeed hd (length hd) False

whitespace :: Parser ()
whitespace = void $ takeWhile Char.isSpace

whitespace1 :: Parser ()
whitespace1 = void $ takeWhile1 Char.isSpace

nonempty :: Parser a -> Parser a
nonempty p = Parser $ \s -> case run p s of
  Succeed _ 0 b -> Fail [] b
  ok -> ok

scope :: String -> Parser a -> Parser a
scope s p = Parser $ \input -> case run p input of
  Fail e b -> Fail (s:e) b
  ok -> ok

commitSuccess :: Parser a -> Parser a
commitSuccess p = Parser $ \input -> case run p input of
  Fail e b -> Fail e b
  Succeed a n _ -> Succeed a n True

commitFail :: Parser a -> Parser a
commitFail p = Parser $ \input -> case run p input of
  Fail e _ -> Fail e True
  Succeed a n b -> Succeed a n b

commit' :: Parser ()
commit' = commitSuccess (pure ())

failWith :: String -> Parser a
failWith error = Parser . const $ Fail [error] False

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep pb = f <$> optional (sepBy1 sep pb)
  where
    f Nothing = []
    f (Just l) = l

sepBy1 :: Parser a -> Parser b -> Parser [b]
sepBy1 sep pb = (:) <$> pb <*> many (sep *> pb)

toEither :: Result a -> Either String a
toEither (Fail e _) = Left (intercalate "\n" e)
toEither (Succeed a _ _) = Right a

data Result a
  = Fail [String] Bool
  | Succeed a Int Bool
  deriving (Show)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance Monad Parser where
  return a = Parser $ \_ -> Succeed a 0 False
  Parser p >>= f = Parser $ \s -> case p s of
    Succeed a n committed -> case run (f a) (drop n s) of
      Succeed b m c2 -> Succeed b (n+m) (committed || c2)
      Fail e b -> Fail e (committed || b)
    Fail e b -> Fail e b

instance MonadPlus Parser where
  mzero = Parser $ \_ -> Fail [] False
  mplus p1 p2 = Parser $ \s -> case run p1 s of
    Fail _ False -> run p2 s
    ok -> ok
