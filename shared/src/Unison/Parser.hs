{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}

module Unison.Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.List hiding (takeWhile)
import Data.Maybe
import Prelude hiding (takeWhile)
import qualified Data.Char as Char
import qualified Prelude
import Debug.Trace

type InLayout = Bool
newtype Parser a = Parser { run' :: (String,InLayout) -> Result a }

root :: Parser a -> Parser a
root p = ignored *> (p <* (optional semicolon <* eof))

semicolon :: Parser ()
semicolon = void $ token (char ';')

semicolon2 :: Parser ()
semicolon2 = semicolon *> semicolon

eof :: Parser ()
eof = Parser $ \(s,_) -> case s of
  [] -> Succeed () 0 False
  _ -> Fail [Prelude.takeWhile (/= '\n') s, "expected eof, got"] False

attempt :: Parser a -> Parser a
attempt p = Parser $ \s -> case run' p s of
   Fail stack _ -> Fail stack False
   Succeed a n _ -> Succeed a n False

run :: Parser a -> String -> Result a
-- run p s = run' p (watch "layoutized" $ layoutize s, False)
run p s = run' p (s, False)
  where watch msg a = trace (msg ++ ":\n" ++ a) a

unsafeRun :: Parser a -> String -> a
unsafeRun p s = case toEither $ run p s of
  Right a -> a
  Left e -> error ("Parse error:\n" ++ e)

unsafeGetSucceed :: Result a -> a
unsafeGetSucceed r = case r of
  Succeed a _ _ -> a
  Fail e _ -> error (unlines ("Parse error:":e))

string :: String -> Parser String
string s = Parser $ \(input,_) ->
  if s `isPrefixOf` input then Succeed s (length s) False
  else Fail ["expected '" ++ s ++ "', got " ++ takeLine input] False

takeLine :: String -> String
takeLine = Prelude.takeWhile (/= '\n')

char :: Char -> Parser Char
char c = Parser $ \(input,_) ->
  if listToMaybe input == Just c then Succeed c 1 False
  else Fail ["expected " ++ show c ++ " near " ++ takeLine input] False

one :: (Char -> Bool) -> Parser Char
one f = Parser $ \(s,_) -> case s of
  (h:_) | f h -> Succeed h 1 False
  _ -> Fail [] False

base64string' :: String -> Parser String
base64string' alphabet = concat <$> many base64group
  where
    base64group :: Parser String
    base64group = do
      chars <- some $ one (`elem` alphabet)
      padding <- sequenceA (replicate (padCount $ length chars) (char '='))
      return $ chars ++ padding
    padCount :: Int -> Int
    padCount len = case len `mod` 4 of 0 -> 0; n -> 4 - n

base64urlstring :: Parser String
base64urlstring = base64string' $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"

notReservedChar :: Char -> Bool
notReservedChar = (`notElem` "\".,`[]{}:;()")

identifier :: [String -> Bool] -> Parser String
identifier = identifier' [not . isSpace, notReservedChar]

identifier' :: [Char -> Bool] -> [String -> Bool] -> Parser String
identifier' charTests stringTests = do
  i <- takeWhile1 "identifier" (\c -> all ($ c) charTests)
  guard (all ($ i) stringTests)
  pure i

-- a wordyId isn't all digits, and isn't all symbols
wordyId :: [String] -> Parser String
wordyId keywords = token $ f <$> sepBy1 dot id
  where
    dot = char '.'
    id = identifier [any (not . Char.isDigit), any Char.isAlphaNum, (`notElem` keywords)]
    f segs = intercalate "." segs

-- a symbolyId is all symbols
symbolyId :: [String] -> Parser String
symbolyId keywords = token $ identifier'
  [notReservedChar, not . Char.isSpace, \c -> Char.isSymbol c || Char.isPunctuation c]
  [(`notElem` keywords)]

token :: Parser a -> Parser a
token p = p <* ignored

haskellLineComment :: Parser ()
haskellLineComment = void $ string "--" *> takeWhile "-- comment" (/= '\n')

lineErrorUnless :: String -> Parser a -> Parser a
lineErrorUnless s p = commitFail $ Parser $ \input -> case run' p input of
    Fail e b -> Fail (s:m:e) b
      where m = "near \'" ++ Prelude.takeWhile (/= '\n') (fst input) ++ "\'"
    ok -> ok

parenthesized :: Parser a -> Parser a
parenthesized p = lp *> body <* rp
  where
    lp = token (char '(')
    body = p
    rp = lineErrorUnless "missing )" $ token (char ')')

takeWhile :: String -> (Char -> Bool) -> Parser String
takeWhile msg f = scope msg . Parser $ \(s,_) ->
  let hd = Prelude.takeWhile f s
  in Succeed hd (length hd) False

takeWhile1 :: String -> (Char -> Bool) -> Parser String
takeWhile1 msg f = scope msg . Parser $ \(s,_) ->
  let hd = Prelude.takeWhile f s
  in if null hd then Fail ["takeWhile1 empty: " ++ take 20 s] False
     else Succeed hd (length hd) False

whitespace :: Parser ()
whitespace = void $ takeWhile "whitespace" Char.isSpace

whitespace1 :: Parser ()
whitespace1 = void $ takeWhile1 "whitespace1" Char.isSpace

nonempty :: Parser a -> Parser a
nonempty p = Parser $ \s -> case run' p s of
  Succeed _ 0 b -> Fail [] b
  ok -> ok

scope :: String -> Parser a -> Parser a
scope s p = Parser $ \input -> case run' p input of
  Fail e b -> Fail (s:e) b
  ok -> ok

commitSuccess :: Parser a -> Parser a
commitSuccess p = Parser $ \input -> case run' p input of
  Fail e b -> Fail e b
  Succeed a n _ -> Succeed a n True

commitFail :: Parser a -> Parser a
commitFail p = Parser $ \input -> case run' p input of
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

ignored :: Parser ()
ignored = void $ many (whitespace1 <|> haskellLineComment)

toEither :: Result a -> Either String a
toEither (Fail e _) = Left (intercalate "\n" e)
toEither (Succeed a _ _) = Right a

data Result a
  = Fail [String] Bool
  | Succeed a Int Bool
  deriving (Show,Functor,Foldable,Traversable)

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
    Succeed a n committed -> case run' (f a) (drop n (fst s), snd s) of
      Succeed b m c2 -> Succeed b (n+m) (committed || c2)
      Fail e b -> Fail e (committed || b)
    Fail e b -> Fail e b

instance MonadPlus Parser where
  mzero = Parser $ \_ -> Fail [] False
  mplus p1 p2 = Parser $ \s -> case run' p1 s of
    Fail _ False -> run' p2 s
    ok -> ok
