{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}
{-# Language BangPatterns #-}

module Unison.Parser where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.List hiding (takeWhile)
import Data.Maybe
import Data.Text (Text)
import Prelude hiding (takeWhile)
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Prelude
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Layout as L

-- import Debug.Trace

type Parser s a = Parsec.Parsec Text (Env s) a

data Env s = Env s L.LayoutEnv

instance L.HasLayoutEnv (Env s) where
  getLayoutEnv (Env _ l) = l
  setLayoutEnv l (Env s _) = Env s l

root :: Parser s a -> Parser s a
root p = optional (L.space) *> (p <* (optional semicolon <* eof))

semicolon :: Parser s ()
semicolon = void $ L.spaced L.semi

semicolon2 :: Parser s ()
semicolon2 = semicolon *> semicolon

eof :: Parser s ()
eof = Parsec.eof

attempt :: Parser s a -> Parser s a
attempt = Parsec.try

lookAhead :: Parser s a -> Parser s a
lookAhead = Parsec.lookAhead

run' :: Parser s a -> String -> s -> String -> Either String a
run' p s s0 name =
  case Parsec.runParser p (Env s0 L.defaultLayoutEnv) name (Text.pack s) of
    Left e -> Left (show e)
    Right a -> Right a

run :: Parser s a -> String -> s -> Either String a
run p s s0 = run' p s s0 ""

unsafeRun :: Parser s a -> String -> s -> a
unsafeRun p s s0 = case run p s s0 of
  Right a -> a
  Left e -> error e

string :: String -> Parser s String
string s = attempt (Parsec.string s) <|> fail ("expected '" ++ s ++ "'")

takeLine :: String -> String
takeLine = Prelude.takeWhile (/= '\n')

char :: Char -> Parser s Char
char = Parsec.char

one :: String -> (Char -> Bool) -> Parser s Char
one msg f = attempt $ Parsec.anyChar >>= \ch -> case f ch of
  True -> pure ch
  False -> fail $ "invalid " ++ msg ++ " '" ++ [ch] ++ "'"

base64string' :: String -> Parser s String
base64string' alphabet = concat <$> many base64group
  where
    base64group :: Parser s String
    base64group = do
      chars <- some $ one "expected base64 character" (`elem` alphabet)
      padding <- sequenceA (replicate (padCount $ length chars) (char '='))
      return $ chars ++ padding
    padCount :: Int -> Int
    padCount len = case len `mod` 4 of 0 -> 0; n -> 4 - n

base64urlstring :: Parser s String
base64urlstring = base64string' $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"

notReservedChar :: Char -> Bool
notReservedChar = (`notElem` "\".,`[]{}:;()")

identifier :: String -> [String -> Bool] -> Parser s String
identifier msg = identifier' msg [not . isSpace, notReservedChar]

label :: String -> Parser s a -> Parser s a
label msg p = Parsec.label p msg

identifier' :: String -> [Char -> Bool] -> [String -> Bool] -> Parser s String
identifier' msg charTests stringTests = attempt $ do
  i <- takeWhile1 msg (\c -> all ($ c) charTests)
  guard (all ($ i) stringTests)
  pure i

-- a wordyId isn't all digits, isn't all symbols, and isn't a symbolyId
wordyId :: [String] -> Parser s String
wordyId keywords = label "wordyId" . token $ do
  op <- (False <$ symbolyId keywords) <|> pure True
  guard op
  f <$> sepBy1 dot id -- todo: this screws up ∀ a. without a space following 'a'
  where
    dot = attempt (char '.')
    id = identifier "alphanumeric identifier"
      [any (not . Char.isDigit), any Char.isAlphaNum, (`notElem` keywords)]
    f segs = intercalate "." segs

-- a symbolyId is all symbols
symbolyId :: [String] -> Parser s String
symbolyId keywords = label "operator" . token $ do
  op <- identifier' "operator identifier"
    [notReservedChar, (/= '_'), not . Char.isSpace, \c -> Char.isSymbol c || Char.isPunctuation c]
    [(`notElem` keywords)]
  qual <- optional (char '_' *> wordyId keywords)
  pure $ maybe op (\qual -> qual ++ "." ++ op) qual

token :: Parser s a -> Parser s a
token p = attempt (L.spaced p)

parenthesized :: Parser s a -> Parser s a
parenthesized p = lp *> body <* rp
  where
    lp = char '(' <* L.withoutLayout "space" (optional L.space)
    body = L.withoutLayout "parentheses" p
    rp = token (char ')')

takeWhile :: String -> (Char -> Bool) -> Parser s String
takeWhile msg f = label msg (Parsec.many (one msg f))

takeWhile1 :: String -> (Char -> Bool) -> Parser s String
takeWhile1 msg f = label msg (Parsec.many1 (one msg f))

whitespace :: Parser s ()
whitespace = void (optional L.space)

whitespace1 :: Parser s ()
whitespace1 = void L.space

chainl1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 = Parsec.chainl1

chainl :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl = Parsec.chainl

sepBy :: Parser s a -> Parser s b -> Parser s [b]
sepBy sep pb = Parsec.sepBy pb sep

sepBy1 :: Parser s a -> Parser s b -> Parser s [b]
sepBy1 sep pb = Parsec.sepBy1 pb sep

get :: Parser s s
get = (\(Env s _) -> s) <$> Parsec.getState

set :: s -> Parser s ()
set s = do
  Env _ l <- Parsec.getState
  Parsec.putState (Env s l)
