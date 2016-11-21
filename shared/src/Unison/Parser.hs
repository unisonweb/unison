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

data Env s =
  Env { overallInput :: String
      , offset :: !Int
      , state :: !s
      , currentInput :: String } -- always just `drop offset overallInput`

newtype Parser s a = Parser { run' :: Env s -> Result s a }

root :: Parser s a -> Parser s a
root p = ignored *> (p <* (optional semicolon <* eof))

semicolon :: Parser s ()
semicolon = void $ token (char ';')

semicolon2 :: Parser s ()
semicolon2 = semicolon *> semicolon

eof :: Parser s ()
eof = Parser $ \env -> case (currentInput env) of
  [] -> Succeed () (state env) 0
  _ -> Fail [Prelude.takeWhile (/= '\n') (currentInput env), "expected eof"] False

attempt :: Parser s a -> Parser s a
attempt p = Parser $ \s -> case run' p s of
   Fail stack _ -> Fail stack False
   succeed -> succeed

run :: Parser s a -> String -> s -> Result s a
run p s s0 = run' p (Env s 0 s0 s)

unsafeRun :: Parser s a -> String -> s -> a
unsafeRun p s s0 = case toEither $ run p s s0 of
  Right a -> a
  Left e -> error ("Parse error:\n" ++ e)

unsafeGetSucceed :: Result s a -> a
unsafeGetSucceed r = case r of
  Succeed a _ _ -> a
  Fail e _ -> error (unlines ("Parse error:":e))

string :: String -> Parser s String
string s = Parser $ \env ->
  if s `isPrefixOf` (currentInput env) then Succeed s (state env) (length s)
  else Fail ["expected " ++ s ++ ", got " ++ takeLine (currentInput env)] False

takeLine :: String -> String
takeLine = Prelude.takeWhile (/= '\n')

char :: Char -> Parser s Char
char c = Parser $ \env ->
  if listToMaybe (currentInput env) == Just c then Succeed c (state env) 1
  else Fail ["expected " ++ show c ++ ", got " ++ takeLine (currentInput env)] False

one :: (Char -> Bool) -> Parser s Char
one f = Parser $ \env -> case (currentInput env) of
  (h:_) | f h -> Succeed h (state env) 1
  _ -> Fail [] False

base64string' :: String -> Parser s String
base64string' alphabet = concat <$> many base64group
  where
    base64group :: Parser s String
    base64group = do
      chars <- some $ one (`elem` alphabet)
      padding <- sequenceA (replicate (padCount $ length chars) (char '='))
      return $ chars ++ padding
    padCount :: Int -> Int
    padCount len = case len `mod` 4 of 0 -> 0; n -> 4 - n

base64urlstring :: Parser s String
base64urlstring = base64string' $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"

notReservedChar :: Char -> Bool
notReservedChar = (`notElem` "\".,`[]{}:;()")

identifier :: [String -> Bool] -> Parser s String
identifier = identifier' [not . isSpace, notReservedChar]

identifier' :: [Char -> Bool] -> [String -> Bool] -> Parser s String
identifier' charTests stringTests = do
  i <- takeWhile1 "identifier" (\c -> all ($ c) charTests)
  guard (all ($ i) stringTests)
  pure i

-- a wordyId isn't all digits, isn't all symbols, and isn't a symbolyId
wordyId :: [String] -> Parser s String
wordyId keywords = do
  op <- (False <$ symbolyId keywords) <|> pure True
  guard op
  token $ f <$> sepBy1 dot id
  where
    dot = char '.'
    id = identifier [any (not . Char.isDigit), any Char.isAlphaNum, (`notElem` keywords)]
    f segs = intercalate "." segs

-- a symbolyId is all symbols
symbolyId :: [String] -> Parser s String
symbolyId keywords = scope "operator" . token $ do
  op <- identifier'
    [notReservedChar, (/= '_'), not . Char.isSpace, \c -> Char.isSymbol c || Char.isPunctuation c]
    [(`notElem` keywords)]
  qual <- optional (char '_' *> wordyId keywords)
  pure $ maybe op (\qual -> qual ++ "." ++ op) qual

token :: Parser s a -> Parser s a
token p = p <* ignored

haskellLineComment :: Parser s ()
haskellLineComment = void $ string "--" *> takeWhile "-- comment" (/= '\n')

lineErrorUnless :: String -> Parser s a -> Parser s a
lineErrorUnless s = commit . scope s

currentLine' :: Env s -> String
currentLine' (Env overall i s cur) = before ++ restOfLine where
  -- this grabs the current line up to current offset, i
  before = reverse . Prelude.takeWhile (/= '\n') . reverse . take i $ overall
  restOfLine = Prelude.takeWhile (/= '\n') cur

currentLine :: Parser s String
currentLine = Parser $ \env -> Succeed (currentLine' env) (state env) 0

parenthesized :: Parser s a -> Parser s a
parenthesized p = lp *> body <* rp
  where
    lp = token (char '(')
    body = p
    rp = lineErrorUnless "missing )" $ token (char ')')

takeWhile :: String -> (Char -> Bool) -> Parser s String
takeWhile msg f = scope msg . Parser $ \(Env _ _ s cur) ->
  let hd = Prelude.takeWhile f cur
  in Succeed hd s (length hd)

takeWhile1 :: String -> (Char -> Bool) -> Parser s String
takeWhile1 msg f = scope msg . Parser $ \(Env _ _ s cur) ->
  let hd = Prelude.takeWhile f cur
  in if null hd then Fail [] False
     else Succeed hd s (length hd)

whitespace :: Parser s ()
whitespace = void $ takeWhile "whitespace" Char.isSpace

whitespace1 :: Parser s ()
whitespace1 = void $ takeWhile1 "whitespace1" Char.isSpace

nonempty :: Parser s a -> Parser s a
nonempty p = Parser $ \s -> case run' p s of
  Succeed _ _ 0 -> Fail [] False
  ok -> ok

scope :: String -> Parser s a -> Parser s a
scope s p = Parser $ \env -> case run' p env of
  Fail e b -> Fail (currentLine' env : s:e) b
  ok -> ok

commit :: Parser s a -> Parser s a
commit p = Parser $ \input -> case run' p input of
  Fail e _ -> Fail e True
  Succeed a s n -> Succeed a s n

sepBy :: Parser s a -> Parser s b -> Parser s [b]
sepBy sep pb = f <$> optional (sepBy1 sep pb)
  where
    f Nothing = []
    f (Just l) = l

sepBy1 :: Parser s a -> Parser s b -> Parser s [b]
sepBy1 sep pb = (:) <$> pb <*> many (sep *> pb)

ignored :: Parser s ()
ignored = void $ many (whitespace1 <|> haskellLineComment)

toEither :: Result s a -> Either String a
toEither (Fail e _) = Left (intercalate "\n" e)
toEither (Succeed a _ _) = Right a

data Result s a
  = Fail [String] !Bool
  | Succeed a s !Int
  deriving (Show,Functor,Foldable,Traversable)

get :: Parser s s
get = Parser (\env -> Succeed (state env) (state env) 0)

set :: s -> Parser s ()
set s = Parser (\env -> Succeed () s 0)

instance Functor (Parser s) where
  fmap = liftM

instance Applicative (Parser s) where
  pure = return
  (<*>) = ap

instance Alternative (Parser s) where
  empty = mzero
  (<|>) = mplus

instance Monad (Parser s) where
  return a = Parser $ \env -> Succeed a (state env) 0
  Parser p >>= f = Parser $ \env@(Env overall i s cur) -> case p env of
    Succeed a s n ->
      case run' (f a) (Env overall (i+n) s (drop n cur)) of
        Succeed b s m -> Succeed b s (n+m)
        Fail e b -> Fail e b
    Fail e b -> Fail e b
  fail msg = Parser $ const (Fail [msg] False)

instance MonadPlus (Parser s) where
  mzero = Parser $ \_ -> Fail [] False
  mplus p1 p2 = Parser $ \env -> case run' p1 env of
    Fail _ False -> run' p2 env
    ok -> ok
