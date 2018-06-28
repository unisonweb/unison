{-# Language LambdaCase, ViewPatterns #-}

module Unison.Lexer where

import Data.Char
import qualified Data.Set as Set
import Data.Set (Set)

-- Design principle:
--   `[Token]` should be sufficient information for parsing without
--   further knowledge of spacing or indentation levels
--   any knowledge of comments
data Token
  = Open String      -- start of a block
  | Semi             -- separator between elements of a block
  | Close            -- end of a block
  | Reserved String  -- reserved tokens such as `{`, `(`, `type`, `effect`, `of`, etc
  | Textual String   -- text literals, `"foo bar"`
  | Backticks String -- an identifier in backticks
  | WordyId String   -- a (non-infix) identifier
  | SymbolyId String -- an infix identifier
  | Numeric String   -- numeric literals, left unparsed
  | Err Err
  deriving (Eq,Show,Ord)

type Line = Int
type Column = Int
type Position = (Line, Column)

data Pos = Pos {-# Unpack #-} !Line !Column deriving (Eq,Ord,Show)

data Err
  = InvalidWordyId String
  | InvalidSymbolyId String
  deriving (Eq,Ord,Show) -- richer algebra

-- data Layout = Layout { layout :: [Column], layoutEnabled :: Bool }

type Layout = [Column]

top :: Layout -> Column
top []    = 1
top (h:_) = h

pop :: [a] -> [a]
pop = drop 1

topLeftCorner :: Pos
topLeftCorner = Pos 1 1

lexer :: String -> [(Token, Position)]
lexer rem = done <$> go1 [] topLeftCorner rem where
  done (t, Pos line col) = (t, (line,col))
  -- skip whitespace and comments
  go1 l pos rem = span' isSpace rem $ \case
    (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
      go1 l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
    (spaces, rem) -> popLayout l (incBy spaces pos) rem

  -- pop the layout stack and emit `Semi` / `Close` tokens as needed
  popLayout l      p       []                = replicate (length l) (Close, p)
  popLayout l p@(Pos _ c2) rem
    | top l == c2 = if p /= topLeftCorner then (Semi, p) : go2 l p rem else go2 l p rem
    | top l <  c2 = go2 l p rem
    | top l >  c2 = (Close, p) : popLayout (pop l) p rem
    | otherwise   = error "impossible"

  -- after we've dealt with whitespace and layout, read a token
  go2 l pos rem = case rem of
    -- delimiters
    ch  : rem     | Set.member ch delimiters  -> (Reserved [ch], pos)  : go1 l (inc pos) rem
    ':' : c : rem | isSpace c || isAlphaNum c -> (Reserved [':'], pos) : go1 l (inc pos) (c:rem)
    -- todo - push layout
    '=' : c : rem | isSpace c || isAlphaNum c -> (Reserved [':'], pos) : go1 l (inc pos) (c:rem)
    '-' : '>' : (rem @ (c : _))
      | isSpace c || isAlphaNum c || Set.member c delimiters -> (Reserved "->", pos) : go1 l (incBy "->" pos) rem

    -- string literals and backticked identifiers
    '"' : rem -> span' (/= '"') rem $ \(lit, rem) ->
                   (Textual lit, pos) : go1 l (inc . incBy lit . inc $ pos) (pop rem)
    '`' : rem -> case wordyId rem of
      Left e -> (Err e, pos) : recover l pos rem
      Right (id, rem) -> (Backticks id, pos) : go1 l (inc . incBy id . inc $ pos) (pop rem)

    -- keywords and identifiers
    (wordyId -> Right (id, rem)) -> (WordyId id, pos) : go1 l (incBy id pos) rem
    (symbolyId -> Right (id, rem)) -> (SymbolyId id, pos) : go1 l (incBy id pos) rem
    -- todo layout blocks
    (matchKeyword -> Just (kw,rem)) -> (Reserved kw, pos) : go1 l (incBy kw pos) rem

    -- numeric literals

    _ -> error "todo"

  recover _l _pos _rem = []

matchKeyword :: String -> Maybe (String,String)
matchKeyword s = case span (not . isSpace) s of
  (kw, rem) | Set.member kw keywords -> Just (kw, rem)
  _ -> Nothing

-- Not a keyword, has at least one letter, and with all characters matching `wordyIdChar`
wordyId :: String -> Either Err (String, String)
wordyId s = span' wordyIdChar s $ \case
  (id @ (_:_), rem) | not (Set.member id keywords)
                   && any isAlpha id -> Right (id, rem)
  (id, _rem) -> Left (InvalidWordyId id)

wordyIdChar :: Char -> Bool
wordyIdChar ch = not (isSpace ch) && not (Set.member ch delimiters) && not (Set.member ch reserved)

symbolyId :: String -> Either Err (String, String)
symbolyId s = span' symbolyIdChar s $ \case
  (id @ (_:_), rem) | not (Set.member id reservedOperators) -> Right (id, rem)
  (id, _rem) -> Left (InvalidSymbolyId id)

symbolyIdChar :: Char -> Bool
symbolyIdChar ch = Set.member ch symbolyIdChars

symbolyIdChars :: Set Char
symbolyIdChars = Set.fromList "!@$%^&*-=+<>?.~"

keywords :: Set String
keywords = Set.fromList [
  "if", "then", "else",
  "handle", "in",
  "where",
  "and", "or", "true", "false",
  "type", "effect", "alias",
  "let", "namespace", "case", "of"]

delimiters :: Set Char
delimiters = Set.fromList "()[]{},"

reserved :: Set Char
reserved = Set.fromList "=:`\""

reservedOperators :: Set String
reservedOperators = Set.fromList ["->"]

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)

incBy :: String -> Pos -> Pos
incBy rem pos@(Pos line col) = case rem of
  []       -> pos
  '\r':rem -> incBy rem $ Pos line col
  '\n':rem -> incBy rem $ Pos (line + 1) 1
  _:rem    -> incBy rem $ Pos line (col + 1)

ex :: String
ex =
  unlines
  [ "hello -- ignored"
  , "goodbye"
  ]

span' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
span' f a k = k (span f a)

spanThru' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
spanThru' f a k = case span f a of
  (l, []) -> k (l, [])
  (l, lz:r) -> k (l ++ [lz], r)
