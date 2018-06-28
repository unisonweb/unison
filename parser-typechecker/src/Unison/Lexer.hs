{-# Language LambdaCase #-}
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

type Line = Int
type Column = Int
type Position = (Line, Column)

data Pos = Pos {-# Unpack #-} !Line !Column

data Err = Err -- richer algebra

-- data Layout = Layout { layout :: [Column], layoutEnabled :: Bool }

type Layout = [Column]

top :: Layout -> Column
top []    = 1
top (h:_) = h

pop :: Layout -> Layout
pop = drop 1

lexer :: String -> Either Err [(Token, Position)]
lexer rem = done $ go1 [] (Pos 1 1) rem where
  done _ = error "todo"
  -- skip whitespace and comments
  go1 l pos rem = span' isSpace rem $ \case
    (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
      go1 l (advance ('-':'-':ignored) . advance spaces $ pos) rem
    (spaces, rem) -> popLayout l (advance spaces pos) rem
  -- pop the layout stack and emit `Semi` / `Close` tokens as needed
  popLayout l      p       []                = replicate (length l) (Close, p)
  popLayout l p@(Pos _ c2) rem | top l == c2 = (Semi, p) : go2 l p rem
                               | top l <  c2 = go2 l p rem
                               | top l >  c2 = (Close, p) : popLayout (pop l) p rem
                               | otherwise   = error "impossible"
  -- reading a token
  go2 l pos rem = case rem of
    ch  : rem     | Set.member ch delimiters  -> (Reserved [ch], pos)  : go1 l (inc pos) rem
    ':' : c : rem | isSpace c || isAlphaNum c -> (Reserved [':'], pos) : go1 l (inc pos) (c:rem)
    '=' : c : rem | isSpace c || isAlphaNum c -> (Reserved [':'], pos) : go1 l (inc pos) (c:rem)
    -- '"' : rem                                 -> span' (/= '"') rem $ \(lit, rem) -> (Textual lit, pos)
    -- '`' : rem                                 -> span'
    _ -> error "todo"



delimiters :: Set Char
delimiters = Set.fromList "()[]{},"

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)

advance :: String -> Pos -> Pos
advance rem pos@(Pos line col) = case rem of
  []       -> pos
  '\r':rem -> advance rem $ Pos line col
  '\n':rem -> advance rem $ Pos (line + 1) 1
  _:rem    -> advance rem $ Pos line (col + 1)

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
