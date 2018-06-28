module Unison.Lexer where

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
type Pos = (Line, Column)

data Err = Err -- richer algebra

lexer :: String -> Either Err [(Token, (Pos, Pos))]
lexer _input = error "todo"

