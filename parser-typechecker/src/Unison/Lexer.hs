{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# Language LambdaCase, ViewPatterns, TemplateHaskell, DeriveFunctor #-}

module Unison.Lexer where

import           Control.Lens.TH (makePrisms)
import           Control.Monad (join)
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import           Data.Char
import           Data.Foldable (traverse_)
import           Data.List
import qualified Data.List.NonEmpty as Nel
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Exts (sortWith)
import           Text.Megaparsec.Error (ShowToken(..))
import           Unison.Hash (Hash)

data Err
  = InvalidWordyId String
  | InvalidSymbolyId String
  | Both Err Err
  | MissingFractional String -- ex `1.` rather than `1.04`
  | UnknownLexeme
  | TextLiteralMissingClosingQuote String
  deriving (Eq,Ord,Show) -- richer algebra

-- Design principle:
--   `[Lexeme]` should be sufficient information for parsing without
--   further knowledge of spacing or indentation levels
--   any knowledge of comments
data Lexeme
  = Open String      -- start of a block
  | Semi             -- separator between elements of a block
  | Close            -- end of a block
  | Reserved String  -- reserved tokens such as `{`, `(`, `type`, `of`, etc
  | Textual String   -- text literals, `"foo bar"`
  | Backticks String -- an identifier in backticks
  | WordyId String   -- a (non-infix) identifier
  | SymbolyId String -- an infix identifier
  | Numeric String   -- numeric literals, left unparsed
  | Hash Hash        -- hash literals
  | Err Err
  deriving (Eq,Show,Ord)

makePrisms ''Lexeme

data Token a = Token {
  payload :: a,
  start :: Pos,
  end :: Pos
} deriving (Eq, Ord, Show, Functor)

instance ShowToken (Token Lexeme) where
  showTokens xs =
      join . Nel.toList . S.evalState (traverse go xs) . end $ Nel.head xs
    where
      go :: (Token Lexeme) -> S.State Pos String
      go tok = do
        prev <- S.get
        S.put $ end tok
        pure $ pad prev (start tok) ++ pretty (payload tok)
      pretty (Open s) = s
      pretty (Reserved w) = w
      pretty (Textual t) = '"' : t ++ ['"']
      pretty (Backticks n) = '`' : n ++ ['`']
      pretty (WordyId n) = n
      pretty (SymbolyId n) = n
      pretty (Numeric n) = n
      pretty (Hash h) = show h
      pretty (Err e) = show e
      pretty _ = ""
      pad (Pos line1 col1) (Pos line2 col2) =
        if line1 == line2
        then replicate (col2 - col1) ' '
        else replicate (line2 - line1) '\n' ++ replicate col2 ' '

instance Applicative Token where
  pure a = Token a (Pos 0 0) (Pos 0 0)
  Token f start _ <*> Token a _ end = Token (f a) start end

type Line = Int
type Column = Int

data Pos = Pos {-# Unpack #-} !Line !Column deriving (Eq,Ord,Show)

line :: Pos -> Line
line (Pos line _) = line

column :: Pos -> Column
column (Pos _ column) = column

type BlockName = String
type Layout = [(BlockName,Column)]

top :: Layout -> Column
top []    = 1
top ((_,h):_) = h

pop :: [a] -> [a]
pop = drop 1

topLeftCorner :: Pos
topLeftCorner = Pos 1 1

stanzas :: [Token Lexeme] -> [[Token Lexeme]]
stanzas ts = go [] ts where
  go acc [] = [reverse acc]
  go acc (c@(payload -> Semi) : t : ts) | column (start t) == 1 = (reverse $ c : acc) : go [] (t:ts)
  go acc (t:ts) = go (t:acc) ts

-- Moves type and effect declarations to the front of the token stream
reorder :: [Token Lexeme] -> [Token Lexeme]
reorder ts = first ++ (join . sortWith f . stanzas $ core) ++ last
  where
    n = length ts
    first = take 1 ts -- save `Open` token from start
    last = drop (n - 1) ts -- and `Close` token from end
    core = take (n - 2) . drop 1 $ ts -- middle n-2 elements
    f ((payload -> Reserved "type")   : _) = 0
    f ((payload -> Reserved "effect") : _) = 0
    f _                                    = 1 :: Int

lexer :: String -> String -> [Token Lexeme]
lexer scope rem =
    Token (Open scope) topLeftCorner topLeftCorner
      : pushLayout scope [] topLeftCorner rem
  where
    -- skip whitespace and comments
    go1 :: Layout -> Pos -> [Char] -> [Token Lexeme]
    go1 l pos rem = span' isSpace rem $ \case
      (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
        go1 l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
      (spaces, rem) -> popLayout l (incBy spaces pos) rem

    popLayout :: Layout -> Pos -> [Char] -> [Token Lexeme]
    popLayout l pos rem = case matchKeyword' layoutCloseAndOpenKeywords rem of
      Nothing -> case matchKeyword' layoutCloseOnlyKeywords rem of
        Nothing -> popLayout0 l pos rem
        Just (kw, rem) ->
          let end = incBy kw pos
          in Token Close pos end
               : Token (Reserved kw) pos end
               : go1 (drop 1 l) (incBy kw pos) rem
      Just (kw, rem) ->
        let end = incBy kw pos
        in Token Close pos pos
             : Token (Open kw) pos end
             -- todo: would be nice to check that top of `l` is an Open "if" or "then"
             : pushLayout kw (drop 1 l) end rem

    -- Examine current column and pop the layout stack
    -- and emit `Semi` / `Close` tokens as needed
    popLayout0 :: Layout -> Pos -> [Char] -> [Token Lexeme]
    popLayout0 l p [] = replicate (length l) $ Token Close p p
    popLayout0 l p@(Pos _ c2) rem
      | top l == c2 = Token Semi p p : go2 l p rem
      | top l <  c2 = go2 l p rem
      | top l >  c2 = Token Close p p : popLayout0 (pop l) p rem
      | otherwise   = error "impossible"

    -- todo: is there a reason we want this to be more than just:
    -- go1 (top l + 1 : l) pos rem
    -- looks for the next non whitespace, non-comment character, and
    -- pushes its column onto the layout stack
    pushLayout :: BlockName -> Layout -> Pos -> [Char] -> [Token Lexeme]
    pushLayout b l pos rem = span' isSpace rem $ \case
      (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
        pushLayout b l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
      (spaces, rem) ->
        let pos' = incBy spaces pos in go2 ((b, column pos') : l) pos' rem

    -- after we've dealt with whitespace and layout, read a token
    go2 :: Layout -> Pos -> [Char] -> [Token Lexeme]
    go2 l pos rem = case rem of
      [] -> popLayout0 l pos []
      -- delimiters - `:`, `@`, `|`, `=`, and `->`
      '{' : rem ->
        Token (Open "{") pos (inc pos) : pushLayout "{" l (inc pos) rem
      '}' : rem ->
        Token (Close) pos (inc pos) : pushLayout "{" l (inc pos) rem
-- ->{
      ch : rem | Set.member ch delimiters ->
        Token (Reserved [ch]) pos (inc pos) : go1 l (inc pos) rem
      ':' : c : rem | isSpace c || isAlphaNum c ->
        Token (Reserved ":") pos (inc pos) : go1 l (inc pos) (c:rem)
      '@' : rem ->
        Token (Reserved "@") pos (inc pos) : go1 l (inc pos) rem
      '_' : rem | hasSep rem ->
        Token (Reserved "_") pos (inc pos) : go1 l (inc pos) rem
      '|' : c : rem | isSpace c || isAlphaNum c ->
        Token (Reserved "|") pos (inc pos) : go1 l (inc pos) (c:rem)
      '=' : c : rem | isSpace c || isAlphaNum c ->
        Token (Open "=") pos (inc pos) : pushLayout "=" l (inc pos) (c:rem)
      '-' : '>' : (rem @ (c : _))
        | isSpace c || isAlphaNum c || Set.member c delimiters ->
          let end = incBy "->" pos
          in case l of
              ("of", _) : _ -> -- `->` opens a block when pattern-matching only
                Token (Open "->") pos end : pushLayout "->" l end rem
              _ -> Token (Reserved "->") pos end : go1 l end rem

      -- string literals and backticked identifiers
      '"' : rem -> span' (/= '"') rem $ \(lit, rem) ->
        if rem == [] then
          [Token (Err (TextLiteralMissingClosingQuote lit)) pos pos]
        else let end = inc . incBy lit . inc $ pos in
                   Token (Textual lit) pos end : go1 l end (pop rem)
      '`' : rem -> case wordyId rem of
        Left e -> Token (Err e) pos pos : recover l pos rem
        Right (id, rem) ->
          let end = inc . incBy id . inc $ pos in
                Token (Backticks id) pos end : go1 l end (pop rem)

      -- keywords and identifiers
      (symbolyId -> Right (id, rem)) ->
        let end = incBy id pos in Token (SymbolyId id) pos end : go1 l end rem
      (wordyId -> Right (id, rem)) ->
        let end = incBy id pos in Token (WordyId id) pos end : go1 l end rem
      (matchKeyword -> Just (kw,rem)) ->
        let end = incBy kw pos in
              case kw of
                kw | Set.member kw layoutKeywords ->
                       Token (Open kw) pos end : pushLayout kw l end rem
                   | otherwise -> Token (Reserved kw) pos end : go1 l end rem

      -- numeric literals
      rem -> case numericLit rem of
        Right (Just (num, rem)) ->
          let end = incBy num pos in Token (Numeric num) pos end : go1 l end rem
        Right Nothing -> Token (Err UnknownLexeme) pos pos : recover l pos rem
        Left e -> Token (Err e) pos pos : recover l pos rem

    recover _l _pos _rem = []

matchKeyword :: String -> Maybe (String,String)
matchKeyword = matchKeyword' keywords

matchKeyword' :: Set String -> String -> Maybe (String,String)
matchKeyword' keywords s = case span (not . isSpace) s of
  (kw, rem) | Set.member kw keywords -> Just (kw, rem)
  _ -> Nothing

numericLit :: String -> Either Err (Maybe (String,String))
numericLit s = go s
  where
  go ('+':s) = go2 "+" s
  go ('-':s) = go2 "-" s
  go s = go2 "" s
  go2 sign s = case span isDigit s of
    (num @ (_:_), []) -> pure $ pure (sign ++ num, [])
    (num @ (_:_), '.':rem) -> case span isDigit rem of
      (fractional @ (_:_), []) ->
        pure $ pure (sign ++ num ++ "." ++ fractional, [])
      (fractional @ (_:_), c:rem)
        | isSep c -> pure $ pure (sign ++ num ++ "." ++ fractional, c:rem)
        | otherwise -> pure Nothing
      ([], _) -> Left (MissingFractional (sign ++ num ++ "."))
    (num @ (_:_), c:rem) -> pure $ pure (sign ++ num, c:rem)
    ([], _) -> pure Nothing

isSep :: Char -> Bool
isSep c = isSpace c || Set.member c delimiters

hasSep :: String -> Bool
hasSep [] = True
hasSep (ch:_) = isSep ch

-- Not a keyword, has at least one letter, and with all characters matching `wordyIdChar`
-- TODO: Is a '.' delimited list of wordyId0 (should not include a trailing '.')
wordyId0 :: String -> Either Err (String, String)
wordyId0 s = span' wordyIdChar s $ \case
  (id @ (_:_), rem) | not (Set.member id keywords)
                   && any isAlpha id || any isEmoji id -> Right (id, rem)
  (id, _rem) -> Left (InvalidWordyId id)

wordyId :: String -> Either Err (String, String)
wordyId s = qualifiedId False s wordyId0 wordyId0

wordyIdChar :: Char -> Bool
wordyIdChar ch =
  not (isSpace ch) && (ch /= '.') && not (Set.member ch delimiters) && not (Set.member ch reserved)

isEmoji :: Char -> Bool
isEmoji c = c >= '\x1F600' && c <= '\x1F64F'

splitOn :: Char -> String -> [String]
splitOn c s = unfoldr step s where
  step [] = Nothing
  step s = Just (case break (== c) s of (l,r) -> (l, drop 1 r))

qualifiedId :: Bool
            -> String
            -> (String -> Either Err (String, String))
            -> (String -> Either Err (String, String))
            -> Either Err (String, String)
qualifiedId requireLast s0 leadingSegments lastSegment = go0 0 s0 where
   go0 acc s = case leadingSegments s of
     Right (seg, '.' : rem) -> go0 (acc + length seg + 1) rem
     Right (seg, rem) -> go1 Nothing (acc + length seg) rem
     Left e -> go1 (Just e) acc s
   err2 e e2 = case e of Nothing -> e2; Just e -> Both e e2
   go1 e acc s = case lastSegment s of
     Left e2 -> if requireLast || acc == 0 then Left (err2 e e2)
                else Right (take acc s0, s)
     Right (seg, s) -> Right (take (acc + length seg) s0, s)

-- Is a '.' delimited list of wordyId, with a final segment of `symbolyId0`
symbolyId :: String -> Either Err (String, String)
symbolyId s = qualifiedId True s wordyId0 symbolyId0

-- Returns either an error or an id and a remainder
symbolyId0 :: String -> Either Err (String, String)
symbolyId0 s = span' symbolyIdChar s $ \case
  (id @ (_:_), rem) | not (Set.member id reservedOperators) && hasSep rem -> Right (id, rem)
  (id, _rem) -> Left (InvalidSymbolyId id)

symbolyIdChar :: Char -> Bool
symbolyIdChar ch = Set.member ch symbolyIdChars

symbolyIdChars :: Set Char
symbolyIdChars = Set.fromList "!$%^&*-=+<>?.~\\/|;"

keywords :: Set String
keywords = Set.fromList [
  "if", "then", "else", "forall", "∀",
  "handle", "in",
  "where",
  "and", "or", "true", "false",
  "type", "effect", "alias",
  "let", "namespace", "case", "of"]

-- These keywords introduce a layout block
layoutKeywords :: Set String
layoutKeywords =
  Set.fromList [
    "if", "in", "let", "where", "of", "namespace"
  ]

-- These keywords end a layout block and begin another layout block
layoutCloseAndOpenKeywords :: Set String
layoutCloseAndOpenKeywords = Set.fromList ["then", "else"]

-- These keywords end a layout block
layoutCloseOnlyKeywords :: Set String
layoutCloseOnlyKeywords = Set.fromList ["}"]

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
  join [ "if\n"
       , "  s = 0\n"
       , "  s > 0\n"
       , "then\n"
       , "  s = 0\n"
       , "  s + 1\n"
       , "else\n"
       , "  s = 0\n"
       , "  s + 2\n" ]

debugLex :: String -> String -> IO ()
debugLex scope = flip S.evalStateT [] . traverse_ f . map payload . lexer scope
  where
    f :: Lexeme -> S.StateT String IO ()
    f x = do
      pad <- S.get
      S.lift . putStrLn $ pad ++ show x
      case x of
        Open _ -> S.modify (++ "  ")
        Close -> S.modify (drop 2)
        _ -> pure ()

debugLex' :: String -> String
debugLex' = unlines . W.execWriter . flip S.evalStateT [] . traverse_ f . map payload . lexer "debugLex"
  where
    f :: Lexeme -> S.StateT String (W.Writer [String]) ()
    f x = do
      pad <- S.get
      S.lift . W.tell $ [pad ++ show x]
      case x of
        Open _ -> S.modify (++ "  ")
        Close -> S.modify (drop 2)
        _ -> pure ()

span' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
span' f a k = k (span f a)

spanThru' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
spanThru' f a k = case span f a of
  (l, []) -> k (l, [])
  (l, lz:r) -> k (l ++ [lz], r)
