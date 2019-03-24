{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# Language BangPatterns, ViewPatterns, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Unison.Lexer where

import           Control.Lens.TH (makePrisms)
import           Control.Monad (join)
import qualified Control.Monad.State as S
import           Data.Char
import           Data.Foldable (toList)
import           Data.List
import qualified Data.List.NonEmpty as Nel
import           Data.Set (Set)
import Unison.Util.Monoid (intercalateMap)
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
  | InvalidEscapeCharacter Char
  | LayoutError
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
  | Blank String     -- a typed hole or placeholder
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
      pretty (Blank s) = "_" ++ s
      pretty (Numeric n) = n
      pretty (Hash h) = show h
      pretty (Err e) = show e
      pretty Close = "<outdent>"
      pretty Semi = "<virtual semicolon>"
      pad (Pos line1 col1) (Pos line2 col2) =
        if line1 == line2
        then replicate (col2 - col1) ' '
        else replicate (line2 - line1) '\n' ++ replicate col2 ' '

instance Applicative Token where
  pure a = Token a (Pos 0 0) (Pos 0 0)
  Token f start _ <*> Token a _ end = Token (f a) start end

type Line = Int
type Column = Int

data Pos = Pos {-# Unpack #-} !Line {-# Unpack #-} !Column deriving (Eq,Ord,Show)

instance Semigroup Pos where (<>) = mappend

instance Monoid Pos where
  mempty = Pos 0 0
  Pos line col `mappend` Pos line2 col2 =
    if line2 == 0 then Pos line (col + col2)
    else Pos (line + line2) col2

line :: Pos -> Line
line (Pos line _) = line

column :: Pos -> Column
column (Pos _ column) = column

type BlockName = String
type Layout = [(BlockName,Column)]

top :: Layout -> Column
top []    = 1
top ((_,h):_) = h

-- todo: make Layout a NonEmpty
topBlockName :: Layout -> Maybe BlockName
topBlockName [] = Nothing
topBlockName ((name,_):_) = Just name

pop :: [a] -> [a]
pop = drop 1

topLeftCorner :: Pos
topLeftCorner = Pos 1 1

data T a = T a [T a] [a] | L a deriving (Functor, Foldable, Traversable)

headToken :: T a -> a
headToken (T a _ _) = a
headToken (L a) = a

instance Show a => Show (T a) where
  show (L a) = show a
  show (T open mid close) =
    show open ++ "\n"
              ++ indent "  " (intercalateMap "\n" show mid) ++ "\n"
              ++ intercalateMap "" show close
    where
      indent by s = by ++ (s >>= go by)
      go by '\n' = '\n' : by
      go _ c = [c]

reorderTree :: ([T a] -> [T a]) -> T a -> T a
reorderTree _ l@(L _) = l
reorderTree f (T open mid close) = T open (f (reorderTree f <$> mid)) close

tree :: [Token Lexeme] -> T (Token Lexeme)
tree toks = one toks (\t _ -> t) where
  one (open@(payload -> Open _) : ts) k = many (T open) [] ts k
  one (t : ts) k = k (L t) ts
  one [] k = k lastErr [] where
    lastErr = case drop (length toks - 1) toks of
      [] -> L (Token (Err LayoutError) topLeftCorner topLeftCorner)
      (t : _) -> L $ t { payload = Err LayoutError }

  many open acc [] k = k (open (reverse acc) []) []
  many open acc (t@(payload -> Close) : ts) k = k (open (reverse acc) [t]) ts
  many open acc ts k = one ts $ \t ts -> many open (t:acc) ts k

stanzas :: [T (Token Lexeme)] -> [[T (Token Lexeme)]]
stanzas ts = go [] ts where
  go acc [] = [reverse acc]
  go acc (t:ts) = case payload $ headToken t of
    Semi   -> (reverse $ t : acc) : go [] ts
    _      -> go (t:acc) ts

-- Moves type and effect declarations to the front of the token stream
-- and move `use` statements to the front of each block
reorder :: [T (Token Lexeme)] -> [T (Token Lexeme)]
reorder ts = join . sortWith f . stanzas $ ts
  where
    f [] = 3 :: Int
    f (t : _) = case payload $ headToken t of
      Open "type" -> 0
      Reserved "effect" -> 0
      Reserved "ability" -> 0
      Reserved "use" -> 1
      _ -> 3 :: Int

lexer :: String -> String -> [Token Lexeme]
lexer scope rem =
  let t = tree $ lexer0 scope rem
      -- after reordering can end up with trailing semicolon at the end of
      -- a block, which we remove with this pass
      fixup ((payload -> Semi) : t@(payload -> Close) : tl) = t : fixup tl
      fixup [] = []
      fixup (h : t) = h : fixup t
  in fixup . toList $ reorderTree reorder t

lexer0 :: String -> String -> [Token Lexeme]
lexer0 scope rem =
    Token (Open scope) topLeftCorner topLeftCorner
      : pushLayout scope [] topLeftCorner rem
  where
    -- skip whitespace and comments
    goWhitespace :: Layout -> Pos -> [Char] -> [Token Lexeme]
    goWhitespace l pos rem = span' isSpace rem $ \case
      (_spaces, '-':'-':'-':_rem) -> popLayout0 l pos []
      (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
        goWhitespace l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
      (spaces, rem) -> popLayout l (incBy spaces pos) rem

    popLayout :: Layout -> Pos -> [Char] -> [Token Lexeme]
    popLayout l pos rem = case matchKeyword' layoutCloseAndOpenKeywords rem of
      Nothing -> case matchKeyword' layoutCloseOnlyKeywords rem of
        Nothing -> popLayout0 l pos rem
        Just (kw, rem) ->
          let end = incBy kw pos
          in Token Close pos end
               : Token (Reserved kw) pos end
               : goWhitespace (drop 1 l) (incBy kw pos) rem
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
      | top l == c2 = Token Semi p p : go l p rem
      | top l <  c2 = go l p rem
      | top l >  c2 = Token Close p p : popLayout0 (pop l) p rem
      | otherwise   = error "impossible"

    -- todo: is there a reason we want this to be more than just:
    -- go1 (top l + 1 : l) pos rem
    -- looks for the next non whitespace, non-comment character, and
    -- pushes its column onto the layout stack
    pushLayout :: BlockName -> Layout -> Pos -> [Char] -> [Token Lexeme]
    pushLayout b l pos rem = span' isSpace rem $ \case
      (_spaces, '-':'-':'-':_rem) -> popLayout0 l pos []
      (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
        pushLayout b l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
      (spaces, rem) ->
        let topcol = top l
            pos'   = incBy spaces pos
            col'   = column pos'
        in
          if b == "=" && col' <= topcol then
            -- force closing by introducing a fake col +1 layout
            popLayout0 ((b, col' + 1) : l) pos' rem
          else
            go ((b, col') : l) pos' rem

    -- Closes a layout block with the given `close` token, e.g. `)` or `}`
    closeWith :: String -> Layout -> Pos -> [Char] -> [Token Lexeme]
    closeWith close l pos rem =
      Token Close pos (incBy close pos) : goWhitespace (drop 1 l) (inc pos) rem

    -- assuming we've dealt with whitespace and layout, read a token
    go :: Layout -> Pos -> [Char] -> [Token Lexeme]
    go l pos rem = case rem of
      [] -> popLayout0 l pos []
      -- '{' and '(' both introduce a block, which is closed by '}' and ')'
      -- The lexer doesn't distinguish among closing blocks: all the ways of
      -- closing a block emit the same sort of token, `Close`.
      --
      -- Note: within {}'s, `->` does not open a block, since `->` is used
      -- inside request patterns like `{State.set s -> k}`
      '{' : rem -> Token (Open "{") pos (inc pos) : pushLayout "{" l (inc pos) rem
      '}' : rem -> closeWith "}" l pos rem
      '(' : rem -> Token (Open "(") pos (inc pos) : pushLayout "(" l (inc pos) rem
      ')' : rem -> closeWith ")" l pos rem
      ch : rem | Set.member ch delimiters ->
        Token (Reserved [ch]) pos (inc pos) : goWhitespace l (inc pos) rem
      op : rem@(c : _)
        | (op == '\'' || op == '!')
        && (isSpace c || isAlphaNum c || Set.member c delimiters) ->
          Token (Reserved [op]) pos (inc pos) : goWhitespace l (inc pos) rem
      ':' : rem@(c : _) | isSpace c || isAlphaNum c ->
        Token (Reserved ":") pos (inc pos) : goWhitespace l (inc pos) rem
      '@' : rem ->
        Token (Reserved "@") pos (inc pos) : goWhitespace l (inc pos) rem
      '_' : rem | hasSep rem ->
        Token (Blank "") pos (inc pos) : goWhitespace l (inc pos) rem
      '_' : (wordyId -> Right (id, rem)) ->
        let pos' = incBy id $ inc pos
        in Token (Blank id) pos pos' : goWhitespace l pos' rem
      '|' : c : rem | isSpace c || isAlphaNum c ->
        Token (Reserved "|") pos (inc pos) : goWhitespace l (inc pos) (c:rem)
      '=' : (rem @ (c : _)) | isSpace c || isAlphaNum c ->
        let end = inc pos
        in case topBlockName l of
          -- '=' does not open a layout block if within a type declaration
          Just "type" -> Token (Reserved "=") pos end : goWhitespace l end rem
          Just _      -> Token (Open "=") pos end : pushLayout "=" l end rem
          Nothing     -> Token (Err LayoutError) pos pos : recover l pos rem
      '-' : '>' : (rem @ (c : _))
        | isSpace c || isAlphaNum c || Set.member c delimiters ->
          let end = incBy "->" pos
          in case topBlockName l of
              Just "of" -> -- `->` opens a block when pattern-matching only
                Token (Open "->") pos end : pushLayout "->" l end rem
              Just _ -> Token (Reserved "->") pos end : goWhitespace l end rem
              Nothing -> Token (Err LayoutError) pos pos : recover l pos rem
      -- string literals and backticked identifiers
      '"' : rem -> case splitStringLit rem of
        Right (delta, lit, rem) -> let end = pos <> delta in
          Token (Textual lit) pos end : goWhitespace l end rem
        Left (TextLiteralMissingClosingQuote _) -> [Token (Err $ TextLiteralMissingClosingQuote rem) pos pos]
        Left err -> [Token (Err err) pos pos]
      '`' : rem -> case wordyId rem of
        Left e -> Token (Err e) pos pos : recover l pos rem
        Right (id, rem) ->
          let end = inc . incBy id . inc $ pos in
                Token (Backticks id) pos end : goWhitespace l end (pop rem)

      -- keywords and identifiers
      (symbolyId -> Right (id, rem)) ->
        let end = incBy id pos in Token (SymbolyId id) pos end : goWhitespace l end rem
      (wordyId -> Right (id, rem)) ->
        let end = incBy id pos in Token (WordyId id) pos end : goWhitespace l end rem
      (matchKeyword -> Just (kw,rem)) ->
        let end = incBy kw pos in
              case kw of
                kw@"type" ->
                  Token (Open kw) pos end
                    : goWhitespace ((kw, column $ inc pos) : l) end rem
                kw | Set.member kw layoutKeywords ->
                       Token (Open kw) pos end : pushLayout kw l end rem
                   | otherwise -> Token (Reserved kw) pos end : goWhitespace l end rem

      -- numeric literals
      rem -> case numericLit rem of
        Right (Just (num, rem)) ->
          let end = incBy num pos in Token (Numeric num) pos end : goWhitespace l end rem
        Right Nothing -> Token (Err UnknownLexeme) pos pos : recover l pos rem
        Left e -> Token (Err e) pos pos : recover l pos rem

    recover _l _pos _rem = []

matchKeyword :: String -> Maybe (String,String)
matchKeyword = matchKeyword' keywords

matchKeyword' :: Set String -> String -> Maybe (String,String)
matchKeyword' keywords s = case span (not . isSep) s of
  (kw, rem) | Set.member kw keywords -> Just (kw, rem)
  _ -> Nothing

-- Split into a string literal and the remainder, and a delta which includes
-- both the starting and ending `"` character
-- The input string should only start with a '"' if the string literal is empty
splitStringLit :: String -> Either Err (Pos, String, String)
splitStringLit rem0 = go (inc mempty) "" rem0 where
  -- n tracks the raw character delta of this literal
  go !n !acc ('\\':s:rem) = case parseEscapeChar s of
    Just e -> go (inc . inc $ n) (e:acc) rem
    Nothing  -> Left $ InvalidEscapeCharacter s
  go !n !acc ('"':rem)    = Right (inc n, reverse acc, rem)
  go !n !acc (x:rem)      = go (inc n) (x:acc) rem
  go _ _ []               = Left $ TextLiteralMissingClosingQuote ""

appendFst :: Char -> (String, a) -> (String, a)
appendFst c (s, r) = (c : s, r)

-- Map a escape symbol to it's character literal
parseEscapeChar :: Char -> Maybe Char
parseEscapeChar '0'  = Just '\0'
parseEscapeChar 'a'  = Just '\a'
parseEscapeChar 'b'  = Just '\b'
parseEscapeChar 'f'  = Just '\f'
parseEscapeChar 'n'  = Just '\n'
parseEscapeChar 'r'  = Just '\r'
parseEscapeChar 't'  = Just '\t'
parseEscapeChar 'v'  = Just '\v'
parseEscapeChar '\'' = Just '\''
parseEscapeChar '"'  = Just '"'
parseEscapeChar '\\' = Just '\\'
parseEscapeChar _    = Nothing


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

-- Not a keyword, '.' delimited list of wordyId0 (should not include a trailing '.')
wordyId0 :: String -> Either Err (String, String)
wordyId0 s = span' wordyIdChar s $ \case
  (id @ (ch:_), rem) | not (Set.member id keywords)
                    && any (\ch -> isAlpha ch || isEmoji ch) id
                    && wordyIdStartChar ch
                    -> Right (id, rem)
  (id, _rem) -> Left (InvalidWordyId id)

wordyId :: String -> Either Err (String, String)
wordyId s = qualifiedId False s wordyId0 wordyId0

wordyIdStartChar :: Char -> Bool
wordyIdStartChar ch = isAlphaNum ch || isEmoji ch || ch == '_'

wordyIdChar :: Char -> Bool
wordyIdChar ch =
  isAlphaNum ch || isEmoji ch || ch `elem` "_-!'"

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
qualifiedId requireLast s0 leadingSegments lastSegment =
  goLeading 0 s0 where
   -- parsing 0 or more leading segments
   goLeading acc s = case leadingSegments s of
     Right (seg, '.' : rem)
       | not requireLast &&
         all (\c -> isSpace c || Set.member c delimiters) (take 1 rem)
         -> Right (seg, '.' : rem)
       | otherwise
         -> goLeading (acc + length seg + 1) rem
     Right (seg, rem) -> goLast Nothing (acc + length seg) rem
     Left e -> goLast (Just e) acc s
   err2 e e2 = case e of Nothing -> e2; Just e -> Both e e2
   -- leading segments produced acc before failing,
   -- now parse lastSegment if required
   goLast e acc s = case lastSegment s of
     Left e2 -> if requireLast || acc == 0 then Left (err2 e e2)
                else Right (take acc s0, s)
     Right (seg, s) -> Right (take (acc + length seg) s0, s)

-- Is a '.' delimited list of wordyId, with a final segment of `symbolyId0`
symbolyId :: String -> Either Err (String, String)
symbolyId s = qualifiedId True s wordyId0 symbolyId0

-- Strips off qualified name, ex: `Int.inc -> `(Int, inc)`
splitWordy :: String -> (String, String)
splitWordy s =
  let qn = reverse . drop 1 . dropWhile wordyIdChar . reverse $ s
  in (qn, if null qn then s else drop (length qn + 1) s)

-- Strips off qualified name, ex: `Int.+` -> `(Int, +)`
splitSymboly :: String -> (String,String)
splitSymboly s =
  let qn = reverse . dropWhile symbolyIdChar . reverse $ s
  in (qn, if null qn then s else drop (length qn + 1) s)

-- Returns either an error or an id and a remainder
symbolyId0 :: String -> Either Err (String, String)
symbolyId0 s = span' symbolyIdChar s $ \case
  (id @ (_:_), rem) | not (Set.member id reservedOperators) && hasSep rem -> Right (id, rem)
  (id, _rem) -> Left (InvalidSymbolyId id)

symbolyIdChar :: Char -> Bool
symbolyIdChar ch = Set.member ch symbolyIdChars

symbolyIdChars :: Set Char
symbolyIdChars = Set.fromList "!$%^&*-=+<>.~\\/|;"

keywords :: Set String
keywords = Set.fromList [
  "if", "then", "else", "forall", "∀",
  "handle", "in",
  "where", "use",
  "and", "or", "true", "false",
  "type", "effect", "ability", "alias",
  "let", "namespace", "case", "of"]

-- These keywords introduce a layout block
layoutKeywords :: Set String
layoutKeywords =
  Set.fromList [
    "if", "in", "let", "where", "of"
  ]

-- These keywords end a layout block and begin another layout block
layoutCloseAndOpenKeywords :: Set String
layoutCloseAndOpenKeywords = Set.fromList ["then", "else"]

-- These keywords end a layout block
layoutCloseOnlyKeywords :: Set String
layoutCloseOnlyKeywords = Set.fromList ["}"]

delimiters :: Set Char
delimiters = Set.fromList "()[]{},?"

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

debugLex'' :: [Token Lexeme] -> String
debugLex'' = show . fmap payload . tree

debugLex :: String -> String -> IO ()
debugLex scope = putStrLn . debugLex'' . lexer scope

debugLex' :: String -> String
debugLex' =  debugLex'' . lexer "debugLex"

debugLex''' :: String -> String -> String
debugLex''' s =  debugLex'' . lexer s

span' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
span' f a k = k (span f a)

spanThru' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
spanThru' f a k = case span f a of
  (l, []) -> k (l, [])
  (l, lz:r) -> k (l ++ [lz], r)
