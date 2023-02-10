{-# LANGUAGE BangPatterns #-}

module Unison.Util.Text.Pattern where

import Data.Char (isAlphaNum, isControl, isLetter, isLower, isMark, isNumber, isPrint, isPunctuation, isSeparator, isSpace, isSymbol, isUpper)
import qualified Data.Text as DT
import Unison.Util.Text (Text)
import qualified Unison.Util.Text as Text

data Pattern
  = Join [Pattern] -- sequencing of patterns
  | Or Pattern Pattern -- left-biased choice: tries second pattern only if first fails
  | Capture Pattern -- capture all the text consumed by the inner pattern, discarding its subcaptures
  | Many Pattern -- zero or more repetitions (at least 1 can be written: Join [p, Many p])
  | Replicate Int Int Pattern -- m to n occurrences of a pattern, optional = 0-1
  | Eof -- succeed if given the empty text, fail otherwise
  | Literal Text -- succeed if input starts with the given text, advance by that text
  | Char CharPattern -- succeed if input starts with a char matching the given pattern, advance by 1 char
  deriving (Show, Eq, Ord)

data CharPattern
  = Any -- any char
  | Not CharPattern -- negation of the given pattern
  | Union CharPattern CharPattern -- match if either pattern matches
  | Intersect CharPattern CharPattern -- match if both patterns match
  | CharRange Char Char -- match if char is in the given range
  | CharSet [Char] -- match if char is in the given set
  | CharClass CharClass -- match if char is in the given class
  deriving (Show, Eq, Ord)

data CharClass
  = AlphaNum -- alphabetic or numeric characters
  | Upper -- uppercase alphabetic characters
  | Lower -- lowercase alphabetic characters
  | Whitespace -- whitespace characters (space, tab, newline, etc.)
  | Control -- non-printing control characters
  | Printable -- letters, numbers, punctuation, symbols, spaces
  | MarkChar -- accents, diacritics, etc.
  | Number -- numeric characters in any script
  | Punctuation -- connectors, brackets, quotes
  | Symbol -- symbols (math, currency, etc.)
  | Separator -- spaces, line separators, paragraph separators
  | Letter -- letters in any script
  deriving (Show, Eq, Ord)

-- Wrapper type. Holds a pattern together with its compilation. This is used as
-- the semantic value of a unison `Pattern a`. Laziness avoids building the
-- matcher until it actually needs to be used, and also avoids recalculating the
-- match function if a `CPattern` is 'run' multiple times, while allowing the
-- builtin runner to just take two arguments, and not try to build a partial
-- application by hand.
--
-- In the future, this can existentially quantify over the type being matched.
data CPattern = CP Pattern (Text -> Maybe ([Text], Text))

instance Eq CPattern where
  CP p _ == CP q _ = p == q

instance Ord CPattern where
  CP p _ `compare` CP q _ = compare p q

cpattern :: Pattern -> CPattern
cpattern p = CP p (run p)

run :: Pattern -> Text -> Maybe ([Text], Text)
run p =
  let cp = compile p (\_ _ -> Nothing) (\acc rem -> Just (s acc, rem))
      s = reverse . capturesToList . stackCaptures
   in \t -> cp (Empty emptyCaptures) t

-- Stack used to track captures and to support backtracking.
-- A `try` will push a `Mark` that allows the old state
-- (both the list of captures and the current remainder)
-- to be restored on failure.
data Stack = Empty !Captures | Mark !Captures !Text !Stack

-- A difference list for representing the captures of a pattern.
-- So that capture lists can be appended in O(1).
type Captures = [Text] -> [Text]

stackCaptures :: Stack -> Captures
stackCaptures (Mark cs _ _) = cs
stackCaptures (Empty cs) = cs
{-# INLINE stackCaptures #-}

pushCaptures :: Captures -> Stack -> Stack
pushCaptures c (Empty cs) = Empty (appendCaptures c cs)
pushCaptures c (Mark cs t s) = Mark (appendCaptures c cs) t s
{-# INLINE pushCaptures #-}

pushCapture :: Text -> Stack -> Stack
pushCapture txt = pushCaptures (txt :)
{-# INLINE pushCapture #-}

appendCaptures :: Captures -> Captures -> Captures
appendCaptures c1 c2 = c1 . c2
{-# INLINE appendCaptures #-}

emptyCaptures :: Captures
emptyCaptures = id

capturesToList :: Captures -> [Text]
capturesToList c = c []

type Compiled r = (Stack -> Text -> r) -> (Stack -> Text -> r) -> Stack -> Text -> r

compile :: Pattern -> Compiled r
compile !Eof !err !success = go
  where
    go acc t
      | Text.size t == 0 = success acc t
      | otherwise = err acc t
compile (Literal txt) !err !success = go
  where
    go acc t
      | Text.take (Text.size txt) t == txt = success acc (Text.drop (Text.size txt) t)
      | otherwise = err acc t
compile (Char Any) !err !success = go
  where
    go acc t = case Text.drop 1 t of
      rem
        | Text.size t > Text.size rem -> success acc rem
        | otherwise -> err acc rem
compile (Capture (Many (Char Any))) !_ !success = \acc t -> success (pushCapture t acc) Text.empty
compile (Capture c) !err !success = go
  where
    err' _ _ acc0 t0 = err acc0 t0
    success' _ rem acc0 t0 = success (pushCapture (Text.take (Text.size t0 - Text.size rem) t0) acc0) rem
    compiled = compile c err' success'
    go acc t = compiled acc t acc t
compile (Or p1 p2) err success = cp1
  where
    cp2 = compile p2 err success
    cp1 = try "Or" (compile p1) cp2 success
compile (Join ps) !err !success = go ps
  where
    go [] = success
    go (p : ps) =
      let pc = compile p err psc
          psc = compile (Join ps) err success
       in pc
compile (Char cp) !err !success = go
  where
    ok = charPatternPred cp
    go acc t = case Text.uncons t of
      Just (ch, rem) | ok ch -> success acc rem
      _ -> err acc t
compile (Many p) !_ !success = case p of
  Char Any -> (\acc _ -> success acc Text.empty)
  Char cp -> walker (charPatternPred cp)
  p -> go
    where
      go = compile p success success'
      success' acc rem
        | Text.size rem == 0 = success acc rem
        | otherwise = go acc rem
  where
    walker ok = go
      where
        go acc t = case Text.unconsChunk t of
          Nothing -> success acc t
          Just (Text.chunkToText -> txt, t) -> case DT.dropWhile ok txt of
            rem
              | DT.null rem -> go acc t
              | otherwise ->
                -- moving the remainder to the root of the tree is much more efficient
                -- since the next uncons will be O(1) rather than O(log n)
                -- this can't unbalance the tree too badly since these promoted chunks
                -- are being consumed and will get removed by a subsequent uncons
                success acc (Text.appendUnbalanced (Text.fromText rem) t)
    {-# INLINE walker #-}
compile (Replicate m n p) !err !success = case p of
  Char Any -> \acc t ->
    if Text.size t < m
      then err acc t
      else success acc (Text.drop n t)
  Char cp -> dropper (charPatternPred cp)
  _ -> try "Replicate" (go1 m) err (go2 (n - m))
  where
    go1 0 = \_err success stk rem -> success stk rem
    go1 n = \err success -> compile p err (go1 (n - 1) err success)
    go2 0 = success
    go2 n = try "Replicate" (compile p) success (go2 (n - 1))

    dropper ok acc t
      | (i, rest) <- Text.dropWhileMax ok n t, i >= m = success acc rest
      | otherwise = err acc t

charInPred, charNotInPred :: [Char] -> Char -> Bool
charInPred [] = const False
charInPred (c : chs) = let ok = charInPred chs in \ci -> ci == c || ok ci
charNotInPred [] = const True
charNotInPred (c : chs) = let ok = charNotInPred chs in (\ci -> ci /= c && ok ci)

charPatternPred :: CharPattern -> Char -> Bool
charPatternPred Any = const True
charPatternPred (Not cp) = let notOk = charPatternPred cp in \ci -> not (notOk ci)
charPatternPred (Union cp1 cp2) = let ok1 = charPatternPred cp1; ok2 = charPatternPred cp2 in \ci -> ok1 ci || ok2 ci
charPatternPred (Intersect cp1 cp2) = let ok1 = charPatternPred cp1; ok2 = charPatternPred cp2 in \ci -> ok1 ci && ok2 ci
charPatternPred (CharRange c1 c2) = \ci -> ci >= c1 && ci <= c2
charPatternPred (CharSet cs) = charInPred cs
charPatternPred (CharClass cc) = charClassPred cc

charClassPred :: CharClass -> Char -> Bool
charClassPred AlphaNum = isAlphaNum
charClassPred Upper = isUpper
charClassPred Lower = isLower
charClassPred Whitespace = isSpace
charClassPred Control = isControl
charClassPred Printable = isPrint
charClassPred MarkChar = isMark
charClassPred Number = isNumber
charClassPred Punctuation = isPunctuation
charClassPred Symbol = isSymbol
charClassPred Separator = isSeparator
charClassPred Letter = isLetter

-- runs c and if it fails, restores state to what it was before
try :: String -> Compiled r -> Compiled r
try msg c err success stk rem =
  c err' success' (Mark id rem stk) rem
  where
    success' stk rem = case stk of
      Mark caps _ stk -> success (pushCaptures caps stk) rem
      _ -> error $ "Pattern compiler error in: " <> msg
    err' stk _ = case stk of
      Mark _ rem stk -> err stk rem
      _ -> error $ "Pattern compiler error in: " <> msg
{-# INLINE try #-}
