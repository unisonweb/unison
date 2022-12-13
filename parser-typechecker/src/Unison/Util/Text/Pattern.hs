{-# LANGUAGE BangPatterns #-}

module Unison.Util.Text.Pattern where

import Basement.Compat.Base (Word64)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isDigit, isLetter, isLower, isNumber, isPunctuation, isSpace, isSymbol, isUpper)
import qualified Data.Text as DT
import Unison.Util.Text (Text)
import qualified Unison.Util.Text as Text

data Pattern
  = Join [Pattern] -- sequencing of patterns
  | Or Pattern Pattern -- left-biased choice: tries second pattern only if first fails
  | AddCapture Pattern -- capture all the text consumed by the inner pattern, discarding its subcaptures
  | Many Pattern -- zero or more repetitions (at least 1 can be written: Join [p, Many p])
  | Replicate Int Int Pattern -- m to n occurrences of a pattern, optional = 0-1
  | Eof -- succeed if given the empty text, fail otherwise
  | Literal Text -- succeed if input starts with the given text, advance by that text
  | Character CharClass -- consume a single character satisfying the given class, or fail
  deriving (Eq, Ord)

data CharClass
  = AnyChar -- matches any character
  | CharRange Char Char -- matches any character in the given range
  | CharIn [Char] -- matches any character in the given set
  | Digit -- matches any digit character (according to Char.isDigit)
  | Letter -- matches any letter character (according to Char.isLetter)
  | Space -- matches any space character (according to Char.isSpace)
  | Punctuation -- matches punctuation char (according to Char.isPunctuation)
  | Symbol -- matches any symbol character (according to Char.isSymbol)
  | Number -- numeric character (according to Char.isNumber)
  | Lower -- lowercase character (according to Char.isLower)
  | Upper -- uppercase character (according to Char.isUpper)
  | CharNot CharClass -- matches any character not in the given class
  | CharAnd CharClass CharClass -- matches any character in both classes
  | CharOr CharClass CharClass -- matches any character in either class
  deriving (Eq, Ord)

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
   in fmap (first (fmap captureText)) <$> cp (Empty emptyCaptures)

-- Stack used to track captures and to support backtracking.
-- A `try` will push a `Mark` that allows the old state
-- (both the list of captures and the current remainder)
-- to be restored on failure.
data Stack = Empty !Captures | Mark !Captures !Text !Stack

-- A difference list for representing the captures of a pattern.
-- So that capture lists can be appended in O(1).
type Captures = [Capture] -> [Capture]

newtype Position = Position Word64
  deriving (Eq, Ord, Show)

data Capture = Capture {captureText :: Text, capturePos :: Position}
  deriving (Eq, Ord, Show)

stackCaptures :: Stack -> Captures
stackCaptures (Mark cs _ _) = cs
stackCaptures (Empty cs) = cs
{-# INLINE stackCaptures #-}

pushCaptures :: Captures -> Stack -> Stack
pushCaptures c (Empty cs) = Empty (appendCaptures c cs)
pushCaptures c (Mark cs t s) = Mark (appendCaptures c cs) t s
{-# INLINE pushCaptures #-}

pushCapture :: Capture -> Stack -> Stack
pushCapture txt = pushCaptures (txt :)
{-# INLINE pushCapture #-}

appendCaptures :: Captures -> Captures -> Captures
appendCaptures c1 c2 = c1 . c2
{-# INLINE appendCaptures #-}

emptyCaptures :: Captures
emptyCaptures = id

capturesToList :: Captures -> [Capture]
capturesToList c = c []

type Compiled r = (Stack -> Text -> r) -> (Stack -> Text -> r) -> Stack -> Text -> r

classToPred :: CharClass -> Char -> Bool
classToPred AnyChar _ = True
classToPred (CharRange c1 c2) c = c >= c1 && c <= c2
classToPred (CharIn cs) c = c `elem` cs
classToPred Digit c = isDigit c
classToPred Letter c = isLetter c
classToPred Space c = isSpace c
classToPred Punctuation c = isPunctuation c
classToPred Symbol c = isSymbol c
classToPred Number c = isNumber c
classToPred Lower c = isLower c
classToPred Upper c = isUpper c
classToPred (CharNot cc) c = not (classToPred cc c)
classToPred (CharAnd cc1 cc2) c = classToPred cc1 c && classToPred cc2 c
classToPred (CharOr cc1 cc2) c = classToPred cc1 c || classToPred cc2 c

compile :: Pattern -> Compiled r
compile Eof !err !success = go
  where
    go acc t
      | Text.size t == 0 = success acc t
      | otherwise = err acc t
compile (Literal txt) !err !success = go
  where
    go acc t
      | Text.take (Text.size txt) t == txt = success acc (Text.drop (Text.size txt) t)
      | otherwise = err acc t
compile (Character AnyChar) !err !success = go
  where
    go acc t = case Text.drop 1 t of
      rem
        | Text.size t > Text.size rem -> success acc rem
        | otherwise -> err acc rem
compile (AddCapture (Many (Character AnyChar))) !_ !success = \acc t -> success (pushCapture t acc) Text.empty
compile (AddCapture c) !err !success = go
  where
    err' _ _ acc0 t0 = err acc0 t0
    success' _ rem acc0 t0 =
      let position = fromIntegral (Text.size t0 - Text.size rem)
          capture = Capture (Text.take (Text.size t0 - Text.size rem) t0) position
       in success (pushCapture capture acc0) rem
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
compile (Character charClass) !err !success = go
  where
    ok = classToPred charClass
    go acc t = case Text.uncons t of
      Just (ch, rem) | ok ch -> success acc rem
      _ -> err acc t
compile (Many p) !_ !success = case p of
  Character AnyChar -> (\acc _ -> success acc Text.empty)
  Character cclass -> walker (classToPred cclass)
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
  Character AnyChar -> \acc t ->
    if Text.size t < m
      then err acc t
      else success acc (Text.drop n t)
  Character cclass -> dropper (classToPred cclass)
  _ -> try "Replicate" (go1 m) err (go2 (n - m))
  where
    go1 0 = \_err success stk rem -> success stk rem
    go1 n = \err success -> compile p err (go1 (n - 1) err success)
    go2 0 = success
    go2 n = try "Replicate" (compile p) success (go2 (n - 1))

    dropper ok acc t
      | (i, rest) <- Text.dropWhileMax ok n t, i >= m = success acc rest
      | otherwise = err acc t

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
