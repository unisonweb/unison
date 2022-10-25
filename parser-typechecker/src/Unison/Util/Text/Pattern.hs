{-# LANGUAGE BangPatterns #-}

module Unison.Util.Text.Pattern where

import Data.Char (isDigit, isLetter, isPunctuation, isSpace)
import qualified Data.Text as DT
import Unison.Util.Text (Text)
import qualified Unison.Util.Text as Text

data Pattern
  = Join [Pattern] -- sequencing of patterns
  | Or Pattern Pattern -- left-biased choice: tries second pattern only if first fails
  | Capture Pattern -- capture all the text consumed by the inner pattern, discarding its subcaptures
  | Many Pattern -- zero or more repetitions (at least 1 can be written: Join [p, Many p])
  | Replicate Int Int Pattern -- m to n occurrences of a pattern, optional = 0-1
  | AnyChar -- consume a single char
  | Eof -- succeed if given the empty text, fail otherwise
  | Literal Text -- succeed if input starts with the given text, advance by that text
  | CharRange Char Char -- consume 1 char in the given range, or fail
  | CharIn [Char] -- consume 1 char in the given set, or fail
  | NotCharIn [Char] -- consume 1 char NOT in the given set, or fail
  | NotCharRange Char Char -- consume 1 char NOT in the given range, or fail
  | Digit -- consume 1 digit (according to Char.isDigit)
  | Letter -- consume 1 letter (according to Char.isLetter)
  | Space -- consume 1 space character (according to Char.isSpace)
  | Punctuation -- consume 1 punctuation char (according to Char.isPunctuation)
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
   in \t -> cp (Empty emptyCaptures) t

data Stack = Empty !Captures | Mark !Captures !Text !Stack

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

appendCaptures :: Captures -> Captures -> Captures
appendCaptures c1 c2 = c1 . c2
{-# INLINE appendCaptures #-}

emptyCaptures :: Captures
emptyCaptures = const []

capturesToList :: Captures -> [Text]
capturesToList c = c []

-- data Stack = Empty [Text] | Mark [Text] !Text !Stack
-- in `Or`: push a mark, then try left branch, if it succeeds,
--          merge top captures into the thing below, and pop from stack
--          if it fails, just pop the top mark from the stack
-- can just pop from the stack until hitting the correct mark
-- but then that leaves marks on the stack, even when the left branch succeeded
--
-- Pattern a -> ([a] -> a -> r) -> ... -- might need a takeable and droppable interface if go this route
compile :: Pattern -> (Stack -> Text -> r) -> (Stack -> Text -> r) -> Stack -> Text -> r
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
compile AnyChar !err !success = go
  where
    go acc t = case Text.drop 1 t of
      rem
        | Text.size t > Text.size rem -> success acc rem
        | otherwise -> err acc rem
compile (Capture (Many AnyChar)) !_ !success = \acc t -> success (pushCapture t acc) Text.empty
compile (Capture c) !err !success = go
  where
    err' _ _ acc0 t0 = err acc0 t0
    success' _ rem acc0 t0 = success (pushCapture (Text.take (Text.size t0 - Text.size rem) t0) acc0) rem
    compiled = compile c err' success'
    go acc t = compiled acc t acc t
compile (Or p1 p2) err success = cp1'
  where
    err' stk _ = case stk of
      Mark _ rem stk -> err stk rem
      _ -> error "pattern compiler bug"
    success' stk rem = case stk of
      Mark caps _ stk -> success (pushCaptures caps stk) rem
      _ -> error "pattern compiler bug"
    cp2 = compile p2 err' success'
    cp1 = compile p1 cp2 success'
    cp1' stk rem = cp1 (Mark (stackCaptures stk) rem stk) rem
compile (Join ps) !err !success = go ps
  where
    go [] = success
    go (p : ps) =
      let pc = compile p err psc
          psc = compile (Join ps) err success
       in pc
compile (NotCharIn cs) !err !success = go
  where
    ok = charNotInPred cs
    go acc t = case Text.uncons t of
      Just (ch, rem) | ok ch -> success acc rem
      _ -> err acc t
compile (CharIn cs) !err !success = go
  where
    ok = charInPred cs
    go acc t = case Text.uncons t of
      Just (ch, rem) | ok ch -> success acc rem
      _ -> err acc t
compile (CharRange c1 c2) !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | ch >= c1 && ch <= c2 -> success acc rem
      _ -> err acc t
compile (NotCharRange c1 c2) !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | not (ch >= c1 && ch <= c2) -> success acc rem
      _ -> err acc t
compile (Many p) !_ !success = case p of
  AnyChar -> (\acc _ -> success acc Text.empty)
  CharIn cs -> walker (charInPred cs)
  NotCharIn cs -> walker (charNotInPred cs)
  Digit -> walker isDigit
  Letter -> walker isLetter
  Punctuation -> walker isPunctuation
  Space -> walker isSpace
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
  AnyChar -> \acc t ->
    if Text.size t < m
      then err acc t
      else success acc (Text.drop n t)
  CharIn cs -> dropper (charInPred cs)
  NotCharIn cs -> dropper (charNotInPred cs)
  Digit -> dropper isDigit
  Letter -> dropper isLetter
  Punctuation -> dropper isPunctuation
  Space -> dropper isSpace
  _ -> go1 m
  where
    go1 0 = go2 (n - m)
    go1 n = compile p err (go1 (n - 1))
    go2 0 = success
    go2 n = compile p success (go2 (n - 1))

    dropper ok acc t
      | (i, rest) <- Text.dropWhileMax ok n t, i >= m = success acc rest
      | otherwise = err acc t
compile Digit !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isDigit ch -> success acc rem
      _ -> err acc t
compile Letter !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isLetter ch -> success acc rem
      _ -> err acc t
compile Punctuation !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isPunctuation ch -> success acc rem
      _ -> err acc t
compile Space !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isSpace ch -> success acc rem
      _ -> err acc t

charInPred, charNotInPred :: [Char] -> Char -> Bool
charInPred [] = const False
charInPred (c : chs) = let ok = charInPred chs in \ci -> ci == c || ok ci
charNotInPred [] = const True
charNotInPred (c : chs) = let ok = charNotInPred chs in (\ci -> ci /= c && ok ci)
