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

uncons :: Pattern -> Text -> Maybe ([Text], Text)
uncons p =
  let cp = compile p (\_ _ -> Nothing) (\acc rem -> Just (reverse acc, rem))
   in \t -> cp [] t

compile :: Pattern -> ([Text] -> Text -> r) -> ([Text] -> Text -> r) -> [Text] -> Text -> r
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
compile (Capture (Many AnyChar)) !_ !success = \acc t -> success (t : acc) Text.empty
compile (Capture c) !err !success = go
  where
    err' _ _ acc0 t0 = err acc0 t0
    success' _ rem acc0 t0 = success (Text.take (Text.size t0 - Text.size rem) t0 : acc0) rem
    compiled = compile c err' success'
    go acc t = compiled acc t acc t
compile (Or p1 p2) err success = cp1
  where
    cp2 = compile p2 err success
    cp1 = compile p1 cp2 success
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
  p -> let go = compile p success (\acc rem -> go acc rem) in go
  where
    walker ok = go
      where
        go acc t = case Text.unconsChunk t of
          Nothing -> success acc t
          Just (Text.chunkToText -> txt, t) -> case DT.dropWhile ok txt of
            rem
              | DT.null rem -> go acc t
              | otherwise -> success acc (Text.fromText rem <> t)
    {-# INLINE walker #-}
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
