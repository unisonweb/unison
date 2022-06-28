{-# LANGUAGE BangPatterns #-}

module Unison.Util.Text.Pattern where

import Data.Char (isDigit, isLetter, isPunctuation, isSpace)
import qualified Data.Text as DT
import Unison.Util.Text (Text)
import qualified Unison.Util.Text as Text

data Pattern
  = Eof
  | Literal Text -- matches and consumes that exact text
  | Digit
  | Letter
  | Space
  | Punctuation
  | CharRange Char Char
  | CharIn [Char]
  | NotCharIn [Char]
  | NotCharRange Char Char
  | Join [Pattern]
  | Capture Pattern
  | Many Pattern

uncons :: Pattern -> Text -> Maybe ([Text], Text)
uncons p =
  let cp = compile p Nothing (\acc rem -> Just (reverse acc, rem))
   in \t -> cp [] t

compile :: Pattern -> r -> ([Text] -> Text -> r) -> [Text] -> Text -> r
compile !Eof !err !success = go
  where
    go acc t
      | Text.size t == 0 = success acc t
      | otherwise = err
compile (Literal txt) !err !success = go
  where
    go acc t
      | Text.take (Text.size txt) t == txt = success acc (Text.drop (Text.size txt) t)
      | otherwise = err
compile (Capture c) !err !success = go
  where
    compiled = compile c (\e _ -> e) (\acc rem -> \_ f -> f acc rem)
    go acc t = compiled acc t err success'
      where
        success' acc rem = success (Text.take (Text.size t - Text.size rem) t : acc) rem
compile (Join ps) !err !success = go ps
  where
    go [] = \acc t -> success acc t
    go (p : ps) =
      let pc = compile p err (\acc rem -> psc acc rem)
          psc = compile (Join ps) err success
       in pc
compile (NotCharIn cs) !err !success = go
  where
    ok = charNotInPred cs
    go acc t = case Text.uncons t of
      Just (ch, rem) | ok ch -> success acc rem
      _ -> err
compile (CharIn cs) !err !success = go
  where
    ok = charInPred cs
    go acc t = case Text.uncons t of
      Just (ch, rem) | ok ch -> success acc rem
      _ -> err
compile (CharRange c1 c2) !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | ch >= c1 && ch <= c2 -> success acc rem
      _ -> err
compile (NotCharRange c1 c2) !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | not (ch >= c1 && ch <= c2) -> success acc rem
      _ -> err
compile (Many p) !_ !success = case p of
  CharIn cs -> walker (charInPred cs)
  NotCharIn cs -> walker (charNotInPred cs)
  Digit -> walker isDigit
  Letter -> walker isLetter
  Punctuation -> walker isPunctuation
  Space -> walker isSpace
  p -> go
    where
      compiled = compile p (\e _ -> e) (\acc rem -> \_ f -> f acc rem)
      go acc t = compiled acc t (success acc t) go
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
      _ -> err
compile Letter !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isLetter ch -> success acc rem
      _ -> err
compile Punctuation !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isPunctuation ch -> success acc rem
      _ -> err
compile Space !err !success = go
  where
    go acc t = case Text.uncons t of
      Just (ch, rem) | isSpace ch -> success acc rem
      _ -> err

charInPred, charNotInPred :: [Char] -> Char -> Bool
charInPred [] = const False
charInPred (c : chs) = let ok = charInPred chs in \ci -> ci == c || ok ci
charNotInPred [] = const True
charNotInPred (c : chs) = let ok = charNotInPred chs in (\ci -> ci /= c && ok ci)
