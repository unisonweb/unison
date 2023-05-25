{-# LANGUAGE AllowAmbiguousTypes #-}

module Unison.Util.Pattern.Text where

import Data.Char (isAlphaNum, isControl, isLetter, isLower, isMark, isNumber, isPrint, isPunctuation, isSeparator, isSpace, isSymbol, isUpper)
import Unison.Util.Pattern
import Unison.Util.Text
import qualified Unison.Util.Text as Text
data CharPattern
  = Any -- any char
  | Not CharPattern -- negation of the given pattern
  | Union CharPattern CharPattern -- match if either pattern matches
  | Intersect CharPattern CharPattern -- match if both patterns match
  | CharRange Char Char -- match if char is in the given range
  | CharSet [Char] -- match if char is in the given set
  | CharClass CharClass -- match if char is in the given class
  deriving (Show, Eq, Ord)


instance Compile Text CharPattern where
  compilePattern p !err !success = go
    where
      ok = charPatternPred p
      go acc bs = case Text.uncons bs of
        Nothing -> err acc bs
        Just (b, bs) -> if ok b then success acc bs else err acc bs
  compileSize = Text.size
  compileTake = Text.take
  compileDrop = Text.drop

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
