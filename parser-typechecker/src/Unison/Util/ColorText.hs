{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}

module Unison.Util.ColorText (
  ColorText, Color(..), style, toANSI, toPlain,
  black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold,
  module Unison.Util.AnnotatedText)
where

import Control.Monad (join)
import           Data.Foldable             (foldl', toList)
import           Data.Sequence             (Seq)
import qualified System.Console.ANSI       as ANSI
import           Unison.Util.AnnotatedText (AnnotatedText(..), annotate)

type ColorText = AnnotatedText Color

data Color
  =  Black | Red | Green | Yellow | Blue | Purple | Cyan | White
  | HiBlack| HiRed | HiGreen | HiYellow | HiBlue | HiPurple | HiCyan | HiWhite
  | Bold
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold :: ColorText -> ColorText
black t = style Black t
red t = style Red t
green t = style Green t
yellow t = style Yellow t
blue t = style Blue t
purple t = style Purple t
cyan t = style Cyan t
white t = style White t
hiBlack t = style HiBlack t
hiRed t = style HiRed t
hiGreen t = style HiGreen t
hiYellow t = style HiYellow t
hiBlue t = style HiBlue t
hiPurple t = style HiPurple t
hiCyan t = style HiCyan t
hiWhite t = style HiWhite t
bold t = style Bold t

style :: Color -> ColorText -> ColorText
style = annotate

-- Convert a `ColorText` to a `String`, ignoring colors
toPlain :: ColorText -> String
toPlain (AnnotatedText at) = join (toList $ fst <$> at)

-- Convert a `ColorText` to a `String`, using ANSI codes to produce colors
toANSI :: ColorText -> String
toANSI (AnnotatedText chunks) = join . toList $
  (snd $ foldl' go (Nothing, mempty) chunks) <> resetANSI
  where go :: (Maybe Color, Seq String) -> (String, Maybe Color) -> (Maybe Color, Seq String)
        go (prev, r) (text, new) =
          if prev == new then (prev, r <> pure text)
          else (new, case new of
            Nothing    -> r <> resetANSI <> pure text
            Just style -> r <> resetANSI <> toANSI style <> pure text)

        resetANSI = pure . ANSI.setSGRCode $ [ANSI.Reset]

        toANSI :: Color -> Seq String
        toANSI c = pure . ANSI.setSGRCode $ case c of
          Black    -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
          Red      -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
          Green    -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
          Yellow   -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
          Blue     -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue]
          Purple   -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta]
          Cyan     -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
          White    -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
          HiBlack  -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
          HiRed    -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
          HiGreen  -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
          HiYellow -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
          HiBlue   -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
          HiPurple -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta]
          HiCyan   -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
          HiWhite  -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
          Bold     -> [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
