{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}

module Unison.Util.ColorText (ANSI, Color (..), ColorText, Rendered, style, renderText)
where

import           Data.Foldable             (asum, foldl')
import           Data.Sequence             (Seq)
import           Data.String               (IsString (..))
import qualified System.Console.ANSI       as ANSI
import           Unison.Util.AnnotatedText (AnnotatedText,
                                            pattern AnnotatedText', reannotate)


type ColorText = AnnotatedText Color

data ANSI

data Color
  =  Black | Red | Green | Yellow | Blue | Purple | Cyan | White
  | HiBlack| HiRed | HiGreen | HiYellow | HiBlue | HiPurple | HiCyan | HiWhite
  | Bold
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

style :: Color -> ColorText -> ColorText
style = reannotate

renderText :: ColorText -> Rendered ANSI
renderText (AnnotatedText' chunks) =
  (snd $ foldl' go (Nothing, mempty) chunks) <> resetANSI
  where go :: (Maybe Color, Rendered ANSI) -> (String, Maybe Color) -> (Maybe Color, Rendered ANSI)
        go (prev, r) (text, new) =
          if prev == new then (prev, r <> fromString text)
          else (new, case new of
            Nothing    -> r <> resetANSI <> fromString text
            Just style -> r <> resetANSI <> toANSI style <> fromString text)

        resetANSI :: Rendered ANSI
        resetANSI = Rendered . pure . ANSI.setSGRCode $ [ANSI.Reset]

        toANSI :: Color -> Rendered ANSI
        toANSI c = Rendered . pure . ANSI.setSGRCode $ case c of
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

newtype Rendered a = Rendered { _rawRender :: Seq String }
  deriving (Semigroup, Monoid)

instance Show (Rendered a) where
  show (Rendered chunks) = asum chunks

instance IsString (Rendered ANSI) where
  fromString s = Rendered (pure s)
