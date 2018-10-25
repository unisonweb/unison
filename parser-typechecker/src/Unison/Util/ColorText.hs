{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Unison.Util.ColorText
  (ANSI, StyledText, Color (..), style,
    renderDocANSI, renderText)
where

import           Data.Foldable (foldl', toList)
import qualified Data.Map as Map
import           Data.String (IsString (..))
import           Safe (headMay)
import qualified System.Console.ANSI as ANSI
import           Unison.Lexer (Pos (..))
import           Unison.Util.AnnotatedText (AnnotatedDocument (..),
                                            AnnotatedExcerpt,
                                            AnnotatedText (..), Rendered (..),
                                            Section (..), annotations,
                                            lineOffset, splitAndRender, text,
                                            trailingNewLine)
import           Unison.Util.Range (Range (..), inRange)

data ANSI
type StyledText = AnnotatedText (Maybe Color)
type StyledBlockquote = AnnotatedExcerpt Color

data Color
  =  Black | Red | Green | Yellow | Blue | Purple | Cyan | White
  | HiBlack| HiRed | HiGreen | HiYellow | HiBlue | HiPurple | HiCyan | HiWhite
  | Bold
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

_unhighlighted :: StyledText -> StyledText
_unhighlighted s = const Nothing <$> s

style :: a -> AnnotatedText (Maybe a) -> AnnotatedText (Maybe a)
style c s = const (Just c) <$> s

toANSI :: Color -> Rendered ANSI
toANSI c = Rendered . pure . ANSI.setSGRCode $ case c of
  Black -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
  Red -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red]
  Green -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
  Yellow -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Yellow]
  Blue -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue]
  Purple -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta]
  Cyan -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Cyan]
  White -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
  HiBlack -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
  HiRed -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  HiGreen -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  HiYellow -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  HiBlue -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  HiPurple -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Magenta]
  HiCyan -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
  HiWhite -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
  Bold -> [ANSI.SetConsoleIntensity ANSI.BoldIntensity]



resetANSI :: Rendered ANSI
resetANSI = Rendered . pure . ANSI.setSGRCode $ [ANSI.Reset]

renderText :: StyledText -> Rendered ANSI
renderText (AnnotatedText chunks) =
  foldl' (go Nothing) mempty chunks <> resetANSI
  where go :: Maybe Color -> Rendered ANSI -> (String, Maybe Color) -> Rendered ANSI
        go prev r (text, new) =
          if prev == new then r <> fromString text
          else case new of
            Nothing -> r <> resetANSI <> fromString text
            Just style -> r <> resetANSI <> toANSI style <> fromString text

renderDocANSI :: Int -> AnnotatedDocument Color -> Rendered ANSI
renderDocANSI excerptCollapseWidth (AnnotatedDocument chunks) =
  go $ toList chunks
  where
  go [] = mempty
  go (Blockquote exc : rest) =
    splitAndRender excerptCollapseWidth renderExcerpt exc <> go rest
  -- go (Describe style : rest) = go (Text (describe style) : rest)
  go (Text t : rest@(Blockquote _ : _)) =
    renderText t
      <> (if trailingNewLine t then mempty else "\n")
      <> go rest
  go (Text t : rest) = renderText t <> go rest

renderExcerpt :: StyledBlockquote -> Rendered ANSI
renderExcerpt e =
  track (Pos line1 1) [] (Map.toList $ annotations e)
    (Rendered . pure $ renderLineNumber line1) (text e)
  where
    line1 :: Int
    line1 = lineOffset e

    renderLineNumber n =
      " " ++ replicate (lineNumberWidth - length sn) ' ' ++ sn ++ " | "
      where sn = show n
            lineNumberWidth = 4

    setupNewLine :: Rendered ANSI -> Pos -> Char -> (Rendered ANSI, Pos)
    setupNewLine openColor (Pos line col) c = case c of
      '\n' -> let r = Rendered . pure $ renderLineNumber (line + 1)
              in (r <> openColor, Pos (line + 1) 1)
      _ -> (mempty, Pos line (col + 1))

    track :: Pos -> [(Color, Pos)] -> [(Range, Color)] -> Rendered ANSI -> String -> Rendered ANSI
    track _pos stack _annotations rendered _input@"" =
      rendered <> if null stack then mempty else resetANSI
    track pos stack annotations rendered _input@(c:rest) =
      let -- get whichever annotations may now be open
          (poppedAnnotations, remainingAnnotations) = span (inRange pos . fst) annotations
          -- drop any stack entries that will be closed after this char
          stack0 = dropWhile ((<=pos) . snd) stack
          -- and add new stack entries
          stack' = foldl' pushColor stack0 poppedAnnotations
            where pushColor s (Range _ end, style) = (style, end) : s
          resetColor = -- stack is newly null, and there are no newly opened annotations
            if null poppedAnnotations && null stack' && not (null stack)
            then resetANSI else mempty
          maybeColor = fst <$> headMay stack'
          openColor = maybe mempty toANSI maybeColor
          (lineHeader, pos') = setupNewLine openColor pos c
          lineHeader' = if null rest then mempty else lineHeader
          newChar =
            if c == '\n'
              then (Rendered . pure) [c] <> resetANSI <> lineHeader'
              else openColor <> (Rendered . pure) [c]
      in track pos' stack' remainingAnnotations
        (rendered <> resetColor <> newChar ) rest
