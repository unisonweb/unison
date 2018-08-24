{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Unison.Util.ColorText where

import           Data.Foldable (foldl', toList)
import qualified Data.Set as Set
import           Data.String (IsString (..))
import           Safe (headMay)
import           System.Console.ANSI (pattern Blue, pattern BoldIntensity,
                                      pattern Dull, pattern Foreground,
                                      pattern Green, pattern Red, pattern Reset,
                                      pattern SetColor,
                                      pattern SetConsoleIntensity,
                                      pattern SetUnderlining,
                                      pattern SingleUnderline, pattern Vivid,
                                      setSGRCode)
import           Unison.Lexer (Pos (..))
import           Unison.Util.AnnotatedText (AnnotatedDocument (..),
                                            AnnotatedExcerpt,
                                            AnnotatedText (..), Rendered (..),
                                            Section (..), annotations,
                                            lineOffset, splitAndRender, text,
                                            trailingNewLine)
import           Unison.Util.Range (Range (..), inRange)

data ANSI
data ASCII
data Style = ForceShow | Type1 | Type2 | ErrorSite deriving (Eq, Ord, Show)
type StyledText = AnnotatedText (Maybe Style)
type StyledBlockquote = AnnotatedExcerpt Style

unhighlighted :: StyledText -> StyledText
unhighlighted s = const Nothing <$> s

style :: Style -> StyledText -> StyledText
style c s = const (Just c) <$> s

type1 :: StyledText -> StyledText
type1 s = const (Just Type1) <$> s

type2 :: StyledText -> StyledText
type2 s = const (Just Type2) <$> s

errorSite :: StyledText -> StyledText
errorSite s = const (Just ErrorSite) <$> s

renderDocANSI :: Int -> AnnotatedDocument Style -> Rendered ANSI
renderDocANSI excerptCollapseWidth (AnnotatedDocument chunks) =
  go $ toList chunks
  where
  go [] = mempty
  go (Blockquote exc : rest) =
    splitAndRender excerptCollapseWidth renderExcerpt exc <> go rest
  go (Describe style : rest) = go (Text (describe style) : rest)
  go (Text t : rest@(Blockquote _ : _)) =
    renderText t
      <> (if trailingNewLine t then mempty else "\n")
      <> go rest
  go (Text t : rest) = renderText t <> go rest

  describe :: Style -> StyledText
  describe ErrorSite = "in " <> errorSite "red"
  describe Type1     = "in " <> type1 "blue"
  describe Type2     = "in " <> type2 "green"
  describe ForceShow = mempty
  toANSI :: Style -> Rendered ANSI
  toANSI c = Rendered . pure . setSGRCode $ case c of
    ErrorSite -> [red]
    Type1     -> [blue]
    Type2     -> [green]
    ForceShow -> []
    where red = SetColor Foreground Vivid Red
          blue = SetColor Foreground Vivid Blue
          green = SetColor Foreground Dull Green
          _bold = SetConsoleIntensity BoldIntensity
          _underline = SetUnderlining SingleUnderline

  resetANSI :: Rendered ANSI
  resetANSI = Rendered . pure . setSGRCode $ [Reset]

  renderText :: StyledText -> Rendered ANSI
  renderText (AnnotatedText chunks) = foldl' go mempty chunks
    where go :: Rendered ANSI -> (String, Maybe Style) -> Rendered ANSI
          go r (text, Nothing)    = r <> resetANSI <> fromString text
          go r (text, Just style) = r <> toANSI style <> fromString text

  renderExcerpt :: StyledBlockquote -> Rendered ANSI
  renderExcerpt e =
    track (Pos line1 1) [] (Set.toList $ annotations e)
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

      track :: Pos -> [(Style, Pos)] -> [(Range, Style)] -> Rendered ANSI -> String -> Rendered ANSI
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
