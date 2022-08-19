{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Util.ColorText
  ( ColorText,
    Color (..),
    style,
    toANSI,
    toPlain,
    toHTML,
    defaultColors,
    black,
    red,
    green,
    yellow,
    blue,
    purple,
    cyan,
    white,
    hiBlack,
    hiRed,
    hiGreen,
    hiYellow,
    hiBlue,
    hiPurple,
    hiCyan,
    hiWhite,
    bold,
    underline,
    invert,
    background,
    unstyled,
    module Unison.Util.AnnotatedText,
  )
where

import qualified System.Console.ANSI as ANSI
import Unison.Prelude
import Unison.Util.AnnotatedText
  ( AnnotatedText (..),
    Segment (..),
    annotate,
    toPair,
  )
import qualified Unison.Util.SyntaxText as ST hiding (toPlain)

type ColorText = AnnotatedText Color

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | White
  | HiBlack
  | HiRed
  | HiGreen
  | HiYellow
  | HiBlue
  | HiPurple
  | HiCyan
  | HiWhite
  | Bold
  | Underline
  | Invert Color
  | Background Color Color
  | Default
  deriving (Eq, Ord, Show, Read)

black, red, green, yellow, blue, purple, cyan, white, hiBlack, hiRed, hiGreen, hiYellow, hiBlue, hiPurple, hiCyan, hiWhite, bold, underline :: ColorText -> ColorText
black = style Black
red = style Red
green = style Green
yellow = style Yellow
blue = style Blue
purple = style Purple
cyan = style Cyan
white = style White
hiBlack = style HiBlack
hiRed = style HiRed
hiGreen = style HiGreen
hiYellow = style HiYellow
hiBlue = style HiBlue
hiPurple = style HiPurple
hiCyan = style HiCyan
hiWhite = style HiWhite
bold = style Bold
underline = style Underline

unstyled :: ColorText -> ColorText
unstyled = style Default

background :: Color -> ColorText -> ColorText
background c ct = ct <&> Background c

invert :: ColorText -> ColorText
invert ct = ct <&> Invert

style :: Color -> ColorText -> ColorText
style = annotate

toHTML :: String -> ColorText -> String
toHTML cssPrefix (AnnotatedText at) =
  toList at >>= \case
    Segment s color -> wrap color (s >>= newlineToBreak)
  where
    newlineToBreak '\n' = "<br/>\n"
    newlineToBreak ch = [ch]
    wrap Nothing s = "<code>" <> s <> "</code>"
    wrap (Just c) s =
      "<code class=" <> colorName c <> ">" <> s <> "</code>"
    colorName c = "\"" <> cssPrefix <> "-" <> show c <> "\""

-- Convert a `ColorText` to a `String`, ignoring colors
toPlain :: ColorText -> String
toPlain (AnnotatedText at) = join (toList $ segment <$> at)

-- Convert a `ColorText` to a `String`, using ANSI codes to produce colors
toANSI :: ColorText -> String
toANSI (AnnotatedText chunks) =
  join . toList $ snd (foldl' go (Nothing, mempty) chunks) <> resetANSI
  where
    go ::
      (Maybe Color, Seq String) ->
      Segment Color ->
      (Maybe Color, Seq String)
    go (prev, r) (toPair -> (text, new)) =
      if prev == new
        then (prev, r <> pure text)
        else
          ( new,
            case new of
              Nothing -> r <> resetANSI <> pure text
              Just style -> r <> resetANSI <> toANSI style <> pure text
          )
    -- End every ANSI escape code in the prompt with '\STX' so that haskeline renders it properly on Windows.
    -- This may also help haskeline make prompt width computations.
    -- See https://github.com/judah/haskeline/wiki/ControlSequencesInPrompt for details.
    startOfText = "\STX"
    resetANSI = pure $ ANSI.setSGRCode [ANSI.Reset] <> startOfText
    toANSI :: Color -> Seq String
    toANSI c = pure $ ANSI.setSGRCode (toANSI' c) <> startOfText

    toANSI' :: Color -> [ANSI.SGR]
    toANSI' c = case c of
      Default -> []
      Background c c2 -> (setBg <$> toANSI' c) <> toANSI' c2
        where
          setBg (ANSI.SetColor ANSI.Foreground intensity color) =
            ANSI.SetColor ANSI.Background intensity color
          setBg sgr = sgr
      Invert c -> [ANSI.SetSwapForegroundBackground True] <> toANSI' c
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
      Underline -> [ANSI.SetUnderlining ANSI.SingleUnderline]

defaultColors :: ST.Element r -> Maybe Color
defaultColors = \case
  ST.NumericLiteral -> Nothing
  ST.TextLiteral -> Nothing
  ST.BytesLiteral -> Just HiBlack
  ST.CharLiteral -> Nothing
  ST.BooleanLiteral -> Nothing
  ST.Blank -> Nothing
  ST.Var -> Nothing
  ST.TypeReference _ -> Nothing
  ST.TermReference _ -> Nothing
  ST.Op _ -> Nothing
  ST.Unit -> Nothing
  ST.AbilityBraces -> Just HiBlack
  ST.ControlKeyword -> Just Bold
  ST.LinkKeyword -> Just HiBlack
  ST.TypeOperator -> Just HiBlack
  ST.BindingEquals -> Nothing
  ST.TypeAscriptionColon -> Just Blue
  ST.DataTypeKeyword -> Nothing
  ST.DataTypeParams -> Nothing
  ST.DataTypeModifier -> Nothing
  ST.UseKeyword -> Just HiBlack
  ST.UsePrefix -> Just HiBlack
  ST.UseSuffix -> Just HiBlack
  ST.HashQualifier _ -> Just HiBlack
  ST.DelayForceChar -> Just Yellow
  ST.DelimiterChar -> Nothing
  ST.Parenthesis -> Nothing
  ST.DocDelimiter -> Just Green
  ST.DocKeyword -> Just Bold
