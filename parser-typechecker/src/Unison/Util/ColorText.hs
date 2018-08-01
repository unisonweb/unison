{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Unison.Util.ColorText where

-- import qualified System.Console.ANSI as A
-- import Control.Monad (join)
-- import Data.Foldable (toList)
import           Data.Foldable             (asum, foldl', toList)
import qualified Data.List                 as List
import           Data.Sequence             (Seq)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
-- import qualified Data.Sequence as Seq
-- import           Control.Exception (assert)
import           Data.String               (IsString (..))
import           Safe                      (headMay)
import           System.Console.ANSI       (pattern Blue, pattern Foreground,
                                            pattern Green, pattern Red,
                                            pattern Reset, pattern SetColor,
                                            pattern Vivid, setSGRCode)
import           Unison.Lexer              (Line, Pos (..))
import           Unison.Util.AnnotatedText
import           Unison.Util.Range         (Range (..), inRange)

data Color = Color1 | Color2 | Color3 deriving (Eq, Ord, Show)
type StyledText = AnnotatedText (Maybe Color)
type StyledBlockquote = AnnotatedExcerpt Color

toANSI :: Color -> Rendered
toANSI c = Rendered . pure . setSGRCode $ case c of
  Color1 -> [SetColor Foreground Vivid Red]
  Color2 -> [SetColor Foreground Vivid Blue]
  Color3 -> [SetColor Foreground Vivid Green]

resetANSI :: Rendered
resetANSI = Rendered . pure . setSGRCode $ [Reset]

newtype Rendered = Rendered (Seq String)

deoffsetRange :: Line -> Range -> Range
deoffsetRange lineOffset (Range (Pos startLine startCol) (Pos endLine endCol)) =
  Range (Pos (startLine - lineOffset + 1) startCol)
        (Pos (endLine - lineOffset + 1) endCol)

-- | drops lines and replaces with "." if there are more than `n` unannotated
-- | lines in a row.
splitAndRenderWithColor :: Int -> StyledBlockquote -> Rendered
splitAndRenderWithColor n e =
  mconcat $ List.intersperse
              "    .\n"
              (renderExcerptWithColor <$> snipWithContext n e)

renderExcerptWithColor :: StyledBlockquote -> Rendered
renderExcerptWithColor e =
  track (Pos line1 1) [] (Set.toList $ annotations e)
    (Rendered . pure $ renderLineNumber line1) (text e)
  where
    line1 :: Int
    line1 = lineOffset e
    renderLineNumber n = " " ++ replicate (lineNumberWidth - length sn) ' ' ++ sn ++ " | " where sn = show n
    lineNumberWidth = 4 --length (show maxLineIndex)
     -- where maxLineIndex = line1 - 1 + length (lines (text e))
    setupNewLine :: Rendered -> Pos -> Char -> (Rendered, Pos)
    setupNewLine openColor (Pos line col) c = case c of
      '\n' -> let r = Rendered . pure $ renderLineNumber (line + 1)
              in (r <> openColor, Pos (line + 1) 1)
      _ -> (mempty, Pos line (col + 1))
    track :: Pos -> [(Color, Pos)] -> [(Range, Color)] -> Rendered -> String -> Rendered
    track _pos stack _annotations rendered "" =
      rendered <> if null stack then mempty else resetANSI
    track pos stack annotations rendered _input@(c:rest) =
      let -- get whichever annotations may now be open
          (poppedAnnotations, remainingAnnotations) = span (inRange pos . fst) annotations
          -- drop any stack entries that will be closed after this char
          stack0 = dropWhile ((<=pos) . snd) stack
          -- and add new stack entries
          stack' = foldl' pushColor stack0 poppedAnnotations
            where pushColor s (Range _ end, color) = (color, end) : s
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
        (rendered <> newChar <> resetColor) rest

snipWithContext :: Ord a => Int -> AnnotatedExcerpt a -> [AnnotatedExcerpt a]
snipWithContext margin source =
  case foldl' whileWithinMargin
              (Nothing, mempty, mempty)
              (Set.toList $ annotations source) of
    (Nothing, _, _) -> []
    (Just (Range (Pos startLine' _) (Pos endLine' _)), group', rest') ->
      let dropLineCount = startLine' - lineOffset source
          takeLineCount = endLine' - startLine' + 1
          text', text2' :: [String]
          (text', text2') =
            splitAt takeLineCount (drop dropLineCount (lines (text source)))
      in AnnotatedExcerpt startLine' (unlines text') group'
        : snipWithContext
            margin (AnnotatedExcerpt (endLine' + 1) (unlines text2') rest')
  where
    withinMargin :: Range -> Range -> Bool
    withinMargin (Range _start1 (Pos end1 _)) (Range (Pos start2 _) _end2) =
      end1 + margin >= start2

    whileWithinMargin :: Ord a
                      => (Maybe Range, Set (Range, a), Set (Range, a))
                      -> (Range, a)
                      -> (Maybe Range, Set (Range, a), Set (Range, a))
    whileWithinMargin (r0, taken, rest) a@(r1,_) =
      case r0 of
        Nothing -> -- haven't processed any annotations yet
          (Just r1, Set.singleton a, mempty)
        Just r0 ->
          if null rest -- if all annotations so far can be joined without .. separations
          then if withinMargin r0 r1 -- if this one can be joined to the compare region without .. separation
            then (Just $ r0 <> r1, Set.insert a taken, mempty) -- add it to the first set and grow the compare region
            else (Just r0, taken, Set.singleton a) -- otherwise add it to the second set
          else (Just r0, taken, Set.insert a rest) -- once we've added to the second set, anything more goes there too

renderStyleTextWithColor :: StyledText -> Rendered
renderStyleTextWithColor (AnnotatedText chunks) = foldl' go mempty chunks
  where go :: Rendered -> (String, Maybe Color) -> Rendered
        go r (text, Nothing)    = r <> resetANSI <> fromString text
        go r (text, Just color) = r <> toANSI color <> fromString text

renderDocInColor :: AnnotatedDocument Color -> Rendered
renderDocInColor (AnnotatedDocument chunks) = go $ toList chunks where
  go [] = mempty
  go (Blockquote exc : rest) = renderExcerptWithColor exc <> go rest
  go (Text t : rest@(Blockquote _ : _)) =
    renderStyleTextWithColor t
      <> if trailingNewLine t then mempty else "\n"
      <> go rest
  go (Text t : rest) = renderStyleTextWithColor t <> go rest

{-

   1    | foo : Int
   2    | foo = 42
❌ 3:80 |
> Hello, world!
  ^^^^^
> Goodbye, world!
  ^^^^^^^

Highlight: Line 1, Cols 1-5
Highlight: Line 2, Cols 1-7
-}

unhighlighted :: StyledText -> StyledText
unhighlighted s = const Nothing <$> s

color :: Color -> StyledText -> StyledText
color c s = const (Just c) <$> s

color1 :: StyledText -> StyledText
color1 s = const (Just Color1) <$> s

color2 :: StyledText -> StyledText
color2 s = const (Just Color2) <$> s

color3 :: StyledText -> StyledText
color3 s = const (Just Color3) <$> s

-- data AnnotatedText
--   = Line { line :: String -- cannot contain newlines
--          , overline :: Maybe String
--          , underline :: Maybe String
--          , colorRegions :: Map (Column) }
-- --       , colorline :: String

-- * Don't print out source that isn't related to the error
-- * Color regions that are related to the error
-- * Insert lines with carets in place of using colors in some cases

  -- vvvvv
  -- Foo
  --

instance Show Rendered where
  show (Rendered chunks) = asum chunks

instance Semigroup Rendered where
  (<>) = mappend

instance Monoid Rendered where
  mempty = Rendered mempty
  mappend (Rendered chunks) (Rendered chunks') = Rendered (chunks <> chunks')

instance IsString Rendered where
  fromString s = Rendered (pure s)
