{-# LANGUAGE PatternSynonyms #-}
module Unison.Util.ColorText where

-- import qualified System.Console.ANSI as A
-- import Control.Monad (join)
-- import Data.Foldable (toList)
import           Data.Foldable     (foldl')
import           Data.Sequence     (Seq)
import           Data.Set          (Set)
import qualified Data.Set          as Set
-- import qualified Data.Sequence as Seq
-- import           Control.Exception (assert)
import           Data.String       (IsString (..))
import           Text.Printf       (printf)
import           Unison.Lexer      (Line, Pos (..))
import Safe (headMay)
import System.Console.ANSI (setSGRCode, pattern SetColor, pattern Reset, pattern Foreground, pattern Vivid, pattern Red, pattern Blue)

-- data Style = Normal | Highlighted Color

data Color = Color1 | Color2 deriving (Eq, Ord, Show)

toANSI :: Color -> Rendered
toANSI Color1 = Rendered . pure . setSGRCode $ [SetColor Foreground Vivid Red]
toANSI Color2 = Rendered . pure . setSGRCode $ [SetColor Foreground Vivid Blue]

resetANSI :: Rendered
resetANSI = Rendered . pure . setSGRCode $ [Reset]

type ColorText = AnnotatedText Color
type ColorExcerpt = AnnotatedExcerpt Color

newtype AnnotatedText a = AnnotatedText { chunks :: Seq (a, String) }
newtype Rendered = Rendered { chunks' :: Seq String }

data Range = Range { start :: Pos, end :: Pos } deriving (Eq, Ord, Show)

contains :: Range -> Range -> Bool
contains (Range _ (Pos line2 col2)) (Range _ (Pos line4 col4)) =
           line4 < line2 || (line4 == line2 && col4 <= col2)

overlaps :: Range -> Range -> Bool
overlaps (Range _ (Pos line2 col2)) (Range (Pos line3 col3) _) =
           line2 > line3 || (line2 == line3 && col2 >= col3)

inRange :: Pos -> Range -> Bool
inRange p r = contains r (Range p p)

isMultiLine :: Range -> Bool
isMultiLine (Range (Pos startLine _) (Pos endLine _)) = startLine < endLine

deoffsetRange :: Line -> Range -> Range
deoffsetRange lineOffset (Range (Pos startLine startCol) (Pos endLine endCol)) =
  Range (Pos (startLine - lineOffset + 1) startCol)
        (Pos (endLine - lineOffset + 1) endCol)

-- ugly because annotation ranges
renderExcerptWithColor :: ColorExcerpt -> Rendered
renderExcerptWithColor e =
  track (Pos 1 1) [] (Set.toList $ annotations e) mempty (text e)
  where
    lineNumberWidth = length (show maxLineIndex)
      where maxLineIndex = lineOffset e - 1 + length (lines (text e))
    newColor stack = maybe mempty toANSI (fst <$> headMay stack)
    checkNewline (Pos line col) c = if c == '\n'
      then (Rendered . pure $ printf " % *d | " lineNumberWidth (line + 1)
            , Pos (line + 1) 1)
      else (mempty, Pos line (col + 1))
    track :: Pos -> [(Color, Pos)] -> [(Range, Color)] -> Rendered -> String -> Rendered
    -- no more input, no formatting on the stack
    track _ [] _ rendered "" = rendered
    -- no more input, reset to clear formatting stack
    track _ (_:_) _ rendered "" = rendered <> resetANSI
    -- no more annotations and all open annotations are finished
    track _ [] [] rendered input = rendered <> Rendered (pure input)
    -- no more annotations, but need to close some annotations
    track pos stack [] rendered (c:rest) =
      let stack' = dropWhile ((<=pos) . snd) stack
          (lineHeader, pos') = checkNewline pos c
          lineHeader' = if null rest then mempty else lineHeader
      in track pos' stack' [] (lineHeader' <> rendered <> newColor stack') rest
    -- no open annotations, but more annotations available
    track pos stack annotations rendered (c:rest) =
      let (poppedAnnotations, remainingAnnotations) =
            span (inRange pos . fst) annotations
          stack' = foldl' pushColor (dropWhile ((<=pos) . snd) stack) poppedAnnotations
            where pushColor stack (Range _ end, color) = (color, end) : stack
          (lineHeader, pos') = checkNewline pos c
          lineHeader' = if null rest then mempty else lineHeader
      in track pos' stack' remainingAnnotations
          (lineHeader' <> rendered <> newColor stack') rest
    -- open annotations and more annotations available, need to check what order to do stuff
    track _pos@(Pos _line _col) _colorStack _annotations _rendered _input =
      -- let (poppedAnnotations, remainingAnnotations) =
            -- span (inRange pos . fst) annotations
            error ""



data AnnotatedExcerpt a = AnnotatedExcerpt
  { lineOffset  :: Line
  , text        :: String
  , annotations :: Set (Range, a)
  } deriving (Show)

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


-- color1 :: ColorText -> ColorText
-- color1 s = ColorText $ fmap (\(_,s) -> (Color1,s)) (chunks s)
--
-- color2 :: ColorText -> ColorText
-- color2 s = ColorText $ fmap (\(_,s) -> (Color2,s)) (chunks s)

-- highlight :: [(String,Int)] -> Map (Pos,Pos) Color -> ColorText
-- highlight s regions = ColorText _ where

-- highlight' :: String -> RenderInColor -> Map (Pos,Pos) Color -> (StartLine, EndLine) -> String
-- highlight' source highlightsForAParticularErrorMessage [(3, 3), (6, 6), (9, 10)]

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


-- colorAt :: ColorText -> Map Pos Color

-- excerpt :: [Line] -> ColorText -> ColorText
--

-- instance IsString ColorText where
--   fromString s = ColorText (pure (Normal, s))


-- instance Monoid ColorText where
--   mempty = ColorText mempty
--   mappend x y = ColorText $ chunks x `mappend` chunks y

-- instance Semigroup ColorText where
--   (<>) = mappend

-- instance Show ColorText where
--   show = renderAnntatedText Nothing . renderInColor

-- renderInColor :: ColorText -> AnnotatedText

-- showExcerpts :: AnnotatedText -> Maybe [(Line,Line)] -> String
-- showExcerpts t Nothing = _
-- showExcerpts _ _ = _todo

-- represent a String as a Map (Pos,Pos) (Char, Color)

-- newtype ColorText = ColorText {
-- render :: RenderInColor -> Color -> Pos -> (Seq String, Pos) }

-- go False _ = pure s
-- go True Normal = pure s
-- go True Color1 = setColor s A.Red
-- go True Color2 = setColor s A.Blue
-- setColor s c = Seq.fromList
--   [ A.setSGRCode [A.SetColor A.Foreground A.Dull c]
--   , s
--   , A.setSGRCode [A.Reset]
--   ]

instance Semigroup Range where
  (Range start end) <> (Range start2 end2) =
    Range (min start start2) (max end end2)

instance Semigroup Rendered where
  (<>) = mappend

instance Monoid Rendered where
  mempty = Rendered mempty
  mappend r1@(Rendered chunks) r2@(Rendered chunks') =
    if (null chunks) then r2
    else if (null chunks') then r1
    else Rendered (chunks <> chunks')

instance Ord a => IsString (AnnotatedExcerpt a) where
  fromString s = AnnotatedExcerpt 1 s mempty
