module Unison.Util.ColorText where

-- import qualified System.Console.ANSI as A
-- import Control.Monad (join)
-- import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
-- import qualified Data.Sequence as Seq
import Data.String (IsString(..))
import Unison.Lexer (Pos(..))

data Style = Normal | Highlighted Color

data Color = Color1 | Color2 deriving (Eq, Ord, Show)

newtype ColorText = ColorText { chunks :: Seq (Color,String) }

data Range = Range { start :: Pos, end :: Pos } deriving (Eq, Ord, Show)

contains :: Range -> Range -> Bool
contains (Range _ (Pos line2 col2)) (Range _ (Pos line4 col4)) =
           line4 < line2 || (line4 == line2 && col4 <= col2)

overlaps :: Range -> Range -> Bool
overlaps (Range _ (Pos line2 col2)) (Range (Pos line3 col3) _) =
           line2 > line3 || (line2 == line3 && col2 >= col3)


instance Semigroup Range where
  (Range start end) <> (Range start2 end2) =
    Range (min start start2) (max end end2)

deoffsetRange :: Int -> Range -> Range
deoffsetRange lineOffset (Range (Pos startLine startCol) (Pos endLine endCol)) =
  Range (Pos (startLine - lineOffset) startCol)
        (Pos (endLine - lineOffset) endCol)

data AnnotatedText =
  AnnotatedText { lineOffset :: Int
                , text :: String
                , annotations :: Set (Range, Color)
                } deriving (Show)

instance IsString AnnotatedText where
  fromString s = AnnotatedText 1 s mempty

snipWithContext :: Int -> AnnotatedText -> [AnnotatedText]
snipWithContext margin source =
  case foldl' whileWithinMargin
              (Nothing, mempty, mempty)
              (Set.toList $ annotations source) of
    (Nothing, _, _) -> []
    (Just (Range (Pos startLine' _) (Pos endLine' _)), group', rest') ->
      let text', text2' :: [String]
          (text', text2') =
            (splitAt (endLine' - startLine' + 1)
                     (drop (startLine' - lineOffset source)
                           (lines (text source))))
      in AnnotatedText startLine' (unlines text') group'
        : snipWithContext margin (AnnotatedText endLine' (unlines text2') rest')
  where
    withinMargin :: Range -> Range -> Bool
    withinMargin (Range _start1 (Pos l1 _)) (Range (Pos l2 _) _end2) =
      (l1 + margin >= l2)
    whileWithinMargin :: (Maybe Range, Set (Range, Color), Set (Range, Color))
                      -> (Range, Color)
                      -> (Maybe Range, Set (Range, Color), Set (Range, Color))
    whileWithinMargin (Nothing, _taken, rest) a@(r1, _) | null rest =
      (Just r1, Set.singleton a, mempty)
    whileWithinMargin (Just r0, taken, rest) a@(r1, _) | null rest =
      if withinMargin r0 r1
      then (Just $ r0 <> r1, Set.insert a taken, mempty)
      else (Just r0, taken, Set.singleton a)
    whileWithinMargin (r0, taken, rest) a = (r0, taken, Set.insert a rest)

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

-- [(Nothing,fromList [],fromList [])
-- ,(Just (Range {start = Pos 3 1, end = Pos 3 5}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1)],fromList [])
-- ,(Just (Range {start = Pos 3 1, end = Pos 3 5}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1)],fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1)])
-- ,(Just (Range {start = Pos 3 1, end = Pos 3 5}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1)],fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1),(Range {start = Pos 7 1, end = Pos 13 44},Color1)])
-- ,(Just (Range {start = Pos 3 1, end = Pos 3 5}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1)],fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1),(Range {start = Pos 7 1, end = Pos 13 44},Color1),(Range {start = Pos 14 1, end = Pos 14 4},Color2)])]
--
-- [AnnotatedText {lineOffset = 3, text = "SCENE I. On a ship at sea: a tempestuous noise\n", annotations = fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1)]}
-- ,AnnotatedText {lineOffset = 5, text = "\n", annotations = fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1)]}
-- ,AnnotatedText {lineOffset = 7, text = "Boatswain\nHere, master: what cheer?\nMaster\nGood, speak to the mariners: fall to't, yarely,\nor we run ourselves aground: bestir, bestir.\nExit\n\nEnter Mariners\n", annotations = fromList [(Range {start = Pos 7 1, end = Pos 13 44},Color1),(Range {start = Pos 14 1, end = Pos 14 4},Color2)]}
--
-- [(Nothing,fromList [],fromList [])
-- ,(Just (Range {start = Pos 5 1, end = Pos 5 5}),fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1)],fromList [])
-- ,(Just (Range {start = Pos 5 1, end = Pos 5 5}),fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1)],fromList [(Range {start = Pos 7 1, end = Pos 13 44},Color1)])
-- ,(Just (Range {start = Pos 5 1, end = Pos 5 5}),fromList [(Range {start = Pos 5 1, end = Pos 5 5},Color1)],fromList [(Range {start = Pos 7 1, end = Pos 13 44},Color1),(Range {start = Pos 14 1, end = Pos 14 4},Color2)])]
--
-- [(Nothing,fromList [],fromList [])
-- ,(Just (Range {start = Pos 7 1, end = Pos 13 44}),fromList [(Range {start = Pos 7 1, end = Pos 13 44},Color1)],fromList [])
-- ,(Just (Range {start = Pos 7 1, end = Pos 14 4}),fromList [(Range {start = Pos 7 1, end = Pos 13 44},Color1),(Range {start = Pos 14 1, end = Pos 14 4},Color2)],fromList [])]
--
-- [(Nothing,fromList [],fromList [])]
-- ]
--
--
-- [(Nothing,fromList [],fromList [])
-- ,(Just (Range {start = Pos 3 1, end = Pos 3 5}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1)],fromList [])
-- ,(Just (Range {start = Pos 3 1, end = Pos 5 5}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1),(Range {start = Pos 5 1, end = Pos 5 5},Color1)],fromList [])
-- ,(Just (Range {start = Pos 3 1, end = Pos 13 44}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1),(Range {start = Pos 5 1, end = Pos 5 5},Color1),(Range {start = Pos 7 1, end = Pos 13 44},Color1)],fromList [])
-- ,(Just (Range {start = Pos 3 1, end = Pos 14 4}),fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1),(Range {start = Pos 5 1, end = Pos 5 5},Color1),(Range {start = Pos 7 1, end = Pos 13 44},Color1),(Range {start = Pos 14 1, end = Pos 14 4},Color2)],fromList [])]
-- [AnnotatedText {lineOffset = 3, text = "SCENE I. On a ship at sea: a tempestuous noise\nof thunder and lightning heard.\nEnter a Master and a Boatswain\n\nMaster\nBoatswain!\nBoatswain\nHere, master: what cheer?\nMaster\nGood, speak to the mariners: fall to't, yarely,\nor we run ourselves aground: bestir, bestir.\nExit\n", annotations = fromList [(Range {start = Pos 3 1, end = Pos 3 5},Color1),(Range {start = Pos 5 1, end = Pos 5 5},Color1),(Range {start = Pos 7 1, end = Pos 13 44},Color1),(Range {start = Pos 14 1, end = Pos 14 4},Color2)]}[(Nothing,fromList [],fromList [])]
-- ]



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

-- Approach 1:
  -- Symbolically apply highlighting to the source
  --  and then automatically generate excerpts.
-- Approach 2:
  -- Manually specify excerpt ranges?

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
