module Unison.Util.ColorText where

-- import qualified System.Console.ANSI as A
-- import Control.Monad (join)
-- import Data.Foldable (toList)
import Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
import Data.String (IsString(..))

data Color = Normal | Color1 | Color2

newtype ColorText = ColorText { chunks :: Seq (Color,String) }

{-

   1    | foo : Int
   2    | foo = 42
❌ 3:80 |
> Hello, world!
^^^^^
Goodbye, world!
^^^^^^^

Highlight: Line 1, Cols 1-5
Highlight: Line 2, Cols 1-7
-}

color1 :: ColorText -> ColorText
color1 s = ColorText $ fmap (\(_,s) -> (Color1,s)) (chunks s)

color2 :: ColorText -> ColorText
color2 s = ColorText $ fmap (\(_,s) -> (Color2,s)) (chunks s)

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

instance IsString ColorText where
  fromString s = ColorText (pure (Normal, s))


instance Monoid ColorText where
  mempty = ColorText mempty
  mappend x y = ColorText $ chunks x `mappend` chunks y

instance Semigroup ColorText where
  (<>) = mappend

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
