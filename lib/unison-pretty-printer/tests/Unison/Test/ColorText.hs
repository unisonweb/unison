{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.ColorText where

-- import EasyTest
import qualified Data.Map as Map
import EasyTest
import Text.RawString.QQ
import Unison.Lexer.Pos (Pos (..))
import Unison.Util.AnnotatedText
  ( AnnotatedExcerpt (..),
    condensedExcerptToText,
    markup,
  )
import Unison.Util.ColorText (Color (..), toANSI)
import qualified Unison.Util.ColorText as ColorText
import Unison.Util.Range (Range (..))

test :: Test ()
test =
  scope "colortext" . tests $
    []

-- commented out because they don't render exactly the same escape sequences, but they're equivalent4 as of this writing
-- scope "inclusive-exclusive range" . expect . trace ("ex4e: " ++ show (rawRender ex4e) ++ "\n" ++ "ex4t: " ++ show (rawRender ex4t) ++ "\n")$ ex4e == ex4t

ex4e :: String
ex4e = toANSI . condensedExcerptToText 1 $ markup "abc" m
  where
    m = Map.singleton (Range (Pos 1 2) (Pos 1 3)) Red

ex4t :: String
ex4t = toANSI $ "    1 | " <> "a" <> ColorText.style Red "b" <> "c" <> "\n"

ex2 :: AnnotatedExcerpt Color
ex2 =
  markup
    ex
    ( Map.fromList
        [ (Range (Pos 3 1) (Pos 3 5), Red), -- SCENE
          (Range (Pos 5 9) (Pos 5 14), Blue), -- Master
          (Range (Pos 5 22) (Pos 5 30), Blue), -- Boatswain
          (Range (Pos 25 1) (Pos 25 6), Red), -- ALONSO
          (Range (Pos 12 30) (Pos 13 27), Green) -- fall ... aground.
        ]
    )

renderEx2 :: String
renderEx2 = toANSI . condensedExcerptToText 3 $ ex2

ex3 :: AnnotatedExcerpt Color
ex3 =
  markup "Hello, world!" $
    Map.fromList
      [ (Range (Pos 1 8) (Pos 1 12), Blue),
        (Range (Pos 1 1) (Pos 1 5), Green)
      ]

ex4 :: AnnotatedExcerpt Color
ex4 =
  markup "Hello,\nworld!" $
    Map.fromList
      [ (Range (Pos 2 1) (Pos 2 5), Blue),
        (Range (Pos 1 1) (Pos 1 5), Green)
      ]

ex :: Ord a => AnnotatedExcerpt a
ex =
  [r|The Tempest | Act 1, Scene 1

SCENE I. On a ship at sea: a tempestuous noise
of thunder and lightning heard.
Enter a Master and a Boatswain

Master
Boatswain!
Boatswain
Here, master: what cheer?
Master
Good, speak to the mariners: fall to't, yarely,
or we run ourselves aground: bestir, bestir.
Exit

Enter Mariners

Boatswain
Heigh, my hearts! cheerly, cheerly, my hearts!
yare, yare! Take in the topsail. Tend to the
master's whistle. Blow, till thou burst thy wind,
if room enough!
Enter ALONSO, SEBASTIAN, ANTONIO, FERDINAND, GONZALO, and others

ALONSO
Good boatswain, have care. Where's the master?
Play the men.
Boatswain
I pray now, keep below.
|]

-- test = scope "colortext.snipWithContext" . expect $
