{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Unison.Test.ColorText where

-- import EasyTest
import qualified Data.Set                  as Set
import           Text.RawString.QQ
import           Unison.Lexer              (Pos (..))
import           Unison.Util.AnnotatedText (AnnotatedExcerpt (..), markup)
import           Unison.Util.ColorText     (Color (..), Rendered,
                                            renderExcerptWithColor)
import           Unison.Util.Range         (Range (..))

ex2 :: AnnotatedExcerpt Color
ex2 = markup ex (Set.fromList
      [ (Range (Pos 3 1) (Pos 3 5), Color2) -- SCENE
      , (Range (Pos 5 1) (Pos 5 5), Color1) -- Enter
      , (Range (Pos 25 1) (Pos 25 6), Color2) -- ALONSO
      , (Range (Pos 12 1) (Pos 13 44), Color1) -- Good, ... bestir.
      ])

renderEx2 :: Rendered
renderEx2 = renderExcerptWithColor ex2

ex3 :: AnnotatedExcerpt Color
ex3 = markup "Hello, world!" $ Set.fromList
        [ (Range (Pos 1 8) (Pos 1 12), Color1)
        , (Range (Pos 1 1) (Pos 1 5), Color2) ]

ex4 :: AnnotatedExcerpt Color
ex4 = markup "Hello,\nworld!" $ Set.fromList
        [ (Range (Pos 2 1) (Pos 2 5), Color1)
        , (Range (Pos 1 1) (Pos 1 5), Color2) ]

ex :: Ord a => AnnotatedExcerpt a
ex = [r|The Tempest | Act 1, Scene 1

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
