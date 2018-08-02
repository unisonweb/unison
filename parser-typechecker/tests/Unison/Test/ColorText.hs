{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Unison.Test.ColorText where

-- import EasyTest
import qualified Data.Set as Set
import           Text.RawString.QQ
import           Unison.Lexer (Pos (..))
import           Unison.Util.AnnotatedText (AnnotatedExcerpt (..), Rendered,
                                            excerptToDoc, markup)
import           Unison.Util.ColorText (ANSI, Style (..), renderDocANSI)
import           Unison.Util.Range (Range (..))

ex2 :: AnnotatedExcerpt Style
ex2 = markup ex (Set.fromList
      [ (Range (Pos 3 1) (Pos 3 5), ErrorSite) -- SCENE
      , (Range (Pos 5 9) (Pos 5 14), Type1) -- Master
      , (Range (Pos 5 22) (Pos 5 30), Type1) -- Boatswain
      , (Range (Pos 25 1) (Pos 25 6), ErrorSite) -- ALONSO
      , (Range (Pos 12 30) (Pos 13 27), Type2) -- fall ... aground.
      ])

renderEx2 :: Rendered ANSI
renderEx2 = renderDocANSI 3 . excerptToDoc $ ex2

ex3 :: AnnotatedExcerpt Style
ex3 = markup "Hello, world!" $ Set.fromList
        [ (Range (Pos 1 8) (Pos 1 12), Type1)
        , (Range (Pos 1 1) (Pos 1 5), Type2) ]

ex4 :: AnnotatedExcerpt Style
ex4 = markup "Hello,\nworld!" $ Set.fromList
        [ (Range (Pos 2 1) (Pos 2 5), Type1)
        , (Range (Pos 1 1) (Pos 1 5), Type2) ]

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
