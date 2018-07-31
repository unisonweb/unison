{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Unison.Test.ColorText where

-- import EasyTest
import           Text.RawString.QQ
import Unison.Util.ColorText
import Data.Set (Set, union)
import qualified Data.Set as Set
import Unison.Lexer (Pos(..))

markup :: AnnotatedText -> Set (Range, Color) -> AnnotatedText
markup a r = a { annotations = r `union` (annotations a) }

ex2 :: AnnotatedText
ex2 = markup ex (Set.fromList
      [ (Range (Pos 3 1) (Pos 3 5), Color1)
      , (Range (Pos 5 1) (Pos 5 5), Color1)
      , (Range (Pos 14 1) (Pos 14 4), Color2)
      , (Range (Pos 7 1) (Pos 13 44), Color1)
      ])
      
ex :: AnnotatedText
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
ANTONIO
Where is the master, boatswain?
Boatswain
Do you not hear him? You mar our labour: keep your
cabins: you do assist the storm.
GONZALO
Nay, good, be patient.
Boatswain
When the sea is. Hence! What cares these roarers
for the name of king? To cabin: silence! trouble us not.
GONZALO
Good, yet remember whom thou hast aboard.
Boatswain
None that I more love than myself. You are a
counsellor; if you can command these elements to
silence, and work the peace of the present, we will
not hand a rope more; use your authority: if you
cannot, give thanks you have lived so long, and make
yourself ready in your cabin for the mischance of
the hour, if it so hap. Cheerly, good hearts! Out
of our way, I say.
Exit

GONZALO
I have great comfort from this fellow: methinks he
hath no drowning mark upon him; his complexion is
perfect gallows. Stand fast, good Fate, to his
hanging: make the rope of his destiny our cable,
for our own doth little advantage. If he be not
born to be hanged, our case is miserable.
Exeunt

Re-enter Boatswain

Boatswain
Down with the topmast! yare! lower, lower! Bring
her to try with main-course.
A cry within

A plague upon this howling! they are louder than
the weather or our office.
Re-enter SEBASTIAN, ANTONIO, and GONZALO

Yet again! what do you here? Shall we give o'er
and drown? Have you a mind to sink?
SEBASTIAN
A pox o' your throat, you bawling, blasphemous,
incharitable dog!
Boatswain
Work you then.
ANTONIO
Hang, cur! hang, you whoreson, insolent noisemaker!
We are less afraid to be drowned than thou art.
GONZALO
I'll warrant him for drowning; though the ship were
no stronger than a nutshell and as leaky as an
unstanched wench.
Boatswain
Lay her a-hold, a-hold! set her two courses off to
sea again; lay her off.
Enter Mariners wet

Mariners
All lost! to prayers, to prayers! all lost!
Boatswain
What, must our mouths be cold?
GONZALO
The king and prince at prayers! let's assist them,
For our case is as theirs.
SEBASTIAN
I'm out of patience.
ANTONIO
We are merely cheated of our lives by drunkards:
This wide-chapp'd rascal--would thou mightst lie drowning
The washing of ten tides!
GONZALO
He'll be hang'd yet,
Though every drop of water swear against it
And gape at widest to glut him.
A confused noise within: 'Mercy on us!'-- 'We split, we split!'--'Farewell, my wife and children!'-- 'Farewell, brother!'--'We split, we split, we split!'

ANTONIO
Let's all sink with the king.
SEBASTIAN
Let's take leave of him.
Exeunt ANTONIO and SEBASTIAN

GONZALO
Now would I give a thousand furlongs of sea for an
acre of barren ground, long heath, brown furze, any
thing. The wills above be done! but I would fain
die a dry death.
Exeunt
|]
-- test = scope "colortext.snipWithContext" . expect $
