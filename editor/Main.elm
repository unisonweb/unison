module Main where

import Unison.Components as U
import Unison.Term as L
import Unison.Metadata as MD
import Unison.Action
import Unison.Node as N

import Graphics.Input(..)
import Graphics.Input.Field(..)
import Window

-- each cell will consist of a single Element
-- 'standard' input boxes not really
-- flexible enough, since depending on
-- scope, are overwriting different region of
-- syntax tree
{-
source : Term -> Element

idea: have just one input box, at the top
alternately, place input box above selection,
with a caret pointing down

f x = [x + 1 + 2 + 3]
     -------------------
    |                   |
     -  ----------------
      \/
f x = [x + 1 + 2 + 3]
need to create an input box
-}

entry : Input Content
entry = input noContent

midnightBlue = rgb 44 62 80
turquoise = rgb 26 188 156
greenSea = rgb 22 160 133

fieldStyle =
  { padding = { left=8, right=8, top=11, bottom=12 }
  , outline = { color=midnightBlue, width=uniformly 3, radius=4 }
  , highlight = noHighlight
  , style = let t = Text.defaultStyle
            in { t | typeface <- ["Lato", "latin"], height <- Just 16 } }

fld = field fieldStyle

main : Signal Element
main =
  let scene (w,h) content = container w h middle (f content)
      f content = width 100 (fld entry.handle id "" content)
  in scene <~ Window.dimensions ~ entry.signal

