module Main where

import Unison.Components as U
import Unison.Term as L
import Unison.Metadata as MD
import Unison.Path
import Unison.Action
import Unison.Node as N

import Graphics.Input(..)
import Graphics.Collage (..)

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

ok : Input Bool
ok = input False

msg : Signal String
msg =
  let f x = case x of
    True -> "Yep"
    False -> "Nope"
  in lift f ok.signal

sq : Element
sq = collage 50 50 [filled blue (square 30)]
  |> hoverable ok.handle id

scene msg =
  flow down [
    toText msg |> style U.body |> centered,
    sq
  ]

main : Signal Element
main = scene <~ msg
