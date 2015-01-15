module Unison.Scope where

import Elmz.Layout (Region, Layout)
import Elmz.Layout as Layout
import Elmz.Signal as Signals
import Elmz.Movement as Movement
import Graphics.Element (Element)
import Graphics.Element as Element
import List
import List ((::))
import Maybe
import Signal
import Unison.Path as Path
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term

type alias E = Path.E
type alias Path = Path.Path

type alias Scope = { focus : Path, ups : List Path, downs : List Path }

type alias Action = Scope -> Scope

scope : Path -> Scope
scope focus = Scope focus [] []

view : Term -> (Path -> Maybe Region) -> Scope -> Element
view e region scope = case region scope.focus of
  Nothing -> Element.empty
  Just r ->
    let bounds = Layout.bounds r
    in Element.container bounds.width
                         bounds.height
                         (Element.topLeftAt (Element.absolute (r.topLeft.x))
                                            (Element.absolute (r.topLeft.y)))
                         (Styles.highlight r.width r.height)

actions : Signal Term
       -> Signal (Layout { a | path : Path })
       -> Signal (Int,Int)
       -> Signal Movement.D2
       -> Signal Action
actions e layout mouse movement =
  Signals.mergeWithBoth
    (resets mouse layout)
    (movements e movement)

resets : Signal (Int,Int) -> Signal (Layout { a | path : Path }) -> Signal Action
resets mouse layout =
  let go (x,y) layout =
    let paths = Layout.atRanked (List.length << .path) layout (Region { x = x, y = y } 2 2)
    in case paths of
      (h :: _) :: _ -> always (scope h.path)
      _ -> identity
  in Signal.sampleOn mouse (Signal.map2 go mouse layout)

movements : Signal Term -> Signal Movement.D2 -> Signal Action
movements e d2s =
  let go e (Movement.D2 upDown leftRight) =
    (if upDown == Movement.Positive then up else identity) >>
    (if upDown == Movement.Negative then down e else identity) >>
    (if leftRight == Movement.Positive then right e else identity) >>
    (if leftRight == Movement.Negative then left e else identity)
  in Signal.sampleOn d2s (Signal.map2 go e d2s)

up : Action
up {focus,ups,downs} = case ups of
  h :: ups -> Scope h ups (focus :: downs)
  [] -> let f = Term.up focus
        in if f == focus then Scope focus ups downs
           else Scope f [] (focus :: downs)

down : Term -> Action
down e {focus,ups,downs} = case downs of
  h :: downs -> Scope h (focus :: ups) downs
  [] -> let f = Term.down e focus
        in if f == focus then Scope focus ups downs
           else Scope f (focus :: ups) []

left : Term -> Action
left e {focus,ups,downs} =
  let p = Term.siblingL e focus
  in if p == focus then Scope focus ups downs
     else Scope p [] []

right : Term -> Action
right e {focus,ups,downs} =
  let p = Term.siblingR e focus
  in if p == focus then Scope focus ups downs
     else Scope p [] []
