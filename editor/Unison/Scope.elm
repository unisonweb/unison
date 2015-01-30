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
import Unison.View as View

type alias E = Path.E
type alias Path = Path.Path

type alias Scope = { focus : Path, ups : List Path, downs : List Path }
type alias Model = Maybe Scope

type alias Action = Model -> Model

scope : Path -> Scope
scope focus = Scope focus [] []

view : { tl | term : Term, layout : Layout View.L } -> Scope -> (Layout View.L, Maybe Region)
view ctx scope =
  let highlighted : Maybe Region
      highlighted = Layout.region Path.startsWith .path ctx.layout scope.focus
                 |> Layout.selectableLub .selectable
   in case highlighted of
        Nothing -> (ctx.layout, highlighted)
        Just region ->
          let l = Layout.transform (\e -> Element.layers [e, Styles.selection region])
                                   ctx.layout
          in (l, highlighted)

-- sample on movement change
movement : Term -> Movement.D2 -> Action
movement e (Movement.D2 upDown leftRight) =
  (if upDown == Movement.Positive then up else identity) >>
  (if upDown == Movement.Negative then down e else identity) >>
  (if leftRight == Movement.Positive then right e else identity) >>
  (if leftRight == Movement.Negative then left e else identity)

-- sample on mouse position change
reset : (Int,Int) -> Layout { a | path : Path } -> Action
reset (x,y) layout =
  let paths = Layout.atRanked (List.length << .path) layout (Region { x = x, y = y } 2 2)
  in case paths of
    (h :: _) :: _ -> always (Just (scope h.path))
    _ -> always Nothing

-- track information about *when* to update separate from *what* this means

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
      (h :: _) :: _ -> always (Just (scope h.path))
      _ -> always Nothing
  in Signal.sampleOn mouse (Signal.map2 go mouse layout)

movements : Signal Term -> Signal Movement.D2 -> Signal Action
movements e d2s =
  Signal.sampleOn d2s (Signal.map2 movement e d2s)

up : Action
up m = case m of
  Nothing -> Nothing
  Just {focus,ups,downs} -> Just (case ups of
    h :: ups -> Scope h ups (focus :: downs)
    [] -> let f = Term.up focus
          in if f == focus then Scope focus ups downs
             else Scope f [] (focus :: downs)
  )

down : Term -> Action
down e m = case m of
  Nothing -> Nothing
  Just {focus,ups,downs} -> Just (case downs of
    h :: downs -> Scope h (focus :: ups) downs
    [] -> let f = Term.down e focus
          in if f == focus then Scope focus ups downs
             else Scope f (focus :: ups) []
  )

left : Term -> Action
left e m = case m of
  Nothing -> Nothing
  Just {focus,ups,downs} -> Just (
    let p = Term.siblingL e focus
    in if p == focus then Scope focus ups downs
       else Scope p [] []
  )

right : Term -> Action
right e m = case m of
  Nothing -> Nothing
  Just {focus,ups,downs} -> Just (
    let p = Term.siblingR e focus
    in if p == focus then Scope focus ups downs
       else Scope p [] []
  )
