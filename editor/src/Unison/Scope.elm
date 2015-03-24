module Unison.Scope where

import Debug
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

{- type Scope =
  Scope { focus : Path
        , up : Maybe Scope
        , down : Maybe Scope
        , left : Maybe Scope
        , right : Maybe Scope }
-}

type alias Scope = { focus : Path, ups : List Path, downs : List Path }
type alias Model = Maybe Scope

type alias Action = Model -> Model

scope : Path -> Scope
scope focus = Scope focus [] []

view : Layout View.L -> Scope -> Maybe Region
view layout scope =
   let startsWith l1 l2 =
         let btoi b = if b then 1 else 0
         in Path.startsWith l1.path l2.path && btoi l1.selectable <= btoi l2.selectable
   in Layout.region startsWith identity layout { path = scope.focus, selectable = True }
      |> Layout.selectableLub .selectable

-- sample on movement change
movement : Term -> Movement.D2 -> Action
movement e (Movement.D2 leftRight upDown) =
  (if upDown == Movement.Positive then up else identity) >>
  (if upDown == Movement.Negative then down e else identity) >>
  (if leftRight == Movement.Positive then right e else identity) >>
  (if leftRight == Movement.Negative then left e else identity)

-- sample on mouse position change
mouse : (Int,Int) -> Layout { a | path : Path } -> Action
mouse (x,y) layout =
  let paths = Layout.atRanked (List.length << .path) layout (Region { x = x, y = y } 2 2)
  in case paths of
    (h :: _) :: _ -> always (Just (scope h.path))
    _ -> always Nothing

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
