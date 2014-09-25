module Unison.Scope where

import Unison.Path as Path

import Unison.Term (Term)
import Unison.Term as Term
type Path = Path.Path

type Scope = { focus : Path, ups : [Path], downs : [Path] }

scope : Path -> Scope
scope focus = Scope focus [] []

up : Scope -> Scope
up {focus,ups,downs} = case ups of
  h :: ups -> Scope h ups (focus :: downs)
  [] -> let f = Term.up focus
        in if f == focus then Scope focus ups downs
           else Scope f [] (focus :: downs)

down : Term -> Scope -> Scope
down e {focus,ups,downs} = case downs of
  h :: downs -> Scope h (focus :: ups) downs
  [] -> let f = Term.down e focus
        in if f == focus then Scope focus ups downs
           else Scope f (focus :: ups) []

left : Term -> Scope -> Scope
left e {focus,ups,downs} =
  let p = Term.siblingL e focus
  in if p == focus then Scope focus ups downs
     else Scope p [] []

right : Term -> Scope -> Scope
right e {focus,ups,downs} =
  let p = Term.siblingR e focus
  in if p == focus then Scope focus ups downs
     else Scope p [] []
