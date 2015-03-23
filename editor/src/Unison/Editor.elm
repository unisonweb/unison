module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Moore (Moore)
import Elmz.Moore as Moore
import Elmz.Mealy as M
import Elmz.Mealy (Mealy)
import Graphics.Element (Element)
import Graphics.Element as Element
import Unison.Action as Action
import Unison.Explorer as Explorer
import Unison.SearchboxParser as SearchboxParser
import Unison.Hash (Hash)
import Unison.Metadata (Metadata)
import Unison.Metadata as Metadata
import Unison.Node as Node
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Reference (Reference)
import Unison.Reference as Reference
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.Terms as Terms
import Unison.Type (Type)
import Unison.Type as Type
import Unison.Var as Var
import Unison.View as View

type Input
  = Keys (List Int)
  | Tap Bool
  | Click Bool
  | Cursor (Int,Int)

type Request
  = LocalRequest Term Path -- obtain the current and admissible type and local completions
  | Search Term Path Int (Maybe Type) Metadata.Query -- global search for a given type
  | Declare Term
  | Edit Path Path Action.Action Term
  | Evaluations (List (Path, Term))
  | Metadatas (List Reference)

type Response
  = LocalResponse Node.LocalInfo
  | SearchResults Node.SearchResults

type Mode a
  = Open a
  | Closed
  | Cancelled

type alias Either a b = Result a b

accepts : Mealy (Mode a) (Maybe a)
accepts =
  let f prev cur = case (prev,cur) of
        (Open a, Closed) -> Just a
        _ -> Nothing
  in M.changesBy f

editor : Term -> Moore (Either Input Response) (Element, List Request)
editor t0 =
  let
    term' : Mealy (Path, Term -> Term) Term
    term' = Moore.feed (term t0)

    termWithLayout : Mealy (View.Env, (Path, Term -> Term)) (Term, Layout View.L)
    termWithLayout =
      let f (env, term) = (term, View.layout term env)
      in M.map f (M.second term')

    termWithLayout' : Mealy (Response, (Path, Term -> Term)) (Term, Layout View.L)
    termWithLayout' =
      M.first viewEnv `M.pipe` termWithLayout
    -- explorer : Mealy (Either Input Response) (Element, List Request, Mode (Term -> Term))

    -- highlightedTermWithLayout : Mealy (View.Env, (Path, Term -> Term), )
  in
    todo

    -- term = { loc : Path, action : Term -> Term, term : Term } -> Term
--
viewEnv : Mealy Response View.Env
viewEnv = todo --

term : Term -> Moore (Path, Term -> Term) Term
term t0 = editable Term.modify t0

todo = Debug.crash "todo"

selection : Moore (Input, (Term, Layout View.L)) (Maybe Path)
selection = todo

highlightLayer : Mealy (Maybe Path, Layout View.L) Element
highlightLayer = todo

editable : (k -> (v -> v) -> kvs -> Maybe kvs)
        -> kvs
        -> Moore (k, v -> v) kvs
editable _ _ = todo

explorer : Mealy (Either Input Response) (Element, List Request, Mode (Term -> Term))
explorer = todo

{-
Moore (Input, Response) (Element, Request)

output of a Moore can't be an Action/Update, as this doesn't play well
with using `map2` and leads to duplicate updates

output of a moore should always just be the new state, not a transition
input can be some description of a transition
`editable` has this property, as does `selection`
explorer does not


type Out a
  = Accept a
  | Cancel
  | Hold

type ExplorerInfo
  = Local Node.LocalInfo
  | Global Node.SearchResults

-- metadata?
-- up to someone else to detect transition from Open to Closed
-- and act on the last `Open` value
explorer : Moore (Either Input ExplorerInfo) (Element, Maybe Request, Mode (Term -> Term))

switch : Moore i (Either a b) -> Moore a o -> Moore b o -> Moore i o

modal : (a -> Maybe m) -> Moore (i,r) a -> Moore (i,m) (Mode r) -> Moore i a

latest : b -> (a -> Maybe b) -> Moore a b

-}
