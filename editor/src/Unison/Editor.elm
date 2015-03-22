module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Moore (Moore)
import Elmz.Moore as M
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

-- todo - implement this

accepts : Moore (Mode a) (Maybe a)
accepts =
  let f prev cur = case (prev,cur) of
        (Open a, Closed) -> Just a
        _ -> Nothing
  in M.changesBy f

-- switch to Mealy

editor : Term -> Moore (Either Input Response) (Element, List Request)
editor t0 =
  let
    term' : Moore (Path, Term -> Term) Term
    term' = term t0

    viewEnv0 : View.Env
    viewEnv0 = todo

    termWithLayout : Moore (View.Env, (Path, Term -> Term)) (Term, Layout View.L)
    termWithLayout = todo
      --let ts : Moore (View.Env, (Path, Term -> Term)) Term
      --    ts = M.contramap snd term'
      --    ts' : Moore (View.Env, (Path, Term -> Term)) (View.Env, Term)
      --    ts' = M.withInput (viewEnv0, [], identity) ts
      --       |> M.map (\((env,_,_), t) -> (env,t))
      --in M.split ts' `M.pipe2` layoutTerm
      --   |> M.map (\((_,t), l) -> (t,l))
  in
    todo

--
viewEnv : Moore Response View.Env
viewEnv = todo --

term : Term -> Moore (Path, Term -> Term) Term
term t0 = editable Term.modify t0

todo = Debug.crash "todo"

layoutTerm : Moore (View.Env, Term) (Layout View.L)
layoutTerm = todo

selection : Moore (Input, (Layout View.L, Term)) Path
selection = todo

highlightLayer : Moore (Path, Layout View.L) Element
highlightLayer = todo

editable : (k -> (v -> v) -> kvs -> Maybe kvs)
        -> kvs
        -> Moore (k, v -> v) kvs
editable _ _ = todo

explorer : Moore (Either Input Response) (Element, List Request, Mode (Term -> Term))
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
