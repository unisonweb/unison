module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Mealy as M
import Elmz.Mealy (Mealy)
import Elmz.Moore (Moore)
import Elmz.Moore as Moore
import Elmz.Movement as Movement
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Signal
import Unison.Action as Action
import Unison.Explorer as Explorer
import Unison.EditableTerm as EditableTerm
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
import Unison.TermExplorer as TermExplorer
import Unison.Terms as Terms
import Unison.Type (Type)
import Unison.Type as Type
import Unison.Var as Var
import Unison.View as View

type alias Inputs =
  { origin : (Int,Int)
  , clicks : Signal ()
  , mouse : Signal (Int,Int)
  , enters : Signal ()
  , edits : Signal Action.Action
  , deletes : Signal ()
  , preapplies : Signal ()
  , viewToggles : Signal ()
  , modifier : Signal Bool -- generally shift
  , movements : Signal Movement.D2
  , searchbox : Signal.Channel Field.Content
  , explorerHasFocus : Signal.Channel Bool
  , responses : Signal Response
  , width : Signal Int }

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

editor : Term -> Inputs -> Signal (Element, List Request)
editor t0 env =
  let
    -- explorerHasFocus : Signal.Channel Bool
    explorerOpen : Signal Bool
    explorerOpen =
      -- a click outside the explorer region closes if open
      -- a click inside noops if not pointing to valid completion
      -- a click inside noops
      todo

    -- Moore (Maybe Movement.D2, Maybe (Int,Int), Layout.L) Foo
    -- still have a loop due to path resolution depending
    -- on the current layout and the input mode
    -- EVERYTHING depends on the current layout
    --
    term' : Moore (Path, Term -> Term) Term
    term' = term t0

    viewEnv' : Signal Response -> Signal View.Env
    viewEnv' = Moore.transform viewEnv

    {-
    layout' : Signal Response
           -> Signal Path
           -> Signal (Maybe (Term -> Term))
           -> Signal (Layout View.L)
    layout' r p =
    -}

    -- explorer : Mealy (Either Input Response) (Element, List Request, Mode (Term -> Term))

    -- highlightedTermWithLayout : Mealy (View.Env, (Path, Term -> Term), )
  in
    todo

    -- term = { loc : Path, action : Term -> Term, term : Term } -> Term
--
viewEnv : Moore Response View.Env
viewEnv = todo --

term : Term -> Moore (Path, Term -> Term) Term
term t0 = editable Term.modify t0

todo = Debug.crash "todo"

editable : (k -> (v -> v) -> kvs -> Maybe kvs)
        -> kvs
        -> Moore (k, v -> v) kvs
editable _ _ = todo

