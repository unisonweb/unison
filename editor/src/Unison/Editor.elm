module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
import Elmz.Movement as Movement
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Maybe
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

type Event
  = Click (Int,Int)
  | Mouse (Int,Int)
  | Movement Movement.D2
  | Width Int
  | Act Action.Action
  | Enter
  | Delete
  | Preapply
  | ViewToggle
  | EvaluationResults (List { path : Path, old : Term, new : Term })
  | SearchResults Node.SearchResults
  | LocalInfoResults Node.LocalInfo
  | FieldContent Field.Content

type Request
  = ExplorerRequest TermExplorer.Request
  | EditRequest TermExplorer.LocalFocus Action.Action
  | Declare Term
  | Evaluations (List (Path, Term))
  | Metadatas (List Reference)

type alias Out = { term : Term, view : Element, request : Maybe Request }

type alias Model = Moore Event Out
type alias Sink a = a -> Signal.Message

model : Sink Field.Content -> Term -> Model
model sink term0 =
  let
    out term = { term = Moore.extract term |> .term
               , view = Moore.extract term |> .layout |> Layout.element
               , request = Nothing }
    toOpen mds term explorer scope =
      -- todo: need to store width, overrides, and raw toggle as well
      let
        focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
        env = View.env0 -- todo, then build this properly from the local names of the term + mds
        ex = Moore.feed explorer (TermExplorer.Open env focus Field.noContent)
        o = let r = out term in { r | request <- Maybe.map ExplorerRequest (Moore.extract ex |> .request) }
      in
        Moore o (exploreropen mds term ex)

    explorerclosed mds term explorer e = case e of
      -- these trigger a state change
      Click xy -> case Moore.feed term (EditableTerm.Mouse xy) of
        term -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen mds term explorer scope)
      Enter -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen mds term explorer scope)
      -- these dont
      Mouse xy -> Moore.step term (EditableTerm.Mouse xy) `Maybe.andThen` \term ->
        Just <| Moore (out term) (explorerclosed mds term explorer)
      Preapply -> Moore.step term (EditableTerm.Modify (Term.App Term.Blank)) `Maybe.andThen` \term ->
        Just <| Moore (out term) (explorerclosed mds term explorer)
      Act action -> (Moore.extract term |> .scope) `Maybe.andThen` \scope ->
        let
          focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
          r = out term
          o = { r | request <- Just (EditRequest focus action) }
        in
          Just <| Moore o (explorerclosed mds term explorer)
      -- Width w -> todo
      _ -> Nothing
    exploreropen mds term explorer e = Nothing
  in
    let
      terms0 = EditableTerm.model term0
      explorer0 = TermExplorer.model sink
    in
      Moore (out terms0) (explorerclosed Metadata.cache terms0 explorer0)

focusOpen : Event -> Maybe TermExplorer.Event
focusOpen _ = Nothing

focusClosed : Event -> Maybe EditableTerm.Event
focusClosed _ = Nothing
