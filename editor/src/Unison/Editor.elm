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
  = Act Action.Action
  | Click (Int,Int)
  | Delete
  | Enter
  | EvaluationResults (List { path : Path, old : Term, new : Term })
  | FieldContent Field.Content
  | LocalInfoResults Node.LocalInfo
  | Mouse (Int,Int)
  | Movement Movement.D2
  | Preapply
  | Replace { path : Path, old : Term, new : Term }
  | SearchResults Node.SearchResults
  | ViewToggle
  | Width Int

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
      let
        focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
        env = { availableWidth = 500 -- could also compute based on term available width
              , metadata = Moore.extract mds
              , overrides = always Nothing
              , raw = Nothing }
        ex = Moore.feed explorer (TermExplorer.Open env focus Field.noContent)
        term' = Moore.feed term { event = Nothing, explorerOpen = True }
        o = let r = out term' in { r | request <- Maybe.map ExplorerRequest (Moore.extract ex |> .request) }
      in
        Moore o (exploreropen mds term' ex)

    feed b m e = Moore.feed m { event = Just e, explorerOpen = b }
    step b m e = Moore.step m { event = Just e, explorerOpen = b }
    (feedX, feed0, stepX, step0) = (feed False, feed True, step False, step True)

    explorerclosed mds term explorer e = case e of
      -- these trigger a state change
      Click xy -> case feedX term (EditableTerm.Mouse xy) of
        term -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen mds term explorer scope)
      Enter -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen mds term explorer scope)
      -- these dont
      Mouse xy -> stepX term (EditableTerm.Mouse xy) `Maybe.andThen` \term ->
        Just <| Moore (out term) (explorerclosed mds term explorer)
      Movement d2 -> stepX term (EditableTerm.Movement d2) `Maybe.andThen` \term ->
        Just <| Moore (out term) (explorerclosed mds term explorer)
      Preapply -> stepX term (EditableTerm.Modify (Term.App Term.Blank)) `Maybe.andThen` \term ->
        Just <| Moore (out term) (explorerclosed mds term explorer)
      Replace r -> stepX term (EditableTerm.Replace r) `Maybe.andThen` \term ->
        Just <| Moore (out term) (explorerclosed mds term explorer)
      Act action -> (Moore.extract term |> .scope) `Maybe.andThen` \scope ->
        let
          focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
          r = out term
          o = { r | request <- Just (EditRequest focus action) }
        in
          Just <| Moore o (explorerclosed mds term explorer)
      Width w -> Maybe.map (\term -> Moore (out term) (explorerclosed mds term explorer))
                           (stepX term (EditableTerm.AvailableWidth w))
      _ -> Nothing
    exploreropen mds term explorer e = case e of
      -- these can trigger a state change
      Click xy -> Nothing
      Enter -> Nothing
      FieldContent content -> Nothing
      -- these cannot
      Mouse xy -> Nothing
      Width w -> Nothing
      SearchResults results -> Nothing
      LocalInfoResults results -> Nothing
      Movement d2 -> Nothing
      _ -> Nothing
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
