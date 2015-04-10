module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
import Elmz.Movement as Movement
import Elmz.Selection1D as Selection1D
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
    offset term e = case Moore.extract term |> .selection of
      Nothing -> Element.empty
      Just region -> Styles.padNW region.topLeft.x (region.topLeft.y + region.height) e
    explorerXY term (x,y) = case Moore.extract term |> .selection of
      Nothing -> (x,y)
      Just region -> (x - region.topLeft.x, y - (region.topLeft.y + region.height))
    out term explorer =
      { term = Moore.extract term |> .term
      , view = Element.layers [ Moore.extract term |> .layout |> Layout.element
                              , offset term (Moore.extract explorer |> .view) ]
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
        o = let r = out term' ex in { r | request <- Maybe.map ExplorerRequest (Moore.extract ex |> .request) }
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
        Just <| Moore (out term explorer) (explorerclosed mds term explorer)
      Movement d2 -> stepX term (EditableTerm.Movement d2) `Maybe.andThen` \term ->
        Just <| Moore (out term explorer) (explorerclosed mds term explorer)
      Preapply -> stepX term (EditableTerm.Modify (Term.App Term.Blank)) `Maybe.andThen` \term ->
        Just <| Moore (out term explorer) (explorerclosed mds term explorer)
      Replace r -> stepX term (EditableTerm.Replace r) `Maybe.andThen` \term ->
        Just <| Moore (out term explorer) (explorerclosed mds term explorer)
      Act action -> (Moore.extract term |> .scope) `Maybe.andThen` \scope ->
        let
          focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
          r = out term explorer
          o = { r | request <- Just (EditRequest focus action) }
        in
          Just <| Moore o (explorerclosed mds term explorer)
      Width w -> Maybe.map (\term -> Moore (out term explorer) (explorerclosed mds term explorer))
                           (stepX term (EditableTerm.AvailableWidth w))
      _ -> Nothing
    exploreropen mds term explorer e = case e of
      -- these can trigger a state change
      Click xy ->
        let (x,y) = explorerXY term xy
            eview = Moore.extract explorer |> .view
        in if x < 0 || y < 0 || x > Element.widthOf eview || y > Element.heightOf eview
           then let ex = TermExplorer.model sink
                in Just <| Moore (out term ex) (explorerclosed mds term ex)
           else Moore.step explorer (TermExplorer.Click (x,y)) `Maybe.andThen`
                \explorer -> Nothing
      Enter -> Nothing
      FieldContent content -> Nothing
      -- these cannot
      Mouse xy ->
        let xy' = explorerXY term xy
        in Moore.step explorer (TermExplorer.Navigate (Selection1D.Mouse xy')) `Maybe.andThen`
           \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
      Width w ->
        Maybe.map (\term -> Moore (out term explorer) (exploreropen mds term explorer))
                  (stepX term (EditableTerm.AvailableWidth w))
      Movement d2 ->
        let d1 = Movement.negateD1 << Movement.xy_y <| d2
        in Moore.step explorer (TermExplorer.Navigate (Selection1D.Move d1)) `Maybe.andThen`
           \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
      SearchResults results ->
        Moore.step explorer (TermExplorer.SearchResults results) `Maybe.andThen`
        \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
      LocalInfoResults results ->
        Moore.step explorer (TermExplorer.LocalInfoResults results) `Maybe.andThen`
        \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
      _ -> Nothing
  in
    let
      terms0 = EditableTerm.model term0
      explorer0 = TermExplorer.model sink
    in
      Moore (out terms0 explorer0) (explorerclosed Metadata.cache terms0 explorer0)

focusOpen : Event -> Maybe TermExplorer.Event
focusOpen _ = Nothing

focusClosed : Event -> Maybe EditableTerm.Event
focusClosed _ = Nothing
