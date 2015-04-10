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
  | EvaluationResults (List { path : Path, old : Term, new : Term }) -- todo, but can put this off
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

type alias In = { event : Maybe Event, availableWidth : Int }
type alias Out = { term : Term, view : Element, request : Maybe Request }

type alias Model = Moore In Out
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
      , request = Maybe.map ExplorerRequest (Moore.extract explorer |> .request) }
    toOpen w mds term explorer scope =
      let
        focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
        env = { availableWidth = 500 -- could also compute based on term available width
              , metadata = Moore.extract mds
              , overrides = always Nothing
              , raw = Nothing }
        ex = Moore.feed explorer (TermExplorer.Open env focus Field.noContent)
        term' = Moore.feed term { event = Nothing, explorerOpen = True, availableWidth = w }
        o = let r = out term' ex in { r | request <- Maybe.map ExplorerRequest (Moore.extract ex |> .request) }
      in
        Moore o (exploreropen mds term' ex)

    feedX w m e = Moore.feed m { event = Just e, explorerOpen = False, availableWidth = w }
    stepX w m e = Moore.step m { event = Just e, explorerOpen = False, availableWidth = w }

    explorerclosed mds term explorer e = case (e.event, e.availableWidth) of
      (Nothing,w) -> Maybe.map
        (\term -> Moore (out term explorer) (explorerclosed mds term explorer))
        (Moore.step term { event = Nothing, explorerOpen = False, availableWidth = w })
      (Just event,w) -> case event of
        -- these trigger a state change
        Click xy -> case feedX w term (EditableTerm.Mouse xy) of
          term -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen w mds term explorer scope)
        Enter -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen w mds term explorer scope)
        -- these dont
        Mouse xy -> stepX w term (EditableTerm.Mouse xy) `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Movement d2 -> stepX w term (EditableTerm.Movement d2) `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Preapply -> stepX w term (EditableTerm.Modify (Term.App Term.Blank)) `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Replace r -> stepX w term (EditableTerm.Replace r) `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Act action -> (Moore.extract term |> .scope) `Maybe.andThen` \scope ->
          let
            focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
            r = out term explorer
            o = { r | request <- Just (EditRequest focus action) }
          in
            Just <| Moore o (explorerclosed mds term explorer)
        _ -> Nothing

    ex0 = TermExplorer.model sink
    tryAccept w mds term explorer = explorer `Maybe.andThen`
      \explorer -> case Moore.extract explorer |> .selection of
        Nothing -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        Just (loc,replacement) ->
          let term' = Moore.feed term { event = Just (EditableTerm.Modify (always replacement))
                                      , explorerOpen = True
                                      , availableWidth = w }
          in Just <| Moore (out term' explorer) (explorerclosed mds term' ex0)

    exploreropen mds term explorer e = case (e.event,e.availableWidth) of
      (Nothing,w) -> Maybe.map
        (\term -> Moore (out term explorer) (exploreropen mds term explorer))
        (Moore.step term { event = Nothing, explorerOpen = True, availableWidth = w })
      (Just event,w) -> case event of
        -- these can trigger a state change
        Click xy ->
          let (x,y) = explorerXY term xy
              eview = Moore.extract explorer |> .view
          in if x < 0 || y < 0 || x > Element.widthOf eview || y > Element.heightOf eview
             then Just <| Moore (out term ex0) (explorerclosed mds term ex0)
             else tryAccept w mds term (Moore.step explorer (TermExplorer.Click (x,y)))
        Enter -> tryAccept w mds term (Moore.step explorer TermExplorer.Enter)
        FieldContent content -> tryAccept w mds term (Moore.step explorer (TermExplorer.FieldContent content))
        -- these cannot
        Mouse xy ->
          let xy' = explorerXY term xy
          in Moore.step explorer (TermExplorer.Navigate (Selection1D.Mouse xy')) `Maybe.andThen`
             \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
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
