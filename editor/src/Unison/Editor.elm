module Unison.Editor where

import Debug
import Execute
import Elmz.Json.Request as JR
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
import Elmz.Movement as Movement
import Elmz.Selection1D as Selection1D
import Elmz.Signal as Signals
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Maybe
import Mouse
import Keyboard
import Result
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
import Unison.View as View
import Window

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
  | MetadataResults (List (Reference.Key, Metadata))
  | ViewToggle

type Request
  = ExplorerRequest TermExplorer.Request
  | EditRequest TermExplorer.LocalFocus Action.Action
  | Declare Term
  | Evaluations (List (Path, Term))
  | Metadatas (List Reference)

type alias In = { event : Maybe Event, availableWidth : Int, topLeft : (Int,Int) }
type alias Out = { term : Term, view : Element, request : Maybe Request }

type alias Model = Moore In Out
type alias Sink a = a -> Signal.Message

moveDown : In -> In
moveDown i = { i | event <- Just (Movement (Movement.D2 Movement.Zero Movement.Negative)) }

model : Sink Field.Content -> Term -> Model
model sink term0 =
  let
    offset term e = case Moore.extract term |> .selection of
      Nothing -> Element.empty
      Just region -> Styles.padNW (region.topLeft.x - Styles.selectionBorderWidth) (region.topLeft.y + region.height) e
    explorerXY term (x,y) = case Moore.extract term |> .selection of
      Nothing -> (x,y)
      Just region -> (x - region.topLeft.x - Styles.selectionBorderWidth, y - (region.topLeft.y + region.height))
    out term explorer =
      let req = Maybe.map ExplorerRequest (Moore.extract explorer |> .request)
      in
        { term = Moore.extract term |> .term
        , view = Element.layers [ Moore.extract term |> .layout |> Layout.element
                                , offset term (Moore.extract explorer |> .view) ]
        , request = req }

    toOpen pt w mds term explorer scope =
      let
        focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
        env = { availableWidth = 500 -- could also compute based on term available width
              , metadata = Moore.extract mds
              , overrides = always Nothing
              , raw = Nothing }
        ex = Moore.feed (TermExplorer.Open env focus Field.noContent) explorer
        term' = Moore.feed { event = Nothing, explorerOpen = True
                           , availableWidth = w, metadata = env.metadata, topLeft = pt }
                           term
        o = let r = out term' ex in { r | request <- Maybe.map ExplorerRequest (Moore.extract ex |> .request) }
      in
        Moore.spike o { o | request <- Nothing } (exploreropen mds term' ex)

    feedX pt w md e m = Moore.feed { event = Just e, explorerOpen = False, availableWidth = w, metadata = md, topLeft = pt } m
    stepX pt w md e m = Moore.step { event = Just e, explorerOpen = False, availableWidth = w, metadata = md, topLeft = pt } m

    explorerclosed mds term explorer e = case (e.event, e.availableWidth, Moore.extract mds) of
      (Nothing,w,md) -> Maybe.map
        (\term -> let o = out term explorer in Moore { o | request <- Nothing } (explorerclosed mds term explorer))
        (Moore.step { event = Nothing, explorerOpen = False, availableWidth = w, metadata = md, topLeft = e.topLeft } term)
      (Just event,w,md) -> case event of
        -- these trigger a state change
        Click xy -> case feedX e.topLeft w md (EditableTerm.Mouse xy) term of
          term -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen e.topLeft w mds term explorer scope)
        Enter -> (Moore.extract term |> .scope) `Maybe.andThen` \scope -> Just (toOpen e.topLeft w mds term explorer scope)
        -- these dont
        Mouse xy -> stepX e.topLeft w md (EditableTerm.Mouse xy) term `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Movement d2 -> stepX e.topLeft w md (EditableTerm.Movement d2) term `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Preapply -> stepX e.topLeft w md (EditableTerm.Modify (Term.App Term.Blank)) term `Maybe.andThen` \term ->
          Moore (out term explorer) (explorerclosed mds term explorer)
          |> Moore.feed (moveDown e) -- moves cursor to point to newly created blank
          |> Just
        ViewToggle -> stepX e.topLeft w md EditableTerm.ToggleRaw term `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Replace r -> stepX e.topLeft w md (EditableTerm.Replace r) term `Maybe.andThen` \term ->
          Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        MetadataResults refs -> case Moore.feed refs mds of
          mds -> Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        Act action -> (Moore.extract term |> .scope) `Maybe.andThen` \scope ->
          let
            focus = TermExplorer.localFocus scope.focus (Moore.extract term |> .term)
            r = out term explorer
            o = { r | request <- Just (EditRequest focus action) }
          in
            Just <| Moore.spike o { o | request <- Nothing } (explorerclosed mds term explorer)
        _ -> Nothing

    ex0 = TermExplorer.model sink
    tryAccept pt w mds term explorer = explorer `Maybe.andThen`
      \explorer -> case Moore.extract explorer |> .selection of
        Nothing -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        Just (loc,replacement) ->
          let term' = Moore.feed { event = Just (EditableTerm.Modify (always replacement))
                                 , explorerOpen = False
                                 , availableWidth = w
                                 , metadata = Moore.extract mds
                                 , topLeft = pt } term
          in Just <| Moore (out term' ex0) (explorerclosed mds term' ex0)

    exploreropen mds term explorer e = case (e.event,e.availableWidth) of
      (Nothing,w) -> Maybe.map
        (\term -> let o = (out term explorer) in Moore { o | request <- Nothing } (exploreropen mds term explorer))
        (Moore.step { event = Nothing, explorerOpen = True
                    , availableWidth = w, metadata = Moore.extract mds, topLeft = e.topLeft }
                    term)
      (Just event,w) -> case event of
        -- these can trigger a state change
        Click xy ->
          let (x,y) = explorerXY term xy
              eview = Moore.extract explorer |> .view
          in if x < 0 || y < 0 || x > Element.widthOf eview || y > Element.heightOf eview
             then Just <| Moore (out term ex0) (explorerclosed mds term ex0)
             else tryAccept e.topLeft w mds term (Moore.step (TermExplorer.Click (x,y)) explorer)
        Enter -> case Moore.feed TermExplorer.Enter explorer of
          explorer -> case Moore.extract explorer |> .selection of
            -- treat Enter as a cancel event if no valid selection
            Nothing -> Just <| Moore (out term ex0) (explorerclosed mds term ex0)
            Just _ -> tryAccept e.topLeft w mds term (Just explorer)
        FieldContent content ->
          tryAccept e.topLeft w mds term (Moore.step (TermExplorer.FieldContent content) explorer)
        -- these cannot
        Mouse xy ->
          let xy' = explorerXY term xy
          in Moore.step (TermExplorer.Navigate (Selection1D.Mouse xy')) explorer `Maybe.andThen`
             \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        Movement d2 ->
          let d1 = Movement.negateD1 << Movement.xy_y <| d2
          in Moore.step (TermExplorer.Navigate (Selection1D.Move d1)) explorer `Maybe.andThen`
             \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        MetadataResults refs -> case Moore.feed refs mds of
          mds -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        SearchResults results -> case Moore.feed results.references mds of
          mds -> Moore.step (TermExplorer.SearchResults results) explorer `Maybe.andThen`
                 \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        LocalInfoResults results ->
          Moore.step (TermExplorer.LocalInfoResults results) explorer `Maybe.andThen`
          \explorer -> Just <| Moore (out term explorer) (exploreropen mds term explorer)
        Replace r ->
          let msg = { availableWidth = w, event = Just (EditableTerm.Replace r)
                    , explorerOpen = True, metadata = Moore.extract mds, topLeft = e.topLeft }
          in Moore.step msg term `Maybe.andThen` \term ->
             Just <| Moore (out term explorer) (explorerclosed mds term explorer)
        _ -> Nothing
  in
    let
      terms0 = EditableTerm.model term0
      explorer0 = TermExplorer.model sink
    in
      Moore (out terms0 explorer0) (explorerclosed Metadata.cache terms0 explorer0)

ignoreUpDown : Signal Field.Content -> Signal Field.Content
ignoreUpDown s =
  let k = Signal.sampleOn (Signal.keepIf (\a -> a.y /= 0) {x = 0, y = 0} Keyboard.arrows)
                          (Signals.delay Field.noContent s)
  in Signal.merge k s

main =
  let
    host = "http://localhost:8080"
    searchbox = Signal.channel Field.noContent
    merge = Signal.merge

    reqChan : Signal.Channel (Maybe Request)
    reqChan = Signal.channel Nothing

  --| Declare Term
    responses : Signal (Maybe Event)
    responses =
      let
        reqs = Signal.subscribe reqChan

        evaluations =
          let
            match r = case r of
              Just (Evaluations es) -> Just es
              _ -> Nothing
          in JR.send (Node.evaluateTerms host `JR.to` EvaluationResults) [] (Signal.map match reqs) |> Signal.map raise

        edits =
          let
            z = ([], [], Action.Noop, Term.Blank) -- bogus initial edit
            match r = case r of
              Just (EditRequest focus action) ->
                Just (focus.pathToClosedSubterm, focus.pathFromClosedSubterm, action, focus.closedSubterm)
              _ -> Nothing
          in JR.send (Node.editTerm host `JR.to` Replace) z (Signal.map match reqs) |> Signal.map raise

        localInfos =
          let
            z = (Term.Lit (Term.Text "@#$!@#"), [])
            match r = case r of
              Just (ExplorerRequest (TermExplorer.LocalInfo focus)) ->
                Just (focus.closedSubterm, focus.pathFromClosedSubterm)
              _ -> Nothing
          in JR.send (Node.localInfo host `JR.to` LocalInfoResults) z (Signal.map match reqs)
             |> Signal.map raise

        metadatas =
          let
            match r = case r of
              Just (Metadatas refs) -> Just refs
              _ -> Nothing
          in JR.send (Node.metadatas host `JR.to` MetadataResults) [] (Signal.map match reqs) |> Signal.map raise

        searches =
          let
            z = (Term.Blank, [], 1, "@#$@#", Nothing) -- bogus initial search
            match r = case r of
              Just (ExplorerRequest (TermExplorer.Search args)) -> Just args
              _ -> Nothing
          in JR.send (Node.search host `JR.to` SearchResults) z (Signal.map match reqs)
             |> Signal.map raise

        raise : Result (JR.Status String) Event -> Maybe Event
        raise r = case r of
          Result.Err _ -> Nothing -- todo, pass this along somehow
          Result.Ok e -> Just e

      in
        evaluations `merge` edits `merge` localInfos `merge` metadatas `merge` searches

    actions : Signal (Maybe Event)
    actions =
      (Signal.map Just <|
        Signals.keyEvent (Act Action.Step) 83 `merge` -- [s]tep
        Signals.keyEvent (Act Action.WHNF) 69 `merge` -- [e]valuate
        Signals.keyEvent (Act Action.Eta) 82 `merge`  -- eta [r]educe
        Signals.keyEvent Delete 68 `merge`            -- [d]elete
        Signals.keyEvent Preapply 65 `merge`          -- pre-[a]pply
        Signals.keyEvent ViewToggle 86 `merge`        -- [v]iew toggle
        Signals.keyEvent Enter 13 `merge`             -- <enter>
        Signal.map Movement (Movement.repeatD2 <| Movement.d2' Keyboard.arrows) `merge`
        Signal.map Click (Signal.sampleOn Mouse.clicks Mouse.position) `merge`
        Signal.map Mouse Mouse.position `merge`
        Signal.map FieldContent (ignoreUpDown (Signal.subscribe searchbox))) `merge`
        responses

    term0 = Term.Lit (Term.Number 42)

    outs : Signal Out
    outs = Signals.tagEvent actions Window.width
        |> Signal.map (\(e,w) -> { event = Maybe.withDefault Nothing e, availableWidth = w, topLeft = (16,16) })
        |> Moore.transform (model (Signal.send searchbox) term0)

    requests = Signals.justs (Signal.map .request outs) |> Signal.map (Signal.send reqChan)
  in
    Signal.map .view (Signals.during outs (Execute.schedule requests))
