module Unison.Editor where

import Debug
import Elmz.Layout (Containment(Inside,Outside), Layout, Pt, Region)
import Elmz.Layout as Layout
import Elmz.Movement as Movement
import Elmz.Selection1D as Selection1D
import Elmz.Signal as Signals
import Elmz.Trie (Trie)
import Elmz.Trie as Trie
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Keyboard
import List
import Maybe
import Mouse
import Result
import Signal
import String
import Time
import Unison.Explorer as Explorer
import Unison.Hash (Hash)
import Unison.Metadata as Metadata
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.Terms as Terms
import Unison.View as View
import Window

type alias Model =
  { term : Term
  , scope : Scope.Model
  , availableWidth : Maybe Int
  , dependents : Trie Path.E (List Path)
  , overrides : Trie Path.E (Layout View.L)
  , hashes : Trie Path.E Hash
  , explorer : Explorer.Model
  , explorerValues : List Term
  , explorerSelection : Selection1D.Model
  , layouts : { panel : Layout View.L
              , panelHighlight : Maybe Region
              , explorer : Layout (Result Containment Int) } }

type alias Request = { term : Term, path : Path, query : Maybe Field.Content }

type alias Action = Model -> (Maybe Request, Model)

combine : Action -> Action -> Action
combine f g model =
  let (req, m2) = f model
      (req', m3) = g m2
  in (Maybe.oneOf [req', req], m3)

model0 : Model
model0 =
  { term = Term.Blank
  , scope = Nothing
  , availableWidth = Nothing
  , dependents = Trie.empty
  , overrides = Trie.empty
  , hashes = Trie.empty
  , explorer = Nothing
  , explorerValues = []
  , explorerSelection = 0
  , layouts = { panel = Layout.empty { path = [], selectable = False }
              , panelHighlight = Nothing
              , explorer = explorerLayout0  } }

layout0 : Layout View.L
layout0 = Layout.empty { path = [], selectable = False }

explorerLayout0 : Layout (Result Containment a)
explorerLayout0 = Layout.empty (Result.Err Outside)

request : Model -> (Maybe Request, Model)
request model = case model.scope of
  Nothing -> (Nothing, model)
  Just scope -> (Just
    { term = model.term
    , path = scope.focus
    , query = Maybe.map .input model.explorer }, model)

norequest : Model -> (Maybe Request, Model)
norequest model = (Nothing, model)

type alias Sink a = a -> Signal.Message

click : Sink Field.Content -> (Int,Int) -> Action
click snk (x,y) model = case model.explorer of
  Nothing -> case Layout.leafAtPoint model.layouts.panel (Pt x y) of
    Nothing -> norequest model -- noop, user didn't click on anything!
    Just node ->
      let (req, m2) = request { model | explorer <- Explorer.zero
                                      , explorerValues <- []
                                      , explorerSelection <- 0 }
      in (req, refreshExplorer snk m2)
  Just _ -> case Layout.leafAtPoint model.layouts.explorer (Pt x y) of
    Nothing -> norequest (closeExplorer model) -- treat this as a close event
    Just (Result.Ok i) -> close { model | explorerSelection <- i } -- close w/ selection
    Just (Result.Err Inside) -> norequest <| model -- noop click inside explorer
    Just (Result.Err Outside) -> norequest <| (closeExplorer model) -- treat this as a close event

moveMouse : (Int,Int) -> Action
moveMouse xy model = case model.explorer of
  Nothing ->
    let scope = Scope.reset xy model.layouts.panel model.scope
        layouts = model.layouts
        highlight : Maybe Region
        highlight = Maybe.andThen scope (Scope.view model.layouts.panel)
        layouts' = { layouts | panelHighlight <- highlight }
    in norequest <| { model | scope <- scope, layouts <- layouts' }
  Just _ -> let e = Selection1D.reset xy model.layouts.explorer model.explorerSelection
            in norequest <| { model | explorerSelection <- e }

updateExplorerValues : Sink Field.Content -> List Term -> Model -> Model
updateExplorerValues searchbox cur model =
  refreshExplorer searchbox
    { model | explorerValues <- cur
            , explorerSelection <- Selection1D.selection model.explorerValues
                                                         cur
                                                         model.explorerSelection }

movement : Movement.D2 -> Action
movement d2 model = norequest <| case model.explorer of
  Nothing ->
    let scope = Scope.movement model.term d2 model.scope
        highlight : Maybe Region
        highlight = Maybe.andThen scope (Scope.view model.layouts.panel)
        layouts = model.layouts
    in { model | scope <- scope, layouts <- { layouts | panelHighlight <- highlight }}
  Just _ -> let d1 = Movement.negateD1 (Movement.xy_y d2)
                limit = List.length model.explorerValues
            in { model | explorerSelection <- Selection1D.movement d1 limit model.explorerSelection }

closeExplorer : Model -> Model
closeExplorer model =
  let layouts = model.layouts
  in { model | explorer <- Nothing, layouts <- { layouts | explorer <- explorerLayout0 }}

close : Action
close model =
  refreshPanel Nothing << Maybe.withDefault (closeExplorer model) <|
  Selection1D.index model.explorerSelection model.explorerValues `Maybe.andThen` \term ->
  model.scope `Maybe.andThen` \scope ->
  Term.set scope.focus model.term term `Maybe.andThen` \t2 ->
  (Just << closeExplorer) { model | term <- t2 }

-- todo: invalidate dependents and overrides if under the edit path

{-| Updates `layouts.panel` and `layouts.panelHighlight` based on a change. -}
refreshPanel : Maybe (Sink Field.Content) -> Action
refreshPanel searchbox model =
  let layout = case model.availableWidth of
        Nothing -> layout0
        Just availableWidth -> View.layout model.term <|
          { rootMetadata = Metadata.anonymousTerm
          , availableWidth = availableWidth
          , metadata h = Metadata.anonymousTerm
          , overrides x = Nothing }
      layouts = model.layouts
      explorerRefresh model = case searchbox of
        Nothing -> norequest model
        Just searchbox -> norequest (refreshExplorer searchbox model)
  in explorerRefresh <| case model.scope of
       Nothing -> { model | layouts <- { layouts | panel <- layout }}
       Just scope ->
         let highlight = Scope.view layout scope
         in { model | layouts <- { layouts | panel <- layout, panelHighlight <- highlight }}

refreshExplorer : Sink Field.Content -> Model -> Model
refreshExplorer searchbox model =
  let explorerTopLeft : Pt
      explorerTopLeft = Debug.watch "ex:topLeft" <| case model.layouts.panelHighlight of
        Nothing -> Pt 0 0
        Just region -> { x = region.topLeft.x - 6, y = region.topLeft.y + region.height + 6 }

      availableWidth = (Maybe.withDefault 1000 model.availableWidth - explorerTopLeft.x - 12)
                       `max` 40

      completions : List Element
      completions = -- todo: real metadata
        let show term = Layout.element << View.layout term <|
          { rootMetadata = Metadata.anonymousTerm
          , availableWidth = availableWidth
          , metadata h = Metadata.anonymousTerm
          , overrides x = Nothing }
        in List.map show model.explorerValues

      explorer' : Explorer.Model
      explorer' = model.explorer |> Maybe.map (\e -> { e | completions <- completions })

      explorerLayout : Layout (Result Containment Int)
      explorerLayout = Explorer.view explorerTopLeft searchbox explorer'

      explorerHighlight : Element
      explorerHighlight =
        Selection1D.view Styles.explorerSelection explorerLayout model.explorerSelection

      highlightedExplorerLayout : Layout (Result Containment Int)
      highlightedExplorerLayout =
        Layout.transform (\e -> Element.layers [e, explorerHighlight]) explorerLayout
  in let layouts = model.layouts
     in { model | explorer <- explorer'
                , layouts <- { layouts | explorer <- highlightedExplorerLayout } }

resize : Maybe (Sink Field.Content) -> Int -> Action
resize sink width model =
  refreshPanel sink { model | availableWidth <- Just width }

enter : Sink Field.Content -> Action
enter snk model = case model.explorer of
  Nothing ->
    let (req, m2) = request { model | explorer <- Explorer.zero
                                    , explorerValues <- []
                                    , explorerSelection <- 0 }
    in (req, refreshExplorer snk m2)
  Just _ -> close model

type alias Inputs =
  { clicks : Signal ()
  , mouse : Signal (Int,Int)
  , enters : Signal ()
  , movements : Signal Movement.D2
  , channel : Signal.Channel Field.Content
  , width : Signal Int }

actions : Inputs -> Signal Action
actions ctx =
  let content = ignoreUpDown (Signal.subscribe ctx.channel)
      movementsRepeated = Movement.repeatD2 ctx.movements
      merge = Signals.mergeWith combine
      clickPositions = Signal.sampleOn ctx.clicks ctx.mouse
      snk = Signal.send ctx.channel
      steadyWidth = Signals.sampleOnMerge Signals.start
                                          (Signals.steady (100 * Time.millisecond) ctx.width)
  in Signal.map (resize (Just snk)) steadyWidth `merge`
     Signal.map (always (enter snk)) ctx.enters `merge`
     Signal.map (click snk) clickPositions `merge`
     Signal.map movement movementsRepeated `merge`
     Signal.map moveMouse ctx.mouse

models : Inputs -> (Signal Request -> Signal (Model -> Model)) -> Model -> Signal Model
models ctx search model0 =
  Signals.asyncUpdate
    search
    (actions ctx)
    { term = model0.term, path = [], query = Nothing }
    model0

view : (Int,Int) -> Model -> Element
view origin model =
  let shift e = Element.spacer 1 (snd origin) `Element.above`
                   (Element.spacer (fst origin) 1 `Element.beside` e)
      highlight = case model.layouts.panelHighlight of
        Nothing -> Element.empty
        Just region -> Styles.selection (Debug.watch "sel" <| Layout.offset origin region)
  in Element.layers [ shift <| Layout.element model.layouts.panel
                    , highlight
                    , shift <| Layout.element model.layouts.explorer ]

ignoreUpDown : Signal Field.Content -> Signal Field.Content
ignoreUpDown s =
  let f arrows c prevC = if arrows.y /= 0 && c.string == prevC.string then prevC else c
  in Signal.map3 f (Signal.keepIf (\a -> a.y /= 0) {x = 0, y = 0} Keyboard.arrows)
                   s
                   (Signals.delay Field.noContent s)

search : Sink Field.Content -> Signal Request -> Signal (Model -> Model)
search searchbox reqs =
  let possible = ["Alice", "Alicia", "Bob", "Burt", "Carol", "Carolina", "Dave", "Don", "Eve"]
      matches content = case content of
        Nothing -> possible
        Just content -> List.filter (String.contains content.string) possible
      go req = let possible = matches req.query
               in updateExplorerValues searchbox (List.map Terms.str possible)
  in Time.delay (100 * Time.millisecond) (Signal.map go reqs)

main =
  let origin = (15,15)
      shift (x,y) = (x - fst origin, y - snd origin)
      inputs = { clicks = Mouse.clicks
               , mouse = Signal.map shift Mouse.position
               , enters = Signal.map (always ()) (Signals.ups (Keyboard.enter))
               , movements = Movement.d2' Keyboard.arrows
                          |> Signal.map (Debug.watch "direction")
               , channel = Signal.channel Field.noContent
               , width = Window.width }
      ignoreReqs actions =
        let ignore action model = snd (action model)
        in Signal.map ignore actions
      ms = models inputs (search (Signal.send inputs.channel)) { model0 | term <- Terms.expr0 }
      -- ms = Signal.foldp (<|) { model0 | term <- Terms.expr0 } (ignoreReqs (actions inputs))
      debug model =
        let summary model = (model.scope, model.explorerValues)
        in Debug.watchSummary "scope" summary model
      ms' = Signal.map debug ms
      -- ms = Signal.constant { model0 | term <- Terms.expr0 }
  in Signal.map (view origin) ms'

