module Unison.Editor (Model) where

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

model0 : Model
model0 =
  { term = Term.Blank
  , scope = Nothing
  , dependents = Trie.empty
  , overrides = Trie.empty
  , hashes = Trie.empty
  , explorer = Nothing
  , explorerValues = []
  , explorerSelection = 0
  , layouts = { panel = Layout.empty { path = [], selectable = False }
              , panelHighlight = Nothing
              , explorer = Layout.empty (Result.Err Outside) } }

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

click : (Int,Int) -> Action
click (x,y) model = case model.explorer of
  Nothing -> case Layout.leafAtPoint model.layouts.panel (Pt x y) of
    Nothing -> norequest model -- noop, user didn't click on anything!
    Just node -> request { model | explorer <- Explorer.zero
                                 , explorerValues <- []
                                 , explorerSelection <- 0 }
  Just _ -> case Layout.leafAtPoint model.layouts.explorer (Pt x y) of
    Nothing -> norequest { model | explorer <- Nothing } -- treat this as a close event
    Just (Result.Ok i) -> close { model | explorerSelection <- i } -- close w/ selection
    Just (Result.Err Inside) -> norequest <| model -- noop click inside explorer
    Just (Result.Err Outside) -> norequest <| { model | explorer <- Nothing } -- treat this as a close event

moveMouse : (Int,Int) -> Action
moveMouse xy model = case model.explorer of
  Nothing -> norequest <| { model | scope <- Scope.reset xy model.layouts.panel model.scope }
  Just _ -> let e = Selection1D.reset xy model.layouts.explorer model.explorerSelection
            in norequest <| { model | explorerSelection <- e }

updateExplorerValues : List Term -> Model -> Model
updateExplorerValues cur model =
  { model | explorerValues <- cur
          , explorerSelection <- Selection1D.selection model.explorerValues
                                                       cur
                                                       model.explorerSelection }
movement : Movement.D2 -> Action
movement d2 model = norequest <| case model.explorer of
  Nothing -> { model | scope <- Scope.movement model.term d2 model.scope }
  Just _ -> let d1 = Movement.negateD1 (Movement.xy_y d2)
                limit = List.length model.explorerValues
            in { model | explorerSelection <- Selection1D.movement d1 limit model.explorerSelection }

close : Action
close model =
  refreshPanel Nothing (Layout.widthOf model.layouts.panel) <<
  Maybe.withDefault { model | explorer <- Nothing } <|
  Selection1D.index model.explorerSelection model.explorerValues `Maybe.andThen` \term ->
  model.scope `Maybe.andThen` \scope ->
  Term.set scope.focus model.term term `Maybe.andThen` \t2 ->
  Just { model | term <- t2, explorer <- Nothing }

-- todo: invalidate dependents and overrides if under the edit path

{-| Updates `layouts.panel` and `layouts.panelHighlight` based on a change. -}
refreshPanel : Maybe (Sink Field.Content) -> Int -> Action
refreshPanel searchbox availableWidth model =
  let layout = View.layout model.term <|
             { rootMetadata = Metadata.anonymousTerm
             , availableWidth = availableWidth
             , metadata h = Metadata.anonymousTerm
             , overrides x = Nothing }
      layouts = model.layouts
      explorerRefresh model = case searchbox of
        Nothing -> norequest model
        Just searchbox -> refreshExplorer searchbox availableWidth model
  in explorerRefresh <| case model.scope of
       Nothing -> { model | layouts <- { layouts | panel <- layout }}
       Just scope ->
         let (panel, highlight) = Scope.view { layout = layout, term = model.term } scope
         in { model | layouts <- { layouts | panel <- panel, panelHighlight <- highlight }}

refreshExplorer : Sink Field.Content -> Int -> Action
refreshExplorer searchbox availableWidth model =
  let explorerTopLeft : Pt
      explorerTopLeft = case model.layouts.panelHighlight of
        Nothing -> Pt 0 0
        Just region -> { x = region.topLeft.x, y = region.topLeft.y + region.height }

      -- todo: use available width
      explorerLayout : Layout (Result Containment Int)
      explorerLayout = Explorer.view explorerTopLeft searchbox model.explorer

      explorerHighlight : Element
      explorerHighlight =
        Selection1D.view Styles.explorerSelection explorerLayout model.explorerSelection

      highlightedExplorerLayout : Layout (Result Containment Int)
      highlightedExplorerLayout =
        Layout.transform (\e -> Element.layers [e, explorerHighlight]) explorerLayout
  in let layouts = model.layouts
     in norequest { model | layouts <- { layouts | explorer <- highlightedExplorerLayout } }

resize : Maybe (Sink Field.Content) -> Int -> Action
resize sink availableWidth =
  refreshPanel sink availableWidth

enter : Action
enter model = case model.explorer of
  Nothing -> request { model | explorer <- Explorer.zero, explorerValues <- [], explorerSelection <- 0 }
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
      steadyWidth = Signals.steady (100 * Time.millisecond) ctx.width
      movementsRepeated = Movement.repeatD2 ctx.movements
      combine f g model =
        let (req, m2) = f model
            (req', m3) = g m2
        in (Maybe.oneOf [req', req], m3)
      merge = Signals.mergeWith combine
      clickPositions = Signal.sampleOn ctx.clicks ctx.mouse
  in Signal.map (always enter) ctx.enters `merge`
     Signal.map click clickPositions `merge`
     Signal.map movement movementsRepeated `merge`
     Signal.map moveMouse ctx.mouse `merge`
     Signal.map (resize (Just (Signal.send ctx.channel))) steadyWidth

models : Inputs -> (Signal Request -> Signal (Model -> Model)) -> Model -> Signal Model
models ctx search model0 =
  Signals.asyncUpdate
    search
    (actions ctx)
    { term = model0.term, path = [], query = Nothing }
    model0

view : Model -> Element
view model =
  Element.layers [ Layout.element model.layouts.panel
                 , Layout.element model.layouts.explorer ]

todo : a
todo = Debug.crash "Editor.todo"

ignoreUpDown : Signal Field.Content -> Signal Field.Content
ignoreUpDown s =
  let f arrows c prevC = if arrows.y /= 0 && c.string == prevC.string then prevC else c
  in Signal.map3 f (Signal.keepIf (\a -> a.y /= 0) {x = 0, y = 0} Keyboard.arrows)
                   s
                   (Signals.delay Field.noContent s)

search : Signal Request -> Signal (Model -> Model)
search reqs =
  let possible = ["Alice", "Alicia", "Bob", "Burt", "Carol", "Carolina", "Dave", "Don", "Eve"]
      matches content = case content of
        Nothing -> possible
        Just content -> List.filter (String.contains content.string) possible
      go req = let possible = matches req.query
               in updateExplorerValues (List.map Terms.str possible)
  in Signal.map go reqs

main =
  let padTop = 10
      padLeft = 10
      shift (x,y) = (x+padTop,y+padLeft)
      shiftE e = Element.spacer 1 padTop `Element.above`
                   (Element.spacer padLeft 1 `Element.beside` e)
      inputs = { clicks = Mouse.clicks
               , mouse = Signal.map shift Mouse.position
               , enters = Signal.map (always ()) (Signals.ups (Keyboard.enter))
               , movements = Movement.d2' Keyboard.arrows
               , channel = Signal.channel Field.noContent
               , width = Window.width }
      ms = models inputs search { model0 | term <- Terms.expr0 }
  in Signal.map (shiftE << view) ms

