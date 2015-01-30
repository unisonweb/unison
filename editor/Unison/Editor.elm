module Unison.Editor (Model) where

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
import Result
import Signal
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
import Unison.View as View

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

type alias Action = Model -> Model

type alias Sink a = a -> Signal.Message

click : (Int,Int) -> Layout View.L -> Layout (Result Containment Int) -> Action
click (x,y) layout explorer model = case model.explorer of
  Nothing -> case Layout.leafAtPoint layout (Pt x y) of
    Nothing -> model -- noop, user didn't click on anything!
    Just node -> { model | explorer <- Explorer.zero, explorerValues <- [], explorerSelection <- 0 }
  Just _ -> case Layout.leafAtPoint explorer (Pt x y) of
    Nothing -> { model | explorer <- Nothing } -- treat this as a close event
    Just (Result.Ok i) -> close { model | explorerSelection <- i } -- close w/ selection
    Just (Result.Err Inside) -> model -- noop click inside explorer
    Just (Result.Err Outside) -> { model | explorer <- Nothing } -- treat this as a close event

moveMouse : (Int,Int) -> Action
moveMouse xy model = case model.explorer of
  Nothing -> { model | scope <- Scope.reset xy model.layouts.panel model.scope }
  Just _ -> let e = Selection1D.reset xy model.layouts.explorer model.explorerSelection
            in { model | explorerSelection <- e }

updateExplorerValues : List Term -> Action
updateExplorerValues cur model =
  { model | explorerValues <- cur
          , explorerSelection <- Selection1D.selection model.explorerValues
                                                       cur
                                                       model.explorerSelection }

movement : Movement.D2 -> Action
movement d2 model = case model.explorer of
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
        Nothing -> model
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
     in { model | layouts <- { layouts | explorer <- highlightedExplorerLayout } }

resize : Maybe (Sink Field.Content) -> Int -> Action
resize sink availableWidth =
  refreshPanel sink availableWidth

enter : Action
enter model = case model.explorer of
  Nothing -> { model | explorer <- Explorer.zero, explorerValues <- [], explorerSelection <- 0 }
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
      merge = Signals.mergeWith (>>)
      clickPositions = Signal.sampleOn ctx.clicks ctx.mouse
  in Signal.map (always enter) ctx.enters `merge`
     Signal.map movement movementsRepeated `merge`
     Signal.map moveMouse ctx.mouse `merge`
     Signal.map (resize (Just (Signal.send ctx.channel))) steadyWidth

-- type alias Action = Model -> (Maybe Search, Model)
-- can then get a models : Signal (Model, Maybe Search)
-- can do search : Signal Search -> Signal (Model -> Model)
-- and then can just map2 the result of search and models to get final output models!!!

models : Inputs -> Model -> Signal Model
models ctx model0 = Signal.foldp (<|) model0 (actions ctx)

-- derived actions handled elsewhere?
-- can listen for explorer becoming active - this can trigger http request to fetch

view : Model -> Element
view model =
  Element.layers [ Layout.element model.layouts.panel
                 , Layout.element model.layouts.explorer ]

todo : a
todo = todo

ignoreUpDown : Signal Field.Content -> Signal Field.Content
ignoreUpDown s =
  let f arrows c prevC = if arrows.y /= 0 && c.string == prevC.string then prevC else c
  in Signal.map3 f (Signal.keepIf (\a -> a.y /= 0) {x = 0, y = 0} Keyboard.arrows)
                   s
                   (Signals.delay Field.noContent s)
