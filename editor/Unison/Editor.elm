module Unison.Editor (Model) where

import Elmz.Layout (Containment(Inside,Outside), Layout, Pt)
import Elmz.Layout as Layout
import Elmz.Movement as Movement
import Elmz.Selection1D as Selection1D
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import List
import Result
import Signal
import Unison.Explorer as Explorer
import Unison.Panel as Panel
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.View as View

type alias Model =
  { panel : Panel.Model
  , explorer : Explorer.Model
  , explorerValues : List Term
  , explorerSelection : Selection1D.Model }

type alias Action = Model -> Model

type alias Sink a = a -> Signal.Message

type alias Context =
  { availableWidth : Int
  , searchbox : Sink Field.Content
  , explorerActive : Sink Bool }

click : (Int,Int) -> Layout View.L -> Layout (Result Containment Int) -> Action
click (x,y) layout explorer model = case model.explorer of
  Nothing -> case Layout.leafAtPoint layout (Pt x y) of
    Nothing -> model -- noop, user didn't click on anything!
    Just node -> { model | explorer <- Explorer.zero, explorerValues <- [], explorerSelection <- 0 }
  Just _ -> case Layout.leafAtPoint explorer (Pt x y) of
    Nothing -> { model | explorer <- Nothing } -- treat this as a close event
    Just (Result.Ok i) -> { model | explorerSelection <- i, explorer <- Nothing } -- close w/ selection
    Just (Result.Err Inside) -> model -- noop click inside explorer
    Just (Result.Err Outside) -> { model | explorer <- Nothing } -- treat this as a close event

moveMouse : (Int,Int) -> Layout View.L -> Layout (Result Containment Int) -> Action
moveMouse xy layout explorer model = case model.explorer of
  Nothing -> { model | panel <- Panel.reset xy layout model.panel }
  Just _ -> { model | explorerSelection <- Selection1D.reset xy explorer model.explorerSelection }

updateExplorerValues : List Term -> List Term -> Action
updateExplorerValues prev cur model =
  { model | explorerValues <- cur
          , explorerSelection <- Selection1D.selection prev cur model.explorerSelection }

movement : Movement.D2 -> Action
movement d2 model = case model.explorer of
  Nothing -> { model | panel <- Panel.movement d2 model.panel }
  Just _ -> let d1 = Movement.negateD1 (Movement.xy_y d2)
                limit = List.length model.explorerValues
            in { model | explorerSelection <- Selection1D.movement d1 limit model.explorerSelection }

enter : Action
enter model = case model.explorer of
  Nothing -> { model | explorer <- Explorer.zero, explorerValues <- [], explorerSelection <- 0 }
  Just _ -> { model | explorer <- Nothing }

-- derived actions handled elsewhere?
-- can listen for explorer becoming active, and can listen for explorer becoming inactive

view : Context -> Model -> (Layout View.L, Layout (Result Containment Int))
view ctx model =
  let (panelLayout, selected) = Panel.view ctx.availableWidth model.panel

      explorerTopLeft : Pt
      explorerTopLeft = case selected of
        Nothing -> Pt 0 0
        Just region -> { x = region.topLeft.x, y = region.topLeft.y + region.height }

      explorerLayout : Layout (Result Containment Int)
      explorerLayout =
        Explorer.view explorerTopLeft
                      ctx.searchbox
                      ctx.explorerActive
                      model.explorer

      explorerHighlight : Element
      explorerHighlight =
        Selection1D.view Styles.explorerSelection explorerLayout model.explorerSelection

      highlightedExplorerLayout : Layout (Result Containment Int)
      highlightedExplorerLayout =
        Layout.transform (\e -> Element.layers [e, explorerHighlight]) explorerLayout

  in (panelLayout, highlightedExplorerLayout)
--
-- viewExplorer : Model -> Layout
-- view : Int -> Term -> Layout View.L
-- view availableWidth term =
--   let rendered : Signal (L.Layout { path : Path, selectable : Bool })
--       rendered = layout <~ Signals.steady (100 * Time.millisecond) Window.width ~ term
--  in

todo : a
todo = todo

-- might want to just add Panel, Cell constructors
-- Panel Term

{-| For each subpanel path, what are the paths to other subpanels that it depends on? -}
-- localDependencies : (Path -> Maybe (Hash,Term)) -> Term -> Path -> [Path]
-- localDependencies sourceOf e = todo
