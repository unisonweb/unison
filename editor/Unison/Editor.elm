module Unison.Editor (Model) where

import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input.Field as Field
import List
import Elmz.Selection1D as Selection1D
import Elmz.Layout as Layout
import Elmz.Layout (Containment, Layout, Pt)
import Unison.Explorer as Explorer
import Unison.Panel as Panel
import Unison.Term (Term)
import Unison.Term as Term
import Unison.Styles as Styles
import Unison.View as View
import Signal

type alias Model =
  { panel : Panel.Model
  , explorer : Explorer.Model
  , explorerValues : List Term
  , explorerSelection : Selection1D.Model }

type alias Sink a = a -> Signal.Message

type alias Context =
  { availableWidth : Int
  , searchbox : Sink Field.Content
  , explorerActive : Sink Bool }

-- view : Context ->
-- clicks : Signal () -> Signal (Int,Int) -> Signal Action
-- clicks click pos =
--   let f click xy pos

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
