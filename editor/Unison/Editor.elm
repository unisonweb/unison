module Unison.Editor (Model) where

import Dict
import Dict (Dict)
import Elmz.Layout (Containment,Layout,Pt,Region)
import Elmz.Layout as Layout
import Elmz.Selection1D as Selection1D
import Elmz.Trie (Trie)
import Elmz.Trie as Trie
import Graphics.Input.Field as Field
import Graphics.Element as Element
import Graphics.Element (Element)
import Result
import Unison.Explorer as Explorer
import Unison.Hash (Hash)
import Unison.Metadata as Metadata
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Scope (Scope)
import Unison.Scope as Scope
import Unison.Styles as Styles
import Unison.Term (Term)
import Unison.Term as Term
import Unison.View as View
import Signal

type alias Model =
  { overall : Term
  , scope : Maybe Scope
  , explorer : Explorer.Model
  , explorerValues : List Term
  , explorerSelection : Selection1D.Model
  , dependents : Trie Path.E (List Path)
  , overrides : Trie Path.E (Layout View.L)
  , hashes : Trie Path.E Hash }

type alias Action = Model -> Model



viewTerm : Int -> Term -> Layout View.L
viewTerm availableWidth term =
  View.layout term { rootMetadata = Metadata.anonymousTerm
                   , availableWidth = availableWidth
                   , metadata h = Metadata.anonymousTerm
                   , overrides x = Nothing }

type alias Sink a = a -> Signal.Message

type alias Context =
  { availableWidth : Int
  , searchbox : Sink Field.Content
  , explorerActive : Sink Bool }

view : Context -> Model -> (Layout View.L, Layout (Result Containment Int))
view ctx model =
  let termLayout = viewTerm ctx.availableWidth model.overall

      highlighted : Maybe Region
      highlighted = case model.scope of
        Nothing -> Nothing
        Just scope -> Layout.region Path.startsWith .path termLayout scope.focus
                   |> Layout.selectableLub .selectable

      highlightLayer : Element
      highlightLayer = case highlighted of
        Nothing -> Element.empty
        Just region -> Styles.selection region

      highlightedTermLayout : Layout View.L
      highlightedTermLayout =
        Layout.transform (\e -> Element.layers [e, highlightLayer]) termLayout

      explorerTopLeft : Pt
      explorerTopLeft = case highlighted of
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

  in (highlightedTermLayout, highlightedExplorerLayout)
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
