module Main where

import Array
import Elmz.Distance as Distance
import Elmz.Layout as L
import Elmz.Maybe
import Elmz.Moore (Moore)
import Elmz.Moore as Moore
import Elmz.Movement as Movement
import Elmz.Selection1D as Selection1D
import Elmz.Signal as Signals
import Elmz.Trie
import Graphics.Element (Element)
import Graphics.Element as Element
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Keyboard
import List
import Maybe
import Mouse
import Set
import Signal
import Signal ((<~), (~), Signal)
import Text
import Time
import Unison.Action
import Unison.Explorer as Explorer
import Unison.Hash (Hash)
import Unison.Metadata as MD
import Unison.Node as N
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Reference as R
import Unison.Scope (Scope)
import Unison.Scope as Scope
import Unison.Styles as S
import Unison.Term as E
import Unison.View as V
import Window

type alias Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

type alias Model =
  { term : E.Term
  , scope : Maybe Scope }
  -- , explorer : Maybe Explorer.Model }
  -- current scope and current selection?

main : Signal Element
main =
  let search = Signal.channel Field.noContent
      active = Signal.channel False

      terms : Signal E.Term
      terms = Signal.constant expr

      rendered : Signal (L.Layout { path : Path, selectable : Bool })
      rendered = layout <~ Signals.steady (100 * Time.millisecond) Window.width ~ terms

      explorerOpen : Signal Bool
      explorerOpen = -- due to mouse click, what about keypress? make this a separate event
        let go ((x,y), active, layout) active =
              if | active && (x > L.widthOf layout || y > L.heightOf layout) -> False
                 | active -> True -- click within keeps explorer open
                 | not active && (x <= L.widthOf layout && y <= L.heightOf layout) -> True
                 | otherwise -> active
            clickAt = Signal.sampleOn Mouse.clicks Mouse.position
        in Signal.foldp go False (Signal.map3 (,,) clickAt (Signal.subscribe active) rendered)

      todo : a
      todo = List.head []

      -- used for moving the selection in the panel being edited
      mouseNav = Signal.dropWhen (Signal.subscribe active) (0,0) Mouse.position

      leaf : Signal (Maybe Path)
      leaf = Signal.map (Maybe.map .path) (leafUnderPtr mouseNav rendered)

      scope : Signal (Maybe Scope)
      scope = resolvedPath terms leaf

      -- explorerModel : Signal (Explorer.Model E.Term)
      -- explorerModel = todo

      highlight : Signal (Maybe L.Region)
      highlight =
        let region layout scope = case scope of
          Nothing -> Nothing
          Just scope -> L.region Path.startsWith .path layout scope.focus
                     |> L.selectableLub .selectable
        in region <~ rendered ~ scope

      highlightLayer : Signal Element
      highlightLayer =
        let f layout region = case region of
          Nothing -> Element.empty
          Just region -> S.selection layout region
        in Signal.map2 f rendered highlight

      scene : L.Layout x -> Element -> Maybe Scope -> Element
      scene l selection scope =
        Element.flow Element.down [
          Element.layers [L.element l, selection],
          Element.spacer 1 100,
          S.codeText ("Path: " ++ toString (Maybe.map .focus scope))
        ]

  in scene <~ rendered ~ highlightLayer ~ scope

ap = E.App
builtin s = E.Ref (R.Builtin s)
derived s = E.Ref (R.Derived s)
int n = E.Lit (E.Number (toFloat n))
vec es = E.Vector (Array.fromList es)

nums : E.Term
nums = vec (List.map int [0..20])

rgbTerm : Int -> Int -> Int -> E.Term
rgbTerm r g b =
  builtin "Color.rgba" `ap` int r `ap` int g `ap` int b `ap` int 1

-- expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (E.Builtin "View.cell" `ap` E.Builtin "View.swatch" `ap` rgbTerm 230 126 34))
expr0 = derived "foo" `ap` nums `ap` (derived "baz" `ap` rgbTerm 230 126 34)
expr = derived "foo" `ap` nums `ap` (derived "baz" `ap` (builtin "View.cell" `ap` builtin "View.swatch" `ap` rgbTerm 230 126 34))
-- this bombs
-- expr = E.Ref "uno" `ap` E.Ref "dos" `ap` E.Ref "tres" `ap` E.Ref "quatro" `ap` E.Ref "cinco" `ap` E.Ref "seis" `ap` E.Ref "siete" `ap` E.Ref "ocho"
-- expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (rgbTerm 230 126 34))

cell f x = builtin "View.cell" `ap` f `ap` x
panel v e = builtin "View.panel" `ap` v `ap` e
function1 f = builtin "View.function1" `ap` f
source e = builtin "View.source" `ap` e
verticalPanel es = panel (builtin "View.vertical") (vec es)
string s = E.Lit (E.Str s)
text s = builtin "View.text" `ap` s
centered s = builtin "View.textbox" `ap` builtin "Text.center" `ap` full `ap` s
h1 s = cell (text E.Blank) (E.Lit (E.Str s))
body s = cell (text E.Blank) (E.Lit (E.Str s))
full = E.Lit (E.Distance (Distance.Fraction 1.0))

--expr = cell (function1 (E.Lam 0 (verticalPanel [h1 "The Answer to The Ultimate Question of Life, the Universe, and Everything...", body "", E.Var 0])))
--            (E.Ref "answer") `ap`
--            (E.Lit (E.Number 42.0))

resolvedPath : Signal E.Term -> Signal (Maybe Path) -> Signal (Maybe Scope)
resolvedPath e pathUnderPtr =
  let defaultScope = Signal.map (Maybe.map Scope.scope) pathUnderPtr
      shifted = Movement.moveD2 Scope.movements
                                Mouse.position
                                (Signals.tuple2 e defaultScope)
                                (Movement.repeatD2 (Movement.d2' Keyboard.arrows))
  in Signals.fromMaybe defaultScope (Maybe.map snd <~ shifted)

terms : Signal E.Term
terms = Signal.constant expr

layout : Int -> E.Term -> L.Layout { path : Path, selectable : Bool }
layout availableWidth term =
  V.layout term { rootMetadata = MD.anonymousTerm
                , availableWidth = availableWidth
                , metadata h = MD.anonymousTerm
                , overrides x = Nothing }

leafUnderPtr : Signal (Int,Int)
            -> Signal (L.Layout { path : Path, selectable : Bool })
            -> Signal (Maybe { path : Path, selectable : Bool })
leafUnderPtr mouse layout =
  let go layout (x,y) =
    let paths = L.atRanked (List.length << .path) layout (L.Region { x = x, y = y } 2 2)
    in case paths of
      (h :: _) :: _ -> Just h
      _ -> Nothing
  in go <~ layout ~ mouse

