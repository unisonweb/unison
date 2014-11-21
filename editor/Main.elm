module Main where

import Array
import Set
import Graphics.Element as Element
import Unison.Reference as R
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Scope (Scope)
import Unison.Scope as Scope
import Unison.Hash (Hash)
import Unison.Styles as S
import Unison.Term as E
import Unison.View as V
import Unison.Metadata as MD
import Unison.Action
import Unison.Node as N

import Graphics.Input(..)
import Graphics.Input.Field(..)
import Maybe
import Window
import Keyboard
import Mouse
import Text
import Elmz.Layout as L
import Elmz.Signal as Signals
import Elmz.Maybe
import Elmz.Distance as Distance

type Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

ap = E.App
builtin s = E.Ref (R.Builtin s)
derived s = E.Ref (R.Derived s)
int n = E.Lit (E.Number (toFloat n))
vec es = E.Vector (Array.fromList es)

nums : E.Term
nums = vec (map int [0..20])

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
  let nonzero {x,y} = x /= 0 || y /= 0
      edit {x,y} e =
        (if y == 1 then Scope.up else identity) >>
        (if y == -1 then Scope.down e else identity) >>
        (if x == 1 then Scope.right e else identity) >>
        (if x == -1 then Scope.left e else identity)
      edits = edit <~ Signals.repeatAfterIf (300*millisecond) 20 nonzero Keyboard.arrows ~ e
      defaultScope = lift (Maybe.map Scope.scope) pathUnderPtr
      shifted = Signals.foldpBetween'
                  Mouse.position
                  (\edit p -> Maybe.map edit p)
                  defaultScope
                  edits
  in Signals.fromMaybe defaultScope shifted

terms : Signal E.Term
terms = constant expr

layout : Int -> E.Term -> L.Layout { path : Path, selectable : Bool }
layout availableWidth term =
  V.layout term { rootMetadata = MD.anonymousTerm
                , availableWidth = availableWidth
                , metadata h = MD.anonymousTerm
                , overrides x = Nothing }

leafUnderPtr : Signal (L.Layout { path : Path, selectable : Bool })
            -> Signal (Maybe { path : Path, selectable : Bool })
leafUnderPtr layout =
  let go layout (x,y) =
    let paths = L.atRanked (length << .path) layout (L.Region { x = x, y = y } 2 2)
    in case paths of
      (h :: _) :: _ -> Just h
      _ -> Nothing
  in go <~ layout ~ Mouse.position

main : Signal Element
main =
  let terms : Signal E.Term
      terms = constant expr

      rendered : Signal (L.Layout { path : Path, selectable : Bool })
      rendered = layout <~ Signals.steady (100 * millisecond) Window.width ~ terms

      leaf : Signal (Maybe Path)
      leaf = lift (Maybe.map .path) (leafUnderPtr rendered)

      scope : Signal (Maybe Scope)
      scope = resolvedPath terms leaf

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
        in lift2 f rendered highlight

      scene : L.Layout x -> Element -> Maybe Scope -> Element
      scene l selection scope =
        Element.flow down [
          Element.layers [L.element l, selection],
          Element.spacer 1 100,
          S.codeText ("Path: " ++ show (Maybe.map .focus scope))
        ]

  in scene <~ rendered ~ highlightLayer ~ scope
