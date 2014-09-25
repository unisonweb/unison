module Main where

import Array
import Set
import Graphics.Element as Element
import Unison.Path (Path)
import Unison.Path as Path
import Unison.Hash (Hash)
import Unison.Styles as S
import Unison.Layout as L
import Unison.Term as E
import Unison.Metadata as MD
import Unison.Action
import Unison.Node as N

import Graphics.Input(..)
import Graphics.Input.Field(..)
import Window
import Keyboard
import Mouse
import Text
import Elmz.Signal as Signals
import Elmz.Maybe

type Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

nums : E.Term
nums = let f x = E.Lit (E.Number (toFloat x))
       in E.Lit (E.Vector (Array.fromList (map f [0..20])))

expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (E.Lit (E.Str "hello world!")))

resolvedPath : Signal E.Term -> Signal (Maybe Path) -> Signal (Maybe Path)
resolvedPath e pathUnderPtr =
  let edit {x,y} e = \p' -> p' |>
        (if y == 1 then E.up else identity) |>
        (if y == -1 then E.down e else identity) |>
        (if x == 1 then E.siblingR e else identity) |>
        (if x == -1 then E.siblingL e else identity)
      edits = edit <~ Keyboard.arrows ~ e
      shifted = Signals.foldpWhen'
                  (Signals.unchanged Mouse.position)
                  (\edit p -> Elmz.Maybe.map edit p)
                  pathUnderPtr
                  edits
  in Signals.fromMaybe pathUnderPtr shifted

terms : Signal E.Term
terms = constant expr

layouts : Signal Int
       -> Signal E.Term
       -> Signal (L.Layout { hash : Hash, path : Path, selectable : Bool })
layouts availableWidth terms =
  let go w e = E.layout e { key = "bar", availableWidth = w, metadata h = MD.anonymousTerm }
  in go <~ availableWidth ~ terms

leafUnderPtr : Signal (L.Layout { hash : Hash, path : Path, selectable : Bool })
            -> Signal (Maybe { hash : Hash, path : Path, selectable : Bool })
leafUnderPtr layout =
  let go layout (x,y) =
    let paths = L.atRanked (length << .path) layout (L.Region { x = x, y = y } 2 2)
    in case paths of
      (h :: _) :: _ -> Just h
      _ -> Nothing
  in go <~ layout ~ Mouse.position

-- pathUnderPtr : Signal (L.Layout { hash : Hash, path : Path, selectable : Bool })
-- pathUnderPtr
-- make sole UI state a Term
-- certain terms are "special"

{-
highlightRegion : Signal L.Pt -> Signal (Maybe Path) -> Signal (Maybe L.Region)
highlightRegion topLeft path =
        isPrefix a b = a.hash == "bar" && Path.startsWith a.path b.path

scene : Int -> (Int,Int) -> Int -> Element
scene w (x,y) lvl =
  let layout = E.layout expr { key = "bar", availableWidth = w - 50, metadata h = MD.anonymousTerm }
      paths = L.atRanked (length . .path) layout (L.Region (L.Pt (x-48) (y-98)) 2 2)
      isPrefix a b = a.hash == "bar" && Path.startsWith a.path b.path
      region = case drop (min (length paths - 1) lvl) paths of
        (k :: _) :: _ -> L.selectableLub .selectable (L.region isPrefix layout k)
        _ -> Nothing
      selection = maybe Element.empty (S.selection layout) region
  in flow down
          [ spacer 50 1 `Element.beside` Element.height 100 (S.codeText ("paths: " ++ show paths))
          , spacer 50 1 `Element.beside` (Element.layers [L.element layout, selection])
          ]

main : Signal Element
main = scene <~ Window.width ~ Mouse.position ~ level
-}
