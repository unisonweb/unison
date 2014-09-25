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
import Maybe
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
  let edit {x,y} e =
        (if y == 1 then E.up else identity) >>
        (if y == -1 then E.down e else identity) >>
        (if x == 1 then E.siblingR e else identity) >>
        (if x == -1 then E.siblingL e else identity)
      edits = edit <~ Keyboard.arrows ~ e
      shifted = Signals.foldpBetween'
                  Mouse.position
                  (\edit p -> Maybe.map edit p)
                  pathUnderPtr
                  edits
  in Signals.fromMaybe pathUnderPtr shifted

terms : Signal E.Term
terms = constant expr

layout : Int -> E.Term -> L.Layout { hash : Hash, path : Path, selectable : Bool }
layout availableWidth term =
  E.layout term { key = "bar"
                , availableWidth = availableWidth
                , metadata h = MD.anonymousTerm }

leafUnderPtr : Signal (L.Layout { hash : Hash, path : Path, selectable : Bool })
            -> Signal (Maybe { hash : Hash, path : Path, selectable : Bool })
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

      rendered : Signal (L.Layout { hash : Hash, path : Path, selectable : Bool })
      rendered = layout <~ Window.width ~ terms

      leaf : Signal (Maybe Path)
      leaf = lift (Maybe.map .path) (leafUnderPtr rendered)

      path : Signal (Maybe Path)
      path = resolvedPath terms leaf

      highlight : Signal (Maybe L.Region)
      highlight =
        let region layout path = case path of
          Nothing -> Nothing
          Just path -> L.region Path.startsWith .path layout path
                    |> L.selectableLub .selectable
        in region <~ rendered ~ path

      highlightLayer : Signal Element
      highlightLayer =
        let f layout region = case region of
          Nothing -> Element.empty
          Just region -> S.selection layout region
        in lift2 f rendered highlight

      scene : L.Layout x -> Element -> Element
      scene l selection = Element.layers [L.element l, selection]
  in scene <~ rendered ~ highlightLayer

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
