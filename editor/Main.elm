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
import Mouse
import Text

nums : E.Term
nums = let f x = E.Lit (E.Number (toFloat x))
       in E.Lit (E.Vector (Array.fromList (map f [0..20])))

expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (E.Lit (E.Str "hello world!")))

scene : Int -> (Int,Int) -> Element
scene w (x,y) =
  let layout = E.layout expr { key = "bar", availableWidth = w - 50, metadata h = MD.anonymousTerm }
      dummy = S.codeText "w00t"
      paths = L.atRanked (Array.length . .path) layout (L.Pt (x-50) (y-100))
      isPrefix a b = a.hash == "bar" && Path.startsWith a.path b.path
      region = case paths of
        [] -> Nothing
        _ -> L.selectableLub .selectable (L.region isPrefix layout (last paths))
      selection = maybe Element.empty (S.selection layout) region
  in flow down
          [ spacer 50 1 `Element.beside` Element.height 100 (S.codeText ("paths: " ++ show paths))
          , spacer 50 1 `Element.beside` (Element.layers [L.element layout, selection])
          ]

main : Signal Element
main = scene <~ Window.width ~ Mouse.position
