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

nums : E.Term
nums = let f x = E.Lit (E.Number (toFloat x))
       in E.Lit (E.Vector (Array.fromList (map f [0..20])))

expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (E.Lit (E.Str "hello world!")))

level : Signal Int
level =
  let go {x,y} i = if | y == 1 -> i + 1
                      | y == -1 -> i - 1 `max` 0
                      | otherwise -> i
  in Keyboard.arrows |> foldp go 0

-- integrate left and right arrows so long as mouse does not move
-- call increment that many
-- moving down after moving over should move to the next leaf

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
