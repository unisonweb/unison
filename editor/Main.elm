module Main where

import Array
import Set
import Unison.Path (Path)
import Unison.Hash (Hash)
import Unison.Styles as S
import Unison.Layout as UL
import Unison.Term as L
import Unison.Metadata as MD
import Unison.Action
import Unison.Node as N

import Graphics.Input(..)
import Graphics.Input.Field(..)
import Window

entry : Input (Maybe (Hash,Path))
entry = input Nothing

nums : L.Term
nums = let f x = L.Lit (L.Number (toFloat x))
       in L.Lit (L.Vector (Array.fromList (map f [0..20])))

expr = L.App (L.App (L.Ref "foo") nums) (L.Ref "baz")

scene : Int -> (Maybe (Hash,Path)) -> Element
scene w p =
  flow down
    [ S.codeText ("path: " ++ show p)
    , L.render expr
      { handle = entry.handle
      , key = "bar"
      , highlighted = []
      , availableWidth = w
      , metadata h = MD.anonymousTerm } ]

main : Signal Element
main = scene <~ Window.width ~ entry.signal
