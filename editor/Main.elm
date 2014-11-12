module Main where

import Array
import Set
import Graphics.Element as Element
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
import Elmz.Distance

type Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

nums : E.Term
nums = let f x = E.Lit (E.Number (toFloat x))
       in E.Lit (E.Vector (Array.fromList (map f [0..15])))

rgbTerm : Int -> Int -> Int -> E.Term
rgbTerm r g b =
  E.App (E.App (E.App (E.App (E.Lit (E.Builtin "Color.rgba")) (E.Lit (E.Number (toFloat r)))) (E.Lit (E.Number (toFloat g)))) (E.Lit (E.Number (toFloat b)))) (E.Lit (E.Number 1.0))

ap = E.App
expr1 = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (E.Lit (E.Builtin "cell") `ap` E.Lit (E.Builtin "swatch") `ap` rgbTerm 20 28 145))
-- this bombs
-- expr = E.Ref "uno" `ap` E.Ref "dos" `ap` E.Ref "tres" `ap` E.Ref "quatro" `ap` E.Ref "cinco" `ap` E.Ref "seis" `ap` E.Ref "siete" `ap` E.Ref "ocho"
expr = E.App (E.App (E.Ref "foo") nums) (E.App (E.Ref "baz") (rgbTerm 20 28 145))

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
                , pixelsPerInch = 130 -- todo, get real value from somewhere
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

      scene : L.Layout x -> Element -> Element
      scene l selection = Element.layers [L.element l, selection]
  in scene <~ rendered ~ highlightLayer

-- make sole UI state a Term
-- certain terms are "special"
