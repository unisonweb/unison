module Unison.Styles where

import Color
import Color (Color)
import Elmz.Layout (Layout, Region)
import Elmz.Layout as L
import Graphics.Input.Field as Field
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage as C
import List
import List ((::))
import Text (Style)
import Text as T

body : Style
body =
  { typeface = [ "Lato", "latin" ]
  , height   = Just 16
  , color    = Color.black
  , bold     = False
  , italic   = False
  , line     = Nothing }

h1 : Style
h1 =
  { typeface = [ "Lato", "latin" ]
  , height   = Just 60
  , color    = Color.black
  , bold     = False
  , italic   = False
  , line     = Nothing }

okColor = turquoise
notOkColor = midnightBlue

statusColor : Bool -> Color
statusColor ok = if ok then okColor else notOkColor

autocomplete : Bool -> Field.Style
autocomplete ok =
  { padding = { left = 10, right = 10, top = 13, bottom = 13 }
  , outline = { color = statusColor ok
              , width = Field.uniformly 3
              , radius = 5 }
  , highlight = Field.noHighlight
  , style = code }

code : Style
code =
  { typeface = [ "Inconsolata", "monospace", "latin" ]
  , height   = Just 16
  , color    = Color.black
  , bold     = False
  , italic   = False
  , line     = Nothing }

carotUp : Int -> Color -> Element
carotUp x c =
  let r = ceiling (toFloat x * sqrt 2.0)
  in C.collage r r [ C.rotate (degrees 45) (C.filled c (C.square (toFloat x))) ]
  |> E.height (ceiling (toFloat x * sqrt 2.0 / 2.0))

codeText : String -> Element
codeText s = T.leftAligned (T.style code (T.fromString s))

numericLiteral : String -> Element
numericLiteral s = T.leftAligned (T.style { code | color <- belizeHole } (T.fromString s))

stringLiteral : String -> Element
stringLiteral s = T.leftAligned (T.style { code | color <- wisteria } (T.fromString s))

cells : k -> Element -> List (Layout k) -> Layout k
cells k ifEmpty ls = let cs = List.map (\l -> L.fill bg (L.pad 5 0 l)) (L.row ls) in case cs of
  [] -> L.outline silver 1 (L.embed k ifEmpty)
  h :: _ -> let vline = L.embed k (E.spacer 1 (L.heightOf h) |> E.color silver)
            in L.outline silver 1 (L.intersperseHorizontal vline cs)

verticalCells : k -> Element -> List (Layout k) -> Layout k
verticalCells k ifEmpty ls = let cs = List.map (\l -> L.fill bg (L.pad 5 0 l)) (L.column ls) in case cs of
  [] -> L.outline silver 1 (L.embed k ifEmpty)
  h :: _ -> let hline = L.embed k (E.spacer (L.widthOf h) 1 |> E.color silver)
            in L.outline silver 1 (L.intersperseVertical hline cs)

selection : Layout k -> Region -> Element
selection l r =
  let
    highlight = E.spacer r.width r.height |> E.color asbestos |> E.opacity 0.15
    n = 1
    border = outline' asbestos n r.width r.height |> E.opacity 0.8
  in E.container (L.widthOf l)
                 (L.heightOf l)
                 (E.topLeftAt (E.absolute (r.topLeft.x)) (E.absolute (r.topLeft.y)))
                 (E.layers [highlight, border])

highlight : Int -> Int -> Element
highlight width height =
  E.layers [ E.spacer width height |> E.color asbestos |> E.opacity 0.15
           , outline' asbestos 1 width height |> E.opacity 0.8 ]

outline : Color -> Int -> Element -> Element
outline c thickness e =
  E.container (E.widthOf e + thickness*2) (E.heightOf e + thickness*2)
              (E.topLeftAt (E.absolute thickness) (E.absolute thickness)) e
  |> E.color c

swatch : Color -> Element
swatch c =
  let e = E.color c (contain (codeText "  "))
      e2 = outline' Color.black 1 (E.widthOf e) (E.heightOf e)
  in E.layers [e, e2]

outline' : Color -> Int -> Int -> Int -> Element
outline' c thickness w h =
  let s = C.solid c
      s' = { s | width <- toFloat thickness }
  in C.collage w h [C.outlined s' (C.rect (toFloat w) (toFloat h))]

contain : Element -> Element
contain e =
  E.container (E.widthOf e) (E.heightOf e) E.middle e

blank : Element
blank = codeText "_"

bg = Color.white

-- http://flatuicolors.com/
turquoise = Color.rgb 26 188 156
greenSea = Color.rgb 22 160 133
sunFlowers = Color.rgb 241 196 15
orange = Color.rgb 243 156 18
emerald = Color.rgb 46 204 113
nephritis = Color.rgb 39 174 96
carrot = Color.rgb 230 126 34
pumpkin = Color.rgb 211 84 0
peterRiver = Color.rgb 52 152 219
belizeHole = Color.rgb 41 128 185
alizarin = Color.rgb 231 76 60
pomegranate = Color.rgb 192 57 43
amethyst = Color.rgb 155 89 182
wisteria = Color.rgb 142 68 173
clouds = Color.rgb 236 240 241
silver = Color.rgb 189 195 199
wetAsphalt = Color.rgb 52 73 94
midnightBlue = Color.rgb 44 62 80
concrete = Color.rgb 149 165 166
asbestos = Color.rgb 127 140 141
