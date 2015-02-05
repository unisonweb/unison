module Unison.Styles where

import Color
import Color (Color)
import Easing (Easing)
import Easing
import Elmz.Signal as Signals
import Elmz.Layout (Layout, Region)
import Elmz.Layout as L
import Graphics.Input.Field as Field
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage as C
import List
import List ((::))
import Signal
import Text (Style)
import Text as T
import Time

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

okColor = silver
notOkColor = alizarin

statusColor : Bool -> Color
statusColor ok = if ok then okColor else notOkColor

autocomplete : Bool -> Field.Style
autocomplete ok =
  { padding = let s = Field.defaultStyle
              in s.padding -- { left = 10, right = 10, top = s.padding.top, bottom = s.padding.bottom }
  , outline = { color = statusColor ok
              , width = Field.uniformly 4
              , radius = 0 }
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

chain1 : Int -> Color -> Element
chain1 x c =
  let block = E.spacer x x |> E.color c
      sep = E.spacer 1 1
  in E.flow E.down [ sep, block, sep ]

carotUp : Int -> Color -> Element
carotUp x c =
  let block = E.spacer x x |> E.color c
      sep = E.spacer 1 1
  in E.flow E.down [ sep, block, sep, block, sep, block, sep ]
  -- let r = ceiling (toFloat x * sqrt 2.0)
  -- in C.collage r r [ C.rotate (degrees 45) (C.filled c (C.square (toFloat x))) ]
  -- |> E.height (ceiling (toFloat x * sqrt 2.0 / 2.0))

codeText : String -> Element
codeText s = T.leftAligned (T.style code (T.fromString s))

numericLiteral : String -> Element
numericLiteral s = T.leftAligned (T.style { code | color <- belizeHole } (T.fromString s))

stringLiteral : String -> Element
stringLiteral s = T.leftAligned (T.style { code | color <- wisteria } (T.fromString s))

cells : k -> Element -> List (Layout k) -> Layout k
cells k ifEmpty ls = let cs = List.map (\l -> L.fill bg (L.pad 5 0 l)) (L.row ls) in case cs of
  [] -> L.outline clouds 1 (L.embed k ifEmpty)
  h :: _ -> let vline = L.embed k (E.spacer 1 (L.heightOf h) |> E.color clouds)
            in L.outline clouds 1 (L.intersperseHorizontal vline cs)

verticalCells : k -> Element -> List (Layout k) -> Layout k
verticalCells k ifEmpty ls = let cs = List.map (\l -> L.fill bg (L.pad 5 0 l)) (L.column ls) in case cs of
  [] -> L.outline clouds 1 (L.embed k ifEmpty)
  h :: _ -> let hline = L.embed k (E.spacer (L.widthOf h) 1 |> E.color clouds)
            in L.outline clouds 1 (L.intersperseVertical hline cs)

explorerCells : k -> List (Layout k) -> Layout k
explorerCells k ls =
  let cs = List.map (\l -> L.fill bg (L.pad 20 5 l)) (L.leftAlignedColumn ls)
  in case cs of
    [] -> L.empty k
    h :: _ -> let hsep = L.embed k (E.spacer 1 5 |> E.color bg)
              in L.intersperseVertical hsep cs |>
                 L.transform (\e -> E.layers [e, outlineOf okColor 8 e])

selection : Region -> Element
selection r =
  let n = 8
  in E.container (r.topLeft.x + r.width + 12)
                 (r.topLeft.y + r.height + 12)
                 (E.topLeftAt (E.absolute (r.topLeft.x - 6)) (E.absolute (r.topLeft.y - 6)))
                 (outline' okColor n (r.width + 12) (r.height + 12))
  -- selectionLayer highlight

explorerSelection : Region -> Element
explorerSelection = selectionLayer highlightExplorer

selectionLayer : (Int -> Int -> Element) -> Region -> Element
selectionLayer highlight r =
  let
    hl = highlight r.width r.height
    n = 1
  in E.container (r.topLeft.x + r.width)
                 (r.topLeft.y + r.height)
                 (E.topLeftAt (E.absolute (r.topLeft.x)) (E.absolute (r.topLeft.y)))
                 hl

highlight : Int -> Int -> Element
highlight width height =
  E.layers [ E.spacer width height |> E.color asbestos |> E.opacity 0.15
           , outline' asbestos 1 width height |> E.opacity 0.8 ]

highlightExplorer : Int -> Int -> Element
highlightExplorer width height =
  E.spacer width height |> E.color midnightBlue |> E.opacity 0.15

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

outlineOf : Color -> Int -> Element -> Element
outlineOf c thickness e = outline' c thickness (E.widthOf e) (E.heightOf e)

contain : Element -> Element
contain e =
  E.container (E.widthOf e) (E.heightOf e) E.middle e

spinner : Signal Element
spinner =
  let pct n = toFloat (n%60) / 60.0
      t = Signal.map pct (Signals.count (Time.fps 60))
      rect = E.color midnightBlue (E.spacer 5 10)
      sep = E.spacer 1 1
      render pct = E.flow E.right
        [ rect, sep
        , E.opacity (Easing.easeInOutExpo pct) rect ]
  in Signal.map render t

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

main =
  let scene e = E.flow E.down [ E.spacer 1 50, E.flow E.right [ E.spacer 50 1, e]]
  in Signal.map scene spinner
