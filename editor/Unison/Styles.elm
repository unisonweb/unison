module Unison.Styles where

import Text (Style)
import Text as T
import Graphics.Element as E
import Elmz.Layout (Layout, Region)
import Elmz.Layout as L

body : Style
body =
  { typeface = [ "Lato", "latin" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing }

code : Style
code =
  { typeface = [ "Inconsolata", "monospace", "latin" ]
  , height   = Just 16
  , color    = black
  , bold     = False
  , italic   = False
  , line     = Nothing }

codeText : String -> Element
codeText s = leftAligned (T.style code (toText s))

numericLiteral : String -> Element
numericLiteral s = leftAligned (T.style { code | color <- belizeHole } (toText s))

stringLiteral : String -> Element
stringLiteral s = leftAligned (T.style { code | color <- wisteria } (toText s))

cells : k -> Element -> [Layout k] -> Layout k
cells k ifEmpty ls = let cs = map (\l -> L.fill bg (L.pad 5 0 l)) (L.row ls) in case cs of
  [] -> L.outline silver 1 (L.embed k ifEmpty)
  h :: _ -> let vline = L.embed k (E.spacer 1 (L.heightOf h) |> E.color silver)
            in L.outline silver 1 (L.intersperseHorizontal vline cs)

verticalCells : k -> Element -> [Layout k] -> Layout k
verticalCells k ifEmpty ls = let cs = map (\l -> L.fill bg (L.pad 5 0 l)) (L.column ls) in case cs of
  [] -> L.outline silver 1 (L.embed k ifEmpty)
  h :: _ -> let hline = L.embed k (E.spacer (L.widthOf h) 1 |> E.color silver)
            in L.outline silver 1 (L.intersperseVertical hline cs)

selection : Layout k -> Region -> Element
selection l r =
  let
    linestyle = let d = dotted wetAsphalt in { d | width <- 4.0 }
    border = spacer r.width r.height |> color asbestos |> opacity 0.2
  in E.container (L.widthOf l)
                 (L.heightOf l)
                 (E.topLeftAt (E.absolute (r.topLeft.x)) (E.absolute (r.topLeft.y)))
                 border

outline : Color -> Int -> Element -> Element
outline c thickness e =
  E.container (E.widthOf e + thickness*2) (E.heightOf e + thickness*2)
              (E.topLeftAt (E.absolute thickness) (E.absolute thickness)) e
  |> E.color c

swatch : Color -> Element
swatch c =
  outline black 1 (E.color c (contain (codeText " ")))

contain : Element -> Element
contain e =
  container (E.widthOf e) (E.heightOf e) E.middle e

blank : Element
blank = outline silver 1 (E.spacer 19 19 |> color bg)

bg = white

-- http://flatuicolors.com/
turquoise = rgb 26 188 156
greenSea = rgb 22 160 133
sunFlowers = rgb 241 196 15
orange = rgb 243 156 18
emerald = rgb 46 204 113
nephritis = rgb 39 174 96
carrot = rgb 230 126 34
pumpkin = rgb 211 84 0
peterRiver = rgb 52 152 219
belizeHole = rgb 41 128 185
alizarin = rgb 231 76 60
pomegranate = rgb 192 57 43
amethyst = rgb 155 89 182
wisteria = rgb 142 68 173
clouds = rgb 236 240 241
silver = rgb 189 195 199
wetAsphalt = rgb 52 73 94
midnightBlue = rgb 44 62 80
concrete = rgb 149 165 166
asbestos = rgb 127 140 141
