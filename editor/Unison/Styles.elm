module Unison.Styles where

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
codeText s = leftAligned (style code (toText s))

fill : Color -> Element -> Element
fill c e = container (widthOf e) (heightOf e)  (topLeftAt (absolute 0) (absolute 0)) e |> color c

outline : Color -> Element -> Element
outline c e =
  container (widthOf e + 2) (heightOf e + 2) (topLeftAt (absolute 1) (absolute 1)) e |> color c

outline2 : Color -> Element -> Element
outline2 c e =
  container (widthOf e + 6) (heightOf e + 6) (topLeftAt (absolute 3) (absolute 3)) e |> color c

pad : Int -> Int -> Element -> Element
pad hpad vpad e =
  container (widthOf e + hpad*2) (heightOf e + vpad*2) (topLeftAt (absolute hpad) (absolute vpad)) e

-- vertically center each cell in the row
row : [Element] -> [Element]
row es = case es of
  [] -> [empty]
  _ -> let maxh = maximum (map heightOf es)
           cell e = if heightOf e == maxh then e else container (widthOf e) maxh middle e
       in map cell es

cell : Element -> Element
cell = pad 10 2

cells : Element -> [Element] -> Element
cells ifEmpty xs =
  let space = cell (codeText " ")
  in if isEmpty xs
     then ifEmpty
     else intersperse (spacer 1 (heightOf space) |> color silver) (map cell (row xs))
          |> flow right
          |> fill white
          |> outline silver

verticalCells : Element -> [Element] -> Element
verticalCells ifEmpty xs =
  if isEmpty xs
  then ifEmpty
  else let cells = map cell xs
           maxw = maximum (map widthOf cells) + 1
       in intersperse (spacer maxw 1 |> color silver) (map cell xs)
       |> flow down
       |> fill white
       |> outline silver

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
