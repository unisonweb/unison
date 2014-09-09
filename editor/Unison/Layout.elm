module Unison.Layout where

import Unison.Trie (Trie)
import Unison.Trie as T
import Array as A
import Array (Array)
import Either(..)
import Graphics.Element as E
import Graphics.Element (Direction, Element, Position)

type Pt = { x : Int, y: Int }

type Region = { topLeft : Pt, width : Int , height : Int }

data LayoutF r
  = Beside r r
  | Above r r
  | Container { width : Int, height : Int, innerTopLeft : Pt } r
  | Embed Element

data Layout k = Layout (LayoutF (Layout k)) Element k

tag : Layout k -> k
tag (Layout _ _ k) = k

element : Layout k -> Element
element (Layout _ e _) = e

transform : (Element -> Element) -> Layout k -> Layout k
transform f (Layout l e k) = Layout l (f e) k

widthOf : Layout k -> Int
widthOf l = E.widthOf (element l)

heightOf : Layout k -> Int
heightOf l = E.heightOf (element l)

embed : k -> Element -> Layout k
embed k e = Layout (Embed e) e k

empty : k -> Layout k
empty k = embed k E.empty

beside : k -> Layout k -> Layout k -> Layout k
beside k left right =
  Layout (Beside left right) (element left `E.beside` element right) k

above : k -> Layout k -> Layout k -> Layout k
above k top bot =
  Layout (Above top bot) (element top `E.beside` element bot) k

horizontal : k -> [Layout k] -> Layout k
horizontal k ls = reduceBalanced (empty k) (beside k) ls

vertical : k -> [Layout k] -> Layout k
vertical k ls = reduceBalanced (empty k) (above k) ls

intersperseHorizontal : Layout k -> [Layout k] -> Layout k
intersperseHorizontal sep ls =
  horizontal (tag sep) (intersperse sep ls)

intersperseVertical : Layout k -> [Layout k] -> Layout k
intersperseVertical sep ls =
  vertical (tag sep) (intersperse sep ls)

container : k -> Int -> Int -> Pt -> Layout k -> Layout k
container k w h pt l =
  let pos = E.topLeftAt (E.absolute pt.x) (E.absolute pt.y)
      e   = E.container w h pos (element l)
  in Layout (Container { width = w, height = h, innerTopLeft = pt } l) e k

pad : Int -> Int -> Layout k -> Layout k
pad eastWestPad northSouthPad l =
  container (tag l)
            (widthOf l + eastWestPad*2)
            (heightOf l + northSouthPad*2)
            (Pt eastWestPad northSouthPad)
            l

outline : Color -> Int -> Layout k -> Layout k
outline c thickness l =
  pad thickness thickness l |> transform (color c)

fill : Color -> Layout k -> Layout k
fill c e = container (tag e) (widthOf e) (heightOf e) (Pt 0 0) e
        |> transform (color c)

-- roundedOutline : k -> Int -> Color -> Int -> Layout { k | element : Element } -> Layout { k | element : Element }
-- roundedOutline k cornerRadius c thickness l = todo

row : [Layout k] -> [Layout k]
row ls = case ls of
  [] -> []
  _ -> let maxh = maximum (map heightOf ls)
           cell e = let diff = maxh - heightOf e
                    in if diff == 0 then e
                       else container (tag e) (widthOf e) maxh (Pt 0 (toFloat diff / 2 |> floor)) e
       in (map cell ls)

column : [Layout k] -> [Layout k]
column ls = case ls of
  [] -> []
  _ -> let maxw = maximum (map widthOf ls)
           cell e = let diff = maxw - widthOf e
                    in if diff == 0 then e
                       else container (tag e) maxw (heightOf e) (Pt (toFloat diff / 2 |> floor) 0) e
       in (map cell ls)

-- cell : Layout { k | element : Element } -> Layout { k | element : Element }
-- cell = nest pad 10 2

--cells : Element -> [Element] -> Element
--cells ifEmpty xs =
--  let space = cell (codeText " ")
--  in if isEmpty xs
--     then ifEmpty
--     else intersperse (spacer 1 (heightOf space) |> color silver) (map cell (row xs))
--          |> flow right
--          |> fill bg
--          |> outline silver
--
--verticalCells : Element -> [Element] -> Element
--verticalCells ifEmpty xs =
--  if isEmpty xs
--  then ifEmpty
--  else let cells = map cell xs
--           maxw = maximum (map widthOf cells) + 1
--       in intersperse (spacer maxw 1 |> color silver) (map cell xs)
--       |> flow down
--       |> fill white
--       |> outline silver

{-| Find all regions in the tree whose path is equal to the given path.
    Relies on the assumption that nodes have paths which prefix paths
    of their descendents; thus, we can avoid searching any subtree whose
    path does not at least prefix the target path.

    More precisely: for any two nodes, `p`, and `c` of the `Layout`,
    where `c` is a descendent of `p`, `prefixOf (tag p).path (tag c).path`
    must be true.
-}
region : (k -> k -> Bool) -> Layout k -> k -> [(k, Region)]
region prefixOf l ks =
  let
    go origin ks (Layout layout e k) =
      if | ks == k -> [ (k, Region origin (E.widthOf e) (E.heightOf e)) ]
         | not (k `prefixOf` ks) -> [] -- avoid recursing on any subtrees which cannot possibly contain ks
         | otherwise -> case layout of
             Beside left right ->
               go origin ks left ++
               go { origin | x <- origin.x + widthOf left } ks right
             Above top bot ->
               go origin ks top ++
               go { origin | y <- origin.y + heightOf top } ks bot
             Container params inner ->
               go { origin | x <- origin.x + params.innerTopLeft.x, y <- origin.y + params.innerTopLeft.y }
                  ks
                  inner
             Embed e -> []
  in go (Pt 0 0) ks l

{-| Find all tags whose region contains the given point. -}
at : Layout k -> Pt -> [k]
at l pt =
  let
    within : Pt -> Int -> Int -> Pt -> Bool
    within topLeft w h pt =
      pt.x >= topLeft.x && pt.x <= topLeft.x + w &&
      pt.y >= topLeft.y && pt.y <= topLeft.y + h

    distinctCons : a -> [a] -> [a]
    distinctCons h t = case t of
      [] -> [h]
      ht :: tt -> if ht == h then t else h :: t

    go origin (Layout layout e k) =
      if not (within origin (E.widthOf e) (E.heightOf e) pt)
      then []
      else k `distinctCons` case layout of
        Beside left right ->
          go origin left ++
          go { origin | x <- origin.x + widthOf left } right
        Above top bot ->
          go origin top ++
          go { origin | y <- origin.y + heightOf top } bot
        Container params inner ->
          go { origin | x <- origin.x + params.innerTopLeft.x, y <- origin.y + params.innerTopLeft.y }
             inner
        Embed e -> []

  in go (Pt 0 0) l

lub : Region -> Region -> Region
lub r1 r2 =
  let topLeft = Pt (r1.topLeft.x `min` r2.topLeft.x) (r1.topLeft.y `min` r2.topLeft.y)
      botRight = Pt (r1.topLeft.x + r1.width `max` r2.topLeft.x + r2.width)
                    (r1.topLeft.y + r1.height `max` r2.topLeft.x + r2.height)
  in Region topLeft (botRight.x - topLeft.x) (botRight.y - topLeft.y)

selectableLub : [{ selectable : Bool, region : Region }] -> Maybe Region
selectableLub rs = case (filter .selectable rs) of
  [] -> Nothing
  rh :: rt -> Just (foldl lub rh.region (map .region rt))

reduceBalanced : a -> (a -> a -> a) -> [a] -> a
reduceBalanced zero op xs =
  let go xs =
    let len = A.length xs
    in if | len == 0  -> zero
          | len == 1  -> A.getOrFail 0 xs
          | otherwise -> let mid = floor (toFloat len / 2)
                         in go (A.slice 0 mid xs) `op` go (A.slice mid len xs)
  in go (A.fromList xs)
