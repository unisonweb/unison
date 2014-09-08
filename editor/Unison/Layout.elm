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

data Layout k = Layout k (LayoutF (Layout k))

nest : (Layout k -> LayoutF (Layout k)) -> Layout k -> Layout k
nest f l = Layout (key l) (f l)

key : Layout k -> k
key (Layout k _) = k

key' : Layout { k | element : Element } -> k
key' (Layout k _) = { k - element }

value : Layout k -> LayoutF (Layout k)
value (Layout _ v) = v

rekey : (k -> k) -> Layout k -> Layout k
rekey f (Layout k l) = Layout (f k) l

element : Layout { k | element : Element } -> Element
element (Layout k _) = k.element

widthOf : Layout { k | element : Element } -> Int
widthOf l = E.widthOf (element l)

heightOf : Layout { k | element : Element } -> Int
heightOf l = E.heightOf (element l)

embed : k -> Element -> Layout { k | element : Element }
embed k e = Layout { k | element = e } (Embed e)

empty : k -> Layout { k | element : Element }
empty k = embed k E.empty

beside : k -> Layout { k | element : Element }
           -> Layout { k | element : Element }
           -> Layout { k | element : Element }
beside k left right =
  let k' = { k | element = (key left).element `E.beside` (key right).element }
  in Layout k' (Beside left right)

above : k -> Layout { k | element : Element }
          -> Layout { k | element : Element }
          -> Layout { k | element : Element }
above k top bot =
  let k' = { k | element = (key top).element `E.above` (key bot).element }
  in Layout k' (Above top bot)

horizontal : k -> [Layout { k | element : Element }] -> Layout { k | element : Element }
horizontal k ls = reduceBalanced (empty k) (beside k) ls

vertical : k -> [Layout { k | element : Element }] -> Layout { k | element : Element }
vertical k ls = reduceBalanced (empty k) (above k) ls

intersperseHorizontal : Layout { k | element : Element }
                     -> [Layout { k | element : Element }]
                     -> Layout { k | element : Element }
intersperseHorizontal sep ls =
  let k = key sep
  in horizontal { k - element } (intersperse sep ls)

intersperseVertical : Layout { k | element : Element }
                   -> [Layout { k | element : Element }]
                   -> Layout { k | element : Element }
intersperseVertical sep ls =
  let k = key sep
  in vertical { k - element } (intersperse sep ls)

container' : Int -> Int -> Pt -> Layout k -> LayoutF (Layout k)
container' w h pt = Container { width = w, height = h, innerTopLeft = pt }

container : k -> Int -> Int -> Pt -> Layout { k | element : Element } -> Layout { k | element : Element }
container k w h pt l =
  let pos = E.topLeftAt (E.absolute pt.x) (E.absolute pt.y)
      e   = E.container w h pos (key l).element
  in Layout { k | element = e } (Container { width = w, height = h, innerTopLeft = pt } l)

pad : k -> Int -> Int -> Layout { k | element : Element } -> Layout { k | element : Element }
pad k eastWestPad northSouthPad l =
  container k (E.widthOf (key l).element + eastWestPad*2)
              (E.heightOf (key l).element + northSouthPad*2)
              (Pt eastWestPad northSouthPad)
              l

outline : k -> Color -> Int -> Layout { k | element : Element } -> Layout { k | element : Element }
outline k c thickness l =
  pad k thickness thickness l |> rekey (\k -> { k | element <- color c k.element })

-- fill : Color -> Layout k -> Layout k
-- fill c e = container (key' e) (widthOf e) (heightOf e) (Pt 0 0)
-- |> rekey (\r -> { r | element <- color c r.element })

-- roundedOutline : k -> Int -> Color -> Int -> Layout { k | element : Element } -> Layout { k | element : Element }
-- roundedOutline k cornerRadius c thickness l = todo

row : k -> [Layout { k | element : Element }] -> Layout { k | element : Element }
row k ls = case ls of
  [] -> empty k
  _ -> let maxh = maximum (map heightOf ls)
           cell e = let diff = maxh - heightOf e
                    in if diff == 0 then e
                       else e |> nest (container' (widthOf e) maxh (Pt 0 (toFloat diff / 2 |> floor)))
       in horizontal k (map cell ls)

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
    where `c` is a descendent of `p`, `prefixOf (key p).path (key c).path`
    must be true.
-}
region : (k -> k -> Bool) -> Layout { tl | element : Element, path : k } -> k -> [ { tl | region : Region } ]
region prefixOf l ks =
  let
    tl : { tl | element : a, path : b } -> tl
    tl r = let r' = { r - element } in { r' - path }
    go origin ks (Layout k layout) =
      if | ks == k.path -> [ tl { k | region = Region origin (E.widthOf k.element) (E.heightOf k.element) } ]
         | not (k.path `prefixOf` ks) -> [] -- avoid recursing on any subtrees which cannot possibly contain ks
         | otherwise -> case layout of
             Beside left right ->
               go origin ks left ++
               go { origin | x <- origin.x + E.widthOf (key left).element } ks right
             Above top bot ->
               go origin ks top ++
               go { origin | y <- origin.y + E.heightOf (key top).element } ks bot
             Container params inner ->
               go { origin | x <- origin.x + params.innerTopLeft.x, y <- origin.y + params.innerTopLeft.y }
                  ks
                  inner
             Embed e -> []
  in go (Pt 0 0) ks l

{-| Find all keys whose region contains the given point. -}
at : Layout { tl | path : k, element : Element } -> Pt -> [ { tl | path : k } ]
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

    go origin (Layout k layout) =
      if not (within origin (E.widthOf k.element) (E.heightOf k.element) pt)
      then []
      else { k - element } `distinctCons` case layout of
        Beside left right ->
          go origin left ++
          go { origin | x <- origin.x + E.widthOf (key left).element } right
        Above top bot ->
          go origin top ++
          go { origin | y <- origin.y + E.heightOf (key top).element } bot
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
