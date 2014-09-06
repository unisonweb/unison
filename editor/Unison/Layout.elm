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
  | Container { width : Int, height : Int, innerTopLeft : Pt, finish : Element -> Element } r
  | Embed Element

data Layout k = Layout k (LayoutF (Layout k))

key : Layout k -> k
key (Layout k _) = k

embed : k -> Element -> Layout k
embed k e = Layout k (Embed e)

empty : k -> Layout k
empty k = Layout k (Embed E.empty)

beside : k -> Layout k -> Layout k -> Layout k
beside k left right = Layout k (Beside left right)

above : k -> Layout k -> Layout k -> Layout k
above k top bot = Layout k (Above top bot)

horizontal : k -> [Layout k] -> Layout k
horizontal k ls = reduceBalanced (empty k) (beside k) ls

vertical : k -> [Layout k] -> Layout k
vertical k ls = reduceBalanced (empty k) (above k) ls

container : (Element -> Element) -> k -> Int -> Int -> Pt -> Layout k -> Layout k
container f k w h pt l =
  Layout k (Container { width = w, height = h, innerTopLeft = pt, finish = f } l)

float : k -> Int -> Int -> Pt -> Layout k -> Layout k
float = container id

render : Layout k -> Layout { k | element : Element }
render (Layout k layout) = case layout of
  Beside left right ->
    let rl = render left
        rr = render right
        e = (key rl).element `E.beside` (key rr).element
    in Layout { k | element = e } (Beside rl rr)
  Above top bot ->
    let rt = render top
        rb = render bot
        e = (key rt).element `E.above` (key rb).element
    in Layout { k | element = e } (Above rt rb)
  Container params r ->
    let rr = render r
        pos = E.topLeftAt (E.absolute params.innerTopLeft.x)
                          (E.absolute params.innerTopLeft.y)
        e = params.finish (E.container params.width params.height pos (key rr).element)
    in Layout { k | element = e } (Container params rr)
  Embed e -> Layout { k | element = e } (Embed e)

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

-- regionAt : Layout { tl | element : Element } -> Pt ->

-- this:
-- at : Pt -> Layout { _ | path : k, element : Element } -> k
-- regionAt : Pt -> Layout { _ | element : Element } -> Region
-- regionAt : Pt -> Layout { _ | path : k, element : Element } -> (k, Region)
-- select : Region -> Layout { _ | element : Element } -> (k,Region)
--
-- layout : Term -> Layout Path

todo : a
todo = todo

reduceBalanced : a -> (a -> a -> a) -> [a] -> a
reduceBalanced zero op xs =
  let go xs =
    let len = A.length xs
    in if | len == 0  -> zero
          | len == 1  -> A.getOrFail 0 xs
          | otherwise -> let mid = floor (toFloat len / 2)
                         in go (A.slice 0 mid xs) `op` go (A.slice mid len xs)
  in go (A.fromList xs)

