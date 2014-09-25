module Unison.Layout where

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
  Layout (Above top bot) (element top `E.above` element bot) k

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
at : Layout k -> Region -> [k]
at l r =
  let
    bx1 = r.topLeft.x
    by1 = r.topLeft.y
    bx2 = bx1 + r.width
    by2 = by1 + r.height

    -- See http://silentmatt.com/rectangle-intersection/ for why this works
    intersects : Pt -> Int -> Int -> Bool
    intersects topLeft w h =
      let ax1 = topLeft.x
          ay1 = topLeft.y
          ax2 = ax1 + w
          ay2 = ay1 + h
      in ax1 < bx2 && ax2 > bx1 && ay1 < by2 && ay2 > by1

    distinctCons : a -> [a] -> [a]
    distinctCons h t = case t of
      [] -> [h]
      ht :: tt -> if ht == h then t else h :: t

    go origin (Layout layout e k) =
      if not (intersects origin (E.widthOf e) (E.heightOf e))
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

{-| Find all tags whose region intersects the given region,
    ordering and grouping results by the given ranking function.
    The first group returned will consist of elements with the
    highest rank, followed by elements with the next highest rank, etc.
-}
atRanked : (k -> Int) -> Layout k -> Region -> [[k]]
atRanked rank l r =
  let f k = (rank k, k)
      g (i, k) (i2, cur, acc) =
        if i == i2 then (i2, k :: cur, acc)
        else            (i, [k], reverse cur :: acc)
      done (_, cur, acc) = reverse cur :: acc
  in case map f (at l r) |> sortBy fst of
    [] -> []
    (i,k) :: tl -> foldl g (i, [k], []) tl |> done

lub : Region -> Region -> Region
lub r1 r2 =
  let topLeft = Pt (r1.topLeft.x `min` r2.topLeft.x) (r1.topLeft.y `min` r2.topLeft.y)
      botRight = Pt (r1.topLeft.x + r1.width `max` r2.topLeft.x + r2.width)
                    (r1.topLeft.y + r1.height `max` r2.topLeft.x + r2.height)
  in Region topLeft (botRight.x - topLeft.x) (botRight.y - topLeft.y)

selectableLub : (a -> Bool) -> [(a, Region)] -> Maybe Region
selectableLub f rs = case filter (f << fst) rs of
  [] -> Nothing
  rh :: rt -> Just (foldl lub (snd rh) (map snd rt))

reduceBalanced : a -> (a -> a -> a) -> [a] -> a
reduceBalanced z op xs =
  let fixup stack = case stack of
        (b,n) :: (a,m) :: t -> if n >= m then fixup ((op a b, n+m) :: t)
                               else (b,n) :: (a,m) :: t
        _ -> stack
      finalize stack = foldl op z (map fst stack)
  in foldl (\a stack -> fixup ((a,1) :: stack)) [] xs
  |> finalize
