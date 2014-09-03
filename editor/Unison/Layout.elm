module Unison.Layout where

import Array
import Either(..)
import Graphics.Element as E
import Graphics.Element (Element)

type Pt = { x : Int, y: Int }

type Region = { topLeftCorner : Pt, width : Int , height : Int }

type Layout k =
  { element : Element   -- the rendering
  , at : Pt -> k -- resolve a screen position to a key
  , select : k -> Maybe Region -- resolve a key to a
  }

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f = maybe Nothing (Just . f)

pxmap : (k -> k2) -> (k2 -> Maybe k) -> Layout k -> Layout k2
pxmap to from l =
  { element = l.element
  , at k = to (l.at k)
  , select k2 = case from k2 of
      Nothing -> Nothing
      Just k -> l.select k
  }

empty : Layout k
empty = { element = E.empty, at _ = todo, select _ = Nothing }

element : k -> Element -> Layout k
element k e =
  { element = e
  , at _ = k
  , select k2 = if k == k2
                then Just (Region (Pt 0 0) (widthOf e) (heightOf e))
                else Nothing
  }

reselect : (k -> Maybe Region) -> Layout k -> Layout k
reselect select l = { l | select <- select }

select : (k -> Maybe Region) -> Layout k -> Layout k
select sel l = { l | select <- \k -> maybe (sel k) Just (l.select k) }

beside : Layout k -> Layout k -> Layout k
beside l r =
  { element = E.flow right [l.element, r.element]
  , at {x,y} = if x <= E.widthOf l.element
               then l.at (Pt x (min (E.heightOf l.element) y))
               else r.at (Pt (x - E.widthOf l.element) (min (E.heightOf r.element) y))
  , select k = case l.select k of
      Nothing -> r.select k
      o -> o
  }

above : Layout k -> Layout k -> Layout k
above top bot =
  { element = E.flow down [top.element, bot.element]
  , at {x,y} = if y <= E.heightOf top.element
               then top.at (Pt (min (E.widthOf top.element) x) y)
               else bot.at (Pt (min (E.widthOf bot.element) x) (y - E.heightOf top.element))
  , select k = case top.select k of
      Nothing -> bot.select k
      o -> o
  }

horizontal : [Layout k] -> Layout k
horizontal = reduceBalanced empty beside

vertical : [Layout k] -> Layout k
vertical = reduceBalanced empty above

section : k -> Layout k -> [Layout k] -> Layout k
section root hdr subsections =
  let col = vertical subsections
      sel k = if k == root
              then Just (Region (Pt 0 0) (E.widthOf hdr.element + E.widthOf col.element)
                                         (E.heightOf hdr.element + E.heightOf col.element))
              else Nothing
      spacer = E.spacer (E.widthOf hdr.element)
                        (E.heightOf col.element `max` E.heightOf hdr.element - E.heightOf hdr.element)
            |> element root
            |> reselect sel
      left = if E.heightOf col.element > E.heightOf hdr.element
             then hdr `above` spacer
             else hdr
  in left `beside` col

reduceBalanced : a -> (a -> a -> a) -> [a] -> a
reduceBalanced zero op xs =
  let go xs =
    let len = Array.length xs
    in if | len == 0  -> zero
          | len == 1  -> Array.getOrFail 0 xs
          | otherwise -> let mid = floor (toFloat len / 2)
                         in go (Array.slice 0 mid xs) `op` go (Array.slice mid len xs)
  in go (Array.fromList xs)

todo : a
todo = todo

