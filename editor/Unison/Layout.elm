module Unison.Layout where

import Unison.Trie (Trie)
import Unison.Trie as T
import Array
import Either(..)
import Graphics.Element as E
import Graphics.Element (Element, Direction)

type Pt = { x : Int, y: Int }

type Region = { topLeftCorner : Pt, width : Int , height : Int }

type Layout k =
  { element : Element      -- the rendering
  , at : Pt -> [k]         -- resolve a screen position to a key
  , select : Trie k Region -- resolve a key to a screen region
  }

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f = maybe Nothing (Just . f)

empty : Layout k
empty = { element = E.empty, at _ = [], select = T.empty }

element : Element -> Layout k
element e =
  { element = e
  , at _ = []
  , select = T.unit (Region (Pt 0 0) (widthOf e) (heightOf e))
  }

nest : k -> Layout k -> Layout k
nest root l =
  { l | select <- T.nest root l.select }

-- Add some extra stuff to the given layout
-- extend : Direction -> [Element] -> Layout k -> Layout k
-- extend dir extra l = { l | element <- flow dir (l.element :: extra) }

-- container, like extend
-- nestedContainer
--
{-
nestBeside : k -> [Element] -> Layout k -> Layout k
nestBeside root extra l =
  let overall = flow right (l.element :: extra)
      sel k = if k == root
              then Just (Region (Pt 0 0) (E.widthOf l.element) (E.heightOf l.element))
              else
-}

beside : Layout k -> Layout k -> Layout k
beside l r =
  { element = E.flow right [l.element, r.element]
  , at {x,y} = if x <= E.widthOf l.element
               then l.at (Pt x (min (E.heightOf l.element) y))
               else r.at (Pt (x - E.widthOf l.element) (min (E.heightOf r.element) y))
  , select = let shift r = { r | topLeftCorner <- Pt (E.widthOf l.element) 0 }
             in T.merge l.select (T.map shift r.select)
  }

above : Layout k -> Layout k -> Layout k
above top bot =
  { element = E.flow down [top.element, bot.element]
  , at {x,y} = if y <= E.heightOf top.element
               then top.at (Pt (min (E.widthOf top.element) x) y)
               else bot.at (Pt (min (E.widthOf bot.element) x) (y - E.heightOf top.element))
  , select = let shift r = { r | topLeftCorner <- Pt 0 (E.heightOf top.element) }
             in T.merge top.select (T.map shift bot.select)
  }

horizontal : [Layout k] -> Layout k
horizontal = reduceBalanced empty beside

vertical : [Layout k] -> Layout k
vertical = reduceBalanced empty above

reduceBalanced : a -> (a -> a -> a) -> [a] -> a
reduceBalanced zero op xs =
  let go xs =
    let len = Array.length xs
    in if | len == 0  -> zero
          | len == 1  -> Array.getOrFail 0 xs
          | otherwise -> let mid = floor (toFloat len / 2)
                         in go (Array.slice 0 mid xs) `op` go (Array.slice mid len xs)
  in go (Array.fromList xs)

{-

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

-}
