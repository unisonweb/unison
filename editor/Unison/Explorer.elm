module Unison.Explorer where

import Elmz.Moore (Moore)
import Elmz.Moore as Moore
import Elmz.Layout as Layout
import Elmz.Layout (Layout,Region)
import Graphics.Element (Element)
import Keyboard
import Unison.Term (Term)

{-|

While CLOSED, on click/enter, if scope is currently defined, enter state OPEN
  * generates a popup immediately below the currently selected scope
  * popup has an input box and a list of search results
While OPEN
  * if on tablet/mobile and valid completions is small, avoid showing input box
    unless user clicks
  * arrow keys do not manipulate scope, they navigate around the explorer
    * similar behavior to scope manipulation, mouse movement cancels
  * clicks outside explorer popup cancel the edit
  * clicks inside input box handled by input box
  * hovers inside explorer pop up previews
  * click/enter of search result exits to CLOSED with a pending edit
  * display goal type
  * display current type
-}

todo : a
todo = todo

states : Moore (S k v, Mode (k,v)) (Mode (k,v))
states = todo

explorer : Moore (S k v) (Mode (k,v))
explorer = states
        |> Moore.map (\a -> (a,a))
        |> Moore.loop

data Mode e = Close | Accept e | Open Element
data Direction = North | South | East | West

type S k v =
  { isKeyboardOpen : Bool
  , focus : Maybe (k, Region)
  , overall : Region
  , completions : [Layout v]
  , highlight : Maybe Int -- index into `completions`
  , mouse : (Int,Int)
  , movement : Maybe Direction }

