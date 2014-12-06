module Unison.Explorer where

import Elmz.Moore (Moore)
import Elmz.Moore as M
import Elmz.Layout as Layout
import Elmz.Layout (Layout,Region)
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Input as Input
import Graphics.Input (Input)
import Graphics.Input.Field as Field
import Keyboard
import Unison.Term (Term)
import Unison.Styles as Styles

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

-- question - how does `sampleOn` work?
-- if I have sampleOn (events f s) s, this seems like
-- need to track whether explorer is open or closed in a simpler way
-- maybe have the explorer just output the mode, and we choose whether
-- to display it or not separately

explorer : Moore (S k v) (Element, Maybe (k,v))
explorer =
  let s0 = M.moore (E.empty, Nothing) closed
      closed s = case s.focus of
        Nothing -> s0
        Just (k,r) -> opened k r s
      opened k r s = todo
  in s0

autocomplete : S k v -> Element
autocomplete s =
  let ok = case s.parse (s.input.string) of
        Nothing -> False
        Just v -> length (s.match s.input.string [v]) > 0
      fld = Field.field (Styles.autocomplete ok)
                        s.searchbox.handle
                        (.string >> s.parse)
                        ""
                        s.input
  in todo

-- data Mode e = Close | Accept e | Open Element
data Direction = North | South | East | West

type S k v =
  { isKeyboardOpen : Bool
  , goal : Element
  , current : Element
  , input : Field.Content
  , searchbox : Input (Maybe v)
  , parse : String -> Maybe v
  , focus : Maybe (k, Region)
  , overall : Region
  , match : String -> [v] -> [v]
  , completions : [(Element,v)]
  , mouse : (Int,Int)
  , click : Maybe (Int,Int)
  , movement : Maybe Direction }

