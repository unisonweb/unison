module Unison.Explorer where

import Elmz.Moore (Moore)
import Elmz.Moore as M
import Elmz.Layout as Layout
import Elmz.Layout (Layout,Region)
import Elmz.Maybe
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

--explorer : Moore (S k v) (Element, Maybe (k,v))
--explorer =
--  let s0 = M.moore (E.empty, Nothing) closed
--      closed s = case s.focus of
--        Nothing -> s0
--        Just (k,r) -> opened k r s
--      opened k r s = todo
--  in s0

autocomplete : k -> Region -> S k v -> Layout (Maybe (k,v,Int))
autocomplete k selectedRegion s =
  let ok = case s.parse (s.input.string) of
        Nothing -> False
        Just v -> length (s.match s.input.string [v]) > 0
      statusColor = if ok then Styles.okColor else Styles.notOkColor
      fld = Field.field (Styles.autocomplete ok)
                        s.searchbox.handle
                        identity
                        s.prompt
                        s.input
      insertion = Styles.carotUp 7 statusColor
      fldWithInsertion = flow down [E.spacer 1 1, flow right [E.spacer 8 0, insertion], fld]
      status = Layout.above Nothing (Layout.embed Nothing s.goal)
                                    (Layout.embed Nothing s.current)
      renderCompletion i (e,v) = Layout.embed (Just (k,v,i)) e
      box = Layout.above Nothing
        (Layout.embed Nothing fldWithInsertion)
        (Styles.verticalCells Nothing E.empty (status :: indexedMap renderCompletion s.completions))
      boxTopLeft = { x = selectedRegion.topLeft.x, y = selectedRegion.topLeft.y + selectedRegion.height }
      h = boxTopLeft.y + E.heightOf (Layout.element box)
  in Layout.container Nothing s.overall.width h boxTopLeft box

data Direction = North | South | East | West

type S k v =
  { isKeyboardOpen : Bool
  , prompt : String
  , goal : Element
  , current : Element
  , input : Field.Content
  , searchbox : Input Field.Content
  , parse : String -> Maybe v
  , focus : Maybe (k, Region)
  , overall : Region
  , match : String -> [v] -> [v]
  , completions : [(Element,v)] }

-- define interactivity separate from the explorer
-- so the explorer doesn't get clicks or mouse movements, just worries about outputting a Layout
