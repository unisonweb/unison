module Unison.Explorer where

import Elmz.Moore (Moore)
import Elmz.Moore as M
import Elmz.Layout as Layout
import Elmz.Layout (Layout,Region)
import Elmz.Maybe
import List
import List ((::))
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Input.Field as Field
import Keyboard
import Signal
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

autocomplete : S v -> Layout (Maybe (v,Int))
autocomplete s =
  let ok = List.length (s.match s.input.string (List.map snd s.completions)) > 0
      statusColor = if ok then Styles.okColor else Styles.notOkColor
      fld = Field.field (Styles.autocomplete ok)
                        (Signal.send s.searchbox)
                        s.prompt
                        s.input
      insertion = Styles.carotUp 7 statusColor
      fldWithInsertion = E.flow E.down [E.spacer 1 1, E.flow E.right [E.spacer 8 0, insertion], fld]
      status = Layout.above Nothing (Layout.embed Nothing s.goal)
                                    (Layout.embed Nothing s.current)
      renderCompletion i (e,v) = Layout.embed (Just (v,i)) e
      box = Layout.above Nothing
        (Layout.embed Nothing fldWithInsertion)
        (Styles.verticalCells Nothing E.empty (status :: List.indexedMap renderCompletion s.completions))
      boxTopLeft = { x = s.focus.topLeft.x, y = s.focus.topLeft.y + s.focus.height }
      h = boxTopLeft.y + E.heightOf (Layout.element box)
  in Layout.container Nothing s.overall.width h boxTopLeft box

type Direction = North | South | East | West

type alias S v =
  { isKeyboardOpen : Bool
  , prompt : String
  , goal : Element
  , current : Element
  , input : Field.Content
  , searchbox : Signal.Channel Field.Content
  , focus : Region
  , overall : Region
  , match : String -> List v -> List v
  , completions : List (Element,v) }
