module Unison.Explorer where

import Debug
import Elmz.Layout (Layout,Pt,Region)
import Elmz.Layout as Layout
import Elmz.Maybe
import Elmz.Movement as Movement
import Elmz.Signal as Signals
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Input as Input
import Graphics.Input.Field as Field
import Keyboard
import List
import List ((::))
import Maybe
import Mouse
import Result
import Set
import Signal
import String
import Time
import Unison.Styles as Styles
import Window

type alias Model =
  { isKeyboardOpen : Bool
  , prompt : String
  , goal : Element
  , current : Element
  , input : Field.Content
  , width : Int -- ditto
  , completions : List Element
  , selectedIndex : Int -- index into completions
  , invalidCompletions : List Element }

type alias Action v = Model -> Result (Maybe v) Model

empty : Action v
empty model = Result.Ok model

-- mouse movements - needs the view, can get the Signal Model
-- from integrating all other events
-- need to separate the list selection

move : Movement.D1 -> Action v
move (Movement.D1 sign) model = case sign of
  Movement.Zero -> Result.Ok model
  Movement.Positive ->
    if model.selectedIndex < List.length model.completions
    then Result.Ok { model | selectedIndex <- model.selectedIndex + 1 }
    else Result.Ok model
  Movement.Negative ->
    if model.selectedIndex < List.length model.completions
    then Result.Ok { model | selectedIndex <- model.selectedIndex - 1 }
    else Result.Ok model

movements : Signal Movement.D1 -> Signal (Action v)
movements d1s = Signal.map move d1s

completions : Signal (List v) -> Signal (Action v)
completions values =
  let vlag = Signals.delay [] values
      f prev cur model =
        if prev == cur then Result.Ok model
        else let prevVal = index model.selectedIndex prev
             in case prevVal of
               Nothing -> Result.Ok model
               Just v -> let ind' = Maybe.withDefault 0 (indexOf ((==) v) cur)
                         in Result.Ok { model | selectedIndex <- ind' }
  in Signal.map2 f vlag values

-- a click while the isOpen signal is `False` closes the explorer
clicks : Signal () -> Signal Bool -> Signal (List v) -> Signal (Action v)
clicks click isOpen values =
  let go isOpen values model =
        if isOpen then Result.Ok model
        else Result.Err (index model.selectedIndex values)
  in Signal.sampleOn click (Signal.map2 go isOpen values)

enters : Signal Bool -> Signal (List v) -> Signal (Action v)
enters enter values =
  let go enterPressed values model =
        if enterPressed then Result.Err (index model.selectedIndex values)
        else Result.Ok model
  in Signal.sampleOn enter (Signal.map2 go enter values)

-- may need another Signal Bool input, which lets the explorer be closed 'externally'
-- or may want a `Signal Pt` which lets origin be moved
-- todo: can replace all Err with the zero Model, and foldp over this to get our Model states

actions : Signal Bool
       -> Signal ()
       -> Signal (Int,Int)
       -> Signal Bool
       -> Signal Movement.D1
       -> Signal (List v)
       -> Signal (Action v)
actions enter click mouse isOpen upDown values =
  let merge = Signals.mergeWith (\a1 a2 model -> a1 model `Result.andThen` a2)
  in completions values `merge`
     movements upDown `merge`
     clicks click isOpen values `merge`
     enters enter values

type alias Sink a = a -> Signal.Message

view : Pt -> Sink Field.Content -> Sink Bool -> Model -> Layout (Maybe Int)
view origin searchbox active s =
  let ok = not (List.isEmpty s.completions)
      statusColor = Styles.statusColor ok
      fld = Field.field (Styles.autocomplete ok)
                        searchbox
                        s.prompt
                        s.input
      insertion = Styles.carotUp 7 statusColor
      status = Layout.above Nothing (Layout.embed Nothing s.goal)
                                    (Layout.embed Nothing s.current)
            |> Layout.transform (Input.clickable (active True))
      renderCompletion i e = Layout.embed (Just i) (Input.clickable (active False) e)
      invalids = List.map (Layout.embed Nothing) s.invalidCompletions
      top = Layout.embed Nothing (Input.clickable (active True) fld)
         |> Layout.transform (Input.clickable (active True))
      spacer = Layout.embed Nothing (E.spacer 1 7)
      bot = Styles.explorerCells Nothing <|
        status :: List.indexedMap renderCompletion s.completions
        `List.append` invalids
      top' = Layout.transform (E.width (Layout.widthOf bot)) top
      box = Layout.above Nothing
        (Layout.embed Nothing (E.beside (E.spacer 14 1) insertion))
        (Layout.above Nothing (Layout.above (Layout.tag top) top' spacer) bot)
      boxTopLeft = origin
      h = boxTopLeft.y + Layout.heightOf box + 50
  in Layout.container Nothing s.width h boxTopLeft box

index : Int -> List a -> Maybe a
index i l = case List.drop i l of
  h :: _ -> Just h
  _ -> Nothing

indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf f l = List.indexedMap (\i a -> (i, f a)) l
           |> List.filterMap (\(i,b) -> if b then Just i else Nothing)
           |> index 0
