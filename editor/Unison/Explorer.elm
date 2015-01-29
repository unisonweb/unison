module Unison.Explorer where

import Debug
import Elmz.Layout (Layout,Pt,Region,Containment(Inside,Outside))
import Elmz.Layout as Layout
import Elmz.Maybe
import Elmz.Movement as Movement
import Elmz.Selection1D
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

type alias Model = Maybe
  { isKeyboardOpen : Bool
  , prompt : String
  , input : Field.Content
  , instructions : Element
  , completions : List Element
  , invalidCompletions : List Element }

type alias Action = Model -> Model

zero : Model
zero = Just
  { isKeyboardOpen = False
  , prompt = ""
  , input = Field.noContent
  , instructions = E.empty -- fill with sweet animated GIF
  , completions = []
  , invalidCompletions = [] }

setPrompt : Signal String -> Signal Action
setPrompt event =
  let f s model = Maybe.map (\m -> { m | prompt <- s }) model
  in Signal.map f event

openKeyboard : Signal () -> Signal Action
openKeyboard event =
  let f _ model = Maybe.map (\m -> { m | isKeyboardOpen <- True }) model
  in Signal.map f event

setInput : Signal Field.Content -> Signal Action
setInput content =
  let f c model = Maybe.map (\m -> { m | input <- c }) model
  in Signal.map f content

setInstructions : Signal Element -> Signal Action
setInstructions e =
  let f e model = Maybe.map (\m -> { m | instructions <- e }) model
  in Signal.map f e

setCompletions : Signal (List Element) -> Signal Action
setCompletions e =
  let f e model = Maybe.map (\m -> { m | completions <- e }) model
  in Signal.map f e

setInvalidCompletions : Signal (List Element) -> Signal Action
setInvalidCompletions e =
  let f e model = Maybe.map (\m -> { m | invalidCompletions <- e }) model
  in Signal.map f e

clicks : { tl | click : Signal (), inside : Signal Bool, allowOpen : Signal Bool } -> Signal Action
clicks {click,inside,allowOpen} =
  let f inside allowOpen model = case model of
        Nothing -> if allowOpen then zero else Nothing -- open explorer on click if allowed
        Just model -> if inside then Just model else Nothing -- close explorer on click outside region
  in Signal.sampleOn click (Signal.map2 f inside allowOpen)

enters : { tl | down : Signal Bool, allowOpen : Signal Bool } -> Signal Action
enters {down,allowOpen} =
  let f enterPressed allowOpen model = case model of
        Nothing -> if allowOpen then zero else Nothing
        Just _ -> Nothing
  in Signal.sampleOn down (Signal.map2 f down allowOpen)

type alias Sink a = a -> Signal.Message

view : Pt -> Sink Field.Content -> Sink Bool -> Model -> Layout (Result Containment Int)
view origin searchbox active model = case model of
  Nothing -> Layout.empty (Result.Err Outside)
  Just s ->
    let ok = not (List.isEmpty s.completions)
        statusColor = Styles.statusColor ok
        fld = Field.field (Styles.autocomplete ok)
                          searchbox
                          s.prompt
                          s.input
        insertion = Styles.carotUp 7 statusColor
        inside = Result.Err Inside
        status = Layout.embed inside s.instructions
              |> Layout.transform (Input.clickable (active True))
        renderCompletion i e = Layout.embed (Result.Ok i)
                                            (Input.clickable (active False) e)
        invalids = List.map (Layout.embed inside) s.invalidCompletions
        top = Layout.embed inside (Input.clickable (active True) fld)
           |> Layout.transform (Input.clickable (active True))
        spacer = Layout.embed inside (E.spacer 1 7)
        bot = Styles.explorerCells inside <|
          status :: List.indexedMap renderCompletion s.completions
          `List.append` invalids
        top' = Layout.transform (E.width (Layout.widthOf bot)) top
        box = Layout.above inside
          (Layout.embed inside (E.beside (E.spacer 14 1) insertion))
          (Layout.above inside (Layout.above (Layout.tag top) top' spacer) bot)
        boxTopLeft = origin
        h = boxTopLeft.y + Layout.heightOf box + 50
    in Layout.container (Result.Err Outside)
                        (boxTopLeft.x + Layout.widthOf box)
                        h
                        boxTopLeft
                        box
