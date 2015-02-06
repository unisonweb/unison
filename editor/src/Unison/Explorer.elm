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

setPrompt : String -> Action
setPrompt s = Maybe.map (\m -> { m | prompt <- s })

getInputOr : Field.Content -> Model -> Field.Content
getInputOr default model = case model of
  Nothing -> default
  Just model -> model.input

setInput : Field.Content -> Action
setInput content = Maybe.map (\m -> { m | input <- content })

openKeyboard : Action
openKeyboard = Maybe.map (\m -> { m | isKeyboardOpen <- True })

setInstructions : Element -> Action
setInstructions e = Maybe.map (\m -> { m | instructions <- e })

setCompletions : List Element -> Action
setCompletions e = Maybe.map (\m -> { m | completions <- e })

setInvalidCompletions : List Element -> Action
setInvalidCompletions e = Maybe.map (\m -> { m | invalidCompletions <- e })

click : { inside : Bool, allowOpen : Bool } -> Action
click {inside, allowOpen} model = case model of
  Nothing -> if allowOpen then zero else Nothing -- open explorer on click if allowed
  Just model -> if inside then Just model else Nothing -- close explorer on click outside region

enter : { down : Signal Bool, allowOpen : Bool } -> Action
enter {down,allowOpen} model = case model of
    Nothing -> if allowOpen then zero else Nothing
    Just _ -> Nothing

type alias Sink a = a -> Signal.Message

view : Pt -> Sink Field.Content -> Model -> Layout (Result Containment Int)
view origin searchbox model = case model of
  Nothing -> Layout.empty (Result.Err Outside)
  Just s ->
    let ok = not (List.isEmpty s.completions)
        statusColor = Styles.statusColor ok
        fld = Field.field (Styles.autocomplete ok)
                          searchbox
                          s.prompt
                          s.input
        insertion = Styles.carotUp 6 Styles.okColor
        inside = Result.Err Inside
        status = Layout.embed inside s.instructions
        renderCompletion i e = Layout.embed (Result.Ok i) e
        invalids = List.map (Layout.embed inside) s.invalidCompletions
        top = Layout.embed inside fld
        spacer = Layout.embed inside (E.beside (E.spacer 9 1) (Styles.chain1 6 Styles.okColor))
        bot = Styles.explorerCells inside <|
          status :: List.indexedMap renderCompletion s.completions
          `List.append` invalids
        fldWidth = (E.widthOf (Styles.codeText s.input.string) + 40) `max` 40
        -- top' = Layout.transform (E.width (Layout.widthOf bot)) top
        top' = Layout.transform (E.width fldWidth) top
        box = Layout.above inside
          (Layout.embed inside (E.beside (E.spacer 9 1) insertion))
          (Layout.above inside (Layout.above (Layout.tag top) top' spacer) bot)
        boxTopLeft = origin
        h = boxTopLeft.y + Layout.heightOf box + 50
    in Layout.container (Result.Err Outside)
                        (boxTopLeft.x + Layout.widthOf box)
                        h
                        boxTopLeft
                        box
