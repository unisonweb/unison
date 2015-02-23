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
  , above : Element
  , completions : List Element
  , below : Element }

type alias Action = Model -> Model

zero : Model
zero = Just
  { isKeyboardOpen = False
  , prompt = ""
  , input = Field.noContent
  , above = E.empty -- todo: fill with sweet animated GIF
  , completions = []
  , below = E.empty }

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

setAbove : Element -> Action
setAbove e = Maybe.map (\m -> { m | above <- e })

setCompletions : List Element -> Action
setCompletions e = Maybe.map (\m -> { m | completions <- e })

setBelow : Element -> Action
setBelow e = Maybe.map (\m -> { m | below <- e })

type alias Sink a = a -> Signal.Message

view : Pt -> Sink Field.Content -> Model -> Layout (Result Containment Int)
view origin searchbox model = case model of
  Nothing -> Layout.empty (Result.Err Outside)
  Just s ->
    let ok = not (List.isEmpty s.completions)
        statusColor = Styles.statusColor ok
        fld = Field.field (Styles.autocomplete ok) searchbox s.prompt s.input
        completions =
          let fit e = E.width (E.widthOf s.above `max` E.widthOf e) e
          in List.indexedMap (\i e -> Layout.embed (Result.Ok i) (fit e)) s.completions
        inside = Result.Err Inside
        bottom = Styles.explorerOutline statusColor <|
          Layout.vertical inside
            [ Layout.embed inside s.above
            , Styles.explorerCells inside completions
            , Layout.embed inside s.below ]
        box = Layout.vertical inside
          [ Layout.embed inside (E.flow E.right [E.spacer 9 1, Styles.carotUp 6 statusColor])
          , Layout.embed inside (E.width (Layout.widthOf bottom `max` 60) fld)
          , Layout.embed inside (E.spacer 1 6)
          , bottom ]
        boxTopLeft = origin
        h = boxTopLeft.y + Layout.heightOf box + 50
    in Layout.container (Result.Err Outside)
                        (boxTopLeft.x + Layout.widthOf box)
                        h
                        boxTopLeft
                        box
