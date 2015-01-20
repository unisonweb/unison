module Unison.Explorer where

import Debug
import Elmz.Layout (Layout,Pt,Region)
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

-- model for overall editor is
-- Term
-- Maybe Scope.Model
-- Explorer.Model
-- can get a bit fancier with tracking dependencies,
-- result of previous evaluations
-- mapping from paths to hashes

type alias Model = Maybe
  { isKeyboardOpen : Bool
  , prompt : String
  , input : Field.Content
  , instructions : Element
  , completions : List Element
  , invalidCompletions : List Element }

type alias Action = Model -> Model

-- actions for setting the instructions,
-- completions, invalid completions, input, and keyboard open

zero : Model
zero = Just
  { isKeyboardOpen = False
  , prompt = ""
  , input = Field.noContent
  , instructions = E.empty -- fill with sweet animated GIF
  , completions = []
  , invalidCompletions = [] }

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

-- may need another Signal Bool input, which lets the explorer be closed 'externally'
-- or may want a `Signal Pt` which lets origin be moved
-- todo: can replace all Err with the zero Model, and foldp over this to get our Model states
{-
actions : { enter : Signal Bool
          , click : Signal ()
          , mouse : Signal (Int,Int)
          , isOpen : Signal Bool
          , upDown : Signal Movement.D1
          , completions : Signal
       -> Signal ()
       -> Signal (Int,Int)
       -> Signal Bool
       -> Signal Movement.D1
       -> Signal (List v)
       -> Signal (Action v)
actions {enter,click,mouse,isOpen,upDown} values =
  let merge = Signals.mergeWith (\a1 a2 model -> a1 model `Result.andThen` a2)
  in completions values `merge`
     movements upDown `merge`
     clicks click isOpen values `merge`
     enters enter values
-}

type alias Sink a = a -> Signal.Message

view : Pt -> Sink Field.Content -> Sink Bool -> Model -> Layout (Maybe Int)
view origin searchbox active model = case model of
  Nothing -> Layout.empty Nothing
  Just s ->
    let ok = not (List.isEmpty s.completions)
        statusColor = Styles.statusColor ok
        fld = Field.field (Styles.autocomplete ok)
                          searchbox
                          s.prompt
                          s.input
        insertion = Styles.carotUp 7 statusColor
        status = Layout.embed Nothing s.instructions
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
    in Layout.container Nothing (boxTopLeft.x + Layout.widthOf box) h boxTopLeft box
