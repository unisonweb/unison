{-| One-dimensional selection model. -}
module Elmz.Selection1D where

import Elmz.Layout (Containment, Layout, Region)
import Elmz.Layout as Layout
import Elmz.Movement as Movement
import Elmz.Signal as Signals
import Graphics.Element (Element)
import Graphics.Element as Element
import Maybe
import Result
import Signal
import List

type alias Model = Int

type alias Action = Model -> Model

view : (Region -> Element) -> Layout (Result Containment Int) -> Model -> Element
view highlightLayer layout model =
  let extract r = case r of
        Result.Ok i -> i
        Result.Err _ -> -1
      regions = Layout.selectableLub (always True) (Layout.region (<=) extract layout model)
  in case regions of
    Nothing -> Element.empty
    Just region -> highlightLayer region

actions : { move : Signal Movement.D1
          , set : Signal Int
          , limit : Signal Int
          , mouse : Signal (Int,Int)
          , layout : Signal (Layout (Result Containment Int)) }
       -> Signal Action
actions {move,set,limit,mouse,layout} =
  let merge = Signals.mergeWith (>>)
  in movements move limit `merge`
     sets set `merge`
     resets mouse layout

resets : Signal (Int,Int) -> Signal (Layout (Result Containment Int)) -> Signal Action
resets mouse layout =
  let f (x,y) layout model = case Layout.leafAtPoint layout (Layout.Pt x y) of
    Just (Result.Ok i) -> i
    _ -> model
  in Signal.sampleOn mouse (Signal.map2 f mouse layout)

sets : Signal Model -> Signal Action
sets m = Signal.map always m

movements : Signal Movement.D1 -> Signal Int -> Signal Action
movements d1s limitExclusive =
  let f (Movement.D1 sign) limit model = case sign of
    Movement.Positive -> limit-1 `min` model+1
    Movement.Negative -> 0 `max` model-1
    Movement.Zero -> model
  in Signal.sampleOn d1s (Signal.map2 f d1s limitExclusive)

