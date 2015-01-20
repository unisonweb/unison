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
          , selection : Signal (List v)
          , limit : Signal Int
          , mouse : Signal (Int,Int)
          , layout : Signal (Layout (Result Containment Int)) }
       -> Signal Action
actions {move,selection,limit,mouse,layout} =
  let merge = Signals.mergeWith (>>)
  in selections selection `merge`
     movements move limit `merge`
     resets mouse layout

resets : Signal (Int,Int) -> Signal (Layout (Result Containment Int)) -> Signal Action
resets mouse layout =
  let f (x,y) layout model = case Layout.leafAtPoint layout (Layout.Pt x y) of
    Just (Result.Ok i) -> i
    _ -> model
  in Signal.sampleOn mouse (Signal.map2 f mouse layout)

movements : Signal Movement.D1 -> Signal Int -> Signal Action
movements d1s limitExclusive =
  let f (Movement.D1 sign) limit model = case sign of
    Movement.Positive -> limit-1 `min` model+1
    Movement.Negative -> 0 `max` model-1
    Movement.Zero -> model
  in Signal.sampleOn d1s (Signal.map2 f d1s limitExclusive)

-- If the list we are indexing into changes, try to update the index to
-- point to whatever the current index points to in the list's previous state
-- fall back to 0 otherwise
selections : Signal (List v) -> Signal Action
selections values =
  let vlag = Signals.delay [] values
      f prev cur model =
        if prev == cur then model
        else let prevVal = index model prev
             in case prevVal of
               Nothing -> model
               Just v -> Maybe.withDefault 0 (indexOf ((==) v) cur)
  in Signal.map2 f vlag values

index : Int -> List a -> Maybe a
index i l = case List.drop i l of
  h :: _ -> Just h
  _ -> Nothing

indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf f l = List.indexedMap (\i a -> (i, f a)) l
           |> List.filterMap (\(i,b) -> if b then Just i else Nothing)
           |> index 0
