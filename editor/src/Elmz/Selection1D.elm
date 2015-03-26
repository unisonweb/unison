{-| One-dimensional selection model. -}
module Elmz.Selection1D where

import Elmz.Layout (Containment, Layout, Region)
import Elmz.Layout as Layout
import Elmz.Movement as Movement
import Elmz.Moore (Moore(..))
import Elmz.Moore as Moore
import Elmz.Signal as Signals
import Graphics.Element (Element)
import Graphics.Element as Element
import Maybe
import Result
import Signal
import List

type Event v
  = Move Movement.D1 -- Movement of the selection up or down
  | Mouse (Int,Int) -- Movement of the mouse
  | View (Layout (Maybe Int)) -- A change to the layout
  | Values (List v) -- A change to the underlying list

type alias Model v = Moore (Event v) (Maybe Region, Maybe Int)

model : Model v
model =
  let
    novalues e = case e of
      Values vs -> if List.isEmpty vs then Nothing
                     else Just (Moore (Nothing, Just 0) (at 0 vs))
      _ -> Nothing

    at index values e = case e of
      View layout ->
        let next region = Moore (Just region, Just index) (interactive index values layout)
        in Maybe.map next (region index layout)
      _ -> Nothing

    interactive ind values layout e = let limitExclusive = List.length values - 1 in case e of
      Move (Movement.D1 sign) ->
        let
          index' = case sign of
            Movement.Positive -> (limitExclusive - 1) `min` (ind + 1) `max` 0
            Movement.Negative -> 0 `max` (ind-1)
            Movement.Zero -> ind
        in
          if ind == index' then Nothing
          else Just <| Moore (region index' layout, Just index') (interactive index' values layout)
      View layout -> Just <|
        let r = region ind layout
        in Moore (r, Maybe.map (always ind) r) (interactive ind values layout)
      Mouse (x,y) -> Layout.leafAtPoint layout (Layout.Pt x y) `Maybe.andThen`
        \i -> i `Maybe.andThen` -- Layout.leafAtPoint returns a Maybe, unwrap that
        \i -> if i == ind then Nothing
              else Just (Moore (region i layout, Just i) (interactive i values layout))
      Values values' -> if values' == values then Nothing else case index ind values of
        Nothing -> Just state0
        Just v ->
          let ind' = Maybe.withDefault 0 (indexOf ((==) v) values')
          in if List.isEmpty values'
             then Just state0
             else Just (Moore (region ind' layout, Just ind') (interactive ind' values' layout))
      _ -> Nothing

    state0 = Moore (Nothing,Nothing) novalues
    region index layout =
      Layout.selectableLub (always True)
                           (Layout.region (<=) (Maybe.withDefault (-1)) layout index)
  in state0

index : Int -> List a -> Maybe a
index i l = case List.drop i l of
  h :: _ -> Just h
  _ -> Nothing

indexOf : (a -> Bool) -> List a -> Maybe Int
indexOf f l = List.indexedMap (\i a -> (i, f a)) l
           |> List.filterMap (\(i,b) -> if b then Just i else Nothing)
           |> index 0
