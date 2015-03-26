module Elmz.Matcher where

import Elmz.Moore as Moore
import Elmz.Moore (Moore(..))
import List
import String

type Event a
  = Query String
  | Results { query : String
            , values : List a
            , additionalResults : Int
            , positionsExamined : List Int }

type alias Model a =
  Moore (Event a) { matches : List a, query : Maybe String }

model : (String -> a -> Bool) -> Model a
model matches =
  let
    missingResults q = { query = q, values = [], additionalResults = 0, positionsExamined = [] }
    empty e = case e of
      Query q -> Just (Moore { matches = [], query = Just q } (nonempty (missingResults q)))
      _ -> Nothing
    nonempty r e = Just <| case e of
      Query q ->
        let
          r' = { r | query <- q }
          o = out r'
          -- tricky part is determining whether we need to do another search
          full = r.additionalResults <= 0
          lastExamined = List.maximum (-1 :: r.positionsExamined)
          ok = -- we've added characters to a search with all results
             (full && String.startsWith r.query q) ||
             -- we've deleted characters, but not past where we have complete results
             (full && String.startsWith q r.query && lastExamined < String.length q)
        in
          Moore { o | query <- if ok then Nothing else Just q }
                (nonempty (if ok then r' else missingResults q))
      Results r' -> Moore (out r') (nonempty r')
    out r = { matches = List.filter (matches r.query) r.values, query = Nothing }
  in
    Moore { matches = [], query = Nothing } empty
