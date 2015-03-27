module Elmz.Matcher where

import Elmz.Moore as Moore
import Elmz.Moore (Moore(..))
import List
import String

type alias Q a = { string : String, values : List a }

type Event a
  = Query (Q a)
  | Results { query : Q a
            , results : List a
            , additionalResults : Int
            , positionsExamined : List Int }

type alias Model a =
  Moore (Event a) { matches : List a, query : Maybe String }

model : (String -> a -> Bool) -> Model a
model matches =
  let
    empty e = case e of
      Query q -> Just <|
        let o = { matches = List.filter (matches q.string) q.values, query = Just q.string }
        in Moore o (nonempty (missingResults q))
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
             (full && String.startsWith r.query.string q.string) ||
             -- we've deleted characters, but not past where we have complete results
             (full && String.startsWith q.string r.query.string && lastExamined < String.length q.string)
        in
          Moore { o | query <- if ok then Nothing else Just q.string }
                (nonempty (if ok then r' else missingResults q))
      Results r' -> Moore (out r') (nonempty r')

    missingResults q = { query = q, results = [], additionalResults = 0, positionsExamined = [] }
    out r = { matches = List.filter (matches r.query.string) (r.query.values ++ r.results), query = Nothing }
  in
    Moore { matches = [], query = Nothing } empty
