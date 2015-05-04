module Elmz.Matcher where

import Debug
import Elmz.Moore as Moore
import Elmz.Moore exposing (Moore(..))
import List
import String

type Event a
  = Query { string : String, values : List a }
  | Results { query : String
            , values : List a
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
        in Moore.spike o { o | query <- Nothing } (waiting q)
      _ -> Nothing

    waiting q e = Just <| case e of
      Query q -> Moore { matches = List.filter (matches q.string) q.values, query = Nothing } (waiting q)
      Results r ->
        Moore { matches = List.filter (matches q.string) (q.values ++ r.values), query = Nothing }
        (hasresults r)

    hasresults r e = Just <| case e of
      Results r -> Moore { matches = List.filter (matches r.query) r.values, query = Nothing }
                   (hasresults r)
      Query q ->
        let
          out = List.filter (matches q.string) (q.values ++ r.values)
          -- tricky part is determining whether we need to do another search
          full = r.additionalResults <= 0
          lastExamined = Maybe.withDefault (-1) (List.maximum r.positionsExamined)
          ok =
             -- we already have results for this query
             r.query == q.string ||
              -- we've added characters to a search with all results
             (full && String.startsWith r.query q.string) ||
             -- we've deleted characters, but not past where we have complete results
             (full && String.startsWith q.string r.query && lastExamined < String.length q.string)
        in
          Moore.spike { matches = out, query = if ok then Nothing else Just q.string }
                      { matches = out, query = Nothing }
                      (if ok then hasresults r else waiting q)
  in
    Moore { matches = [], query = Nothing } empty
