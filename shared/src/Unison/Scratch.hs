{-# LANGUAGE OverloadedStrings #-}

module Unison.Scratch where

import qualified Unison.Parsers as PS
import qualified Unison.TermParser as E
import qualified Unison.Parser as P
import qualified Unison.View as View
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Text.Parsec.Layout as L

parser = L.spaced (P.attempt $ P.string "let") *> L.laidout (L.spaced (P.attempt $ P.string "a"))

type V = Symbol View.DFO
type TermV = Term V

parse' :: P.Parser (PS.S V) a -> String -> Either String a
parse' p input = P.run p input PS.s0

parse :: P.Parser (PS.S V) TermV -> String -> Either String TermV
parse p input = P.run p input PS.s0

input' = unlines
  [ "let "
  , "  a = 23"
  , "  x : Number"
  , "  x = 2"
  , "  x + 1" ]

input = "let { x = 1 ; x }"
-- input = "let\n  x = 1\n  x"

main :: IO ()
main =
  let p = E.term
      -- p2 = parser
  --in case parse' p2 "let { a; a; a   }" of
  --  Left err -> putStrLn err
  --  Right a -> putStrLn (show a)
  -- in case parse p "let { x=1; x }" of
  in case parse p "let { a = 2 ; 1 }" of
    Left err -> putStrLn err
    Right a -> putStrLn (show a)
