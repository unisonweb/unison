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

-- parse' :: P.Parser (PS.S V) a -> String -> Either String a
-- parse' p input = P.run p input PS.s0

-- parse :: P.Parser (PS.S V) TermV -> String -> Either String TermV
-- parse p input = P.run p input PS.s0

parse :: P.Parser (PS.S V) [(V,TermV)] -> String -> Either String [(V, TermV)]
parse p input = P.run p input PS.s0

main :: IO ()
main = do
  input <- readFile "unison-src/extra.u"
  -- let p = E.term
  let p = E.moduleBindings
  case parse p input of
    Left err -> putStrLn err
    Right a -> putStrLn (show a)
