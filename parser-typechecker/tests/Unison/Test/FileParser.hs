{-# Language OverloadedStrings #-}

module Unison.Test.FileParser where

  import EasyTest
  import Unison.FileParser
  import qualified Data.Map as Map
  import qualified Unison.Reference as R

  test = scope "fileparser" . tests . map parses $
    ["type Pair a b = Pair a b"
    ,"type Optional a = Just a | Nothing"
    ,unlines
      ["type Optional2 a"
      ,"  = Just a"
      ,"  | Nothing"]
    ,unlines
      ["type Optional a b c"
      ,"  Just : a -> Optional a"
      ,"  Nothing : Optional Int64"]
    , unlines
      ["type Optional"
      ,"   a"
      ,"   b"
      ,"   c"
      ,"  Just : a -> Optional a"
      ,"  Nothing : Optional Int64"]
    , unlines
      ["effect State s foo bar"
      ,"  get : {State s} s"
      ,"  set : s -> {State s} ()"]
    , unlines
      ["ping x = pong (x + 1)"
      ,"pong x = ping (x - 1)"]
    ]

  builtins = Map.fromList
    [("Pair", (R.Builtin "Pair", 0)),
     ("State.set", (R.Builtin "State", 0))]

  parses s = scope s $ do
    let p = unsafeParseFile s builtins :: UnisonFile
    noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
    ok
