{-# Language OverloadedStrings #-}

module Unison.Test.FileParser where

  import EasyTest
  import Unison.FileParser
  import Unison.Parser
  import Unison.DataDeclaration
  import Unison.EffectDeclaration
  import qualified Unison.Parser as Parser
  import qualified Unison.Parsers as Parsers
  import Unison.Parsers (unsafeGetRight)
  import Data.Map (Map)
  import qualified Data.Map as Map
  import qualified Unison.Reference as R
  import Unison.Symbol (Symbol)

  test = scope "fileparser" . tests . map parses $
    [{-"type Pair a b = Pair a b"
    ,"type Optional a = Just a | Nothing"
    ,-}unlines
      ["type Optional2 a"
      ,"  = Just a"
      ,"  | Nothing\n"]
    -- -- ,unlines
    -- --   ["type Optional a b c where"
    -- --   ,"  Just : a -> Optional a"
    -- --   ,"  Nothing : Optional Int64"]
    -- -- , unlines
    -- --   ["type Optional"
    -- --   ,"   a"
    -- --   ,"   b"
    -- --   ,"   c where"
    -- --   ,"  Just : a -> Optional a"
    -- --   ,"  Nothing : Optional Int64"]
    -- , unlines
    --   ["effect State s where"
    --   ,"  get : {State s} s"
    --   ,"  set : s -> {State s} ()"]
    -- , unlines
    --   ["ping x = pong (x + 1)"
    --   ,"pong x = ping (x - 1)"]
    ]

  builtins = Map.fromList
    [("Pair", (R.Builtin "Pair", 0)),
     ("State.set", (R.Builtin "State", 0))]

  -- parses s = scope s $ do
  --   let p = unsafeParseFile s builtins :: UnisonFile Symbol
  --   noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
  --   ok

  parses s = scope s $ do
    let p = unsafeGetRight $ Unison.Parser.run (Parser.root declarations) s Parsers.s0 builtins
        p' = p :: (Map Symbol (DataDeclaration Symbol), Map Symbol (EffectDeclaration Symbol))
    noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
    ok
