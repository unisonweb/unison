{-# Language OverloadedStrings #-}

module Unison.Test.FileParser where

  import EasyTest
  import Control.Applicative
  import Unison.FileParser (file)
  import Unison.Parser
  import qualified Unison.Parser as Parser
  import qualified Unison.Parsers as Parsers
  import Unison.Parsers (unsafeGetRight, unsafeReadAndParseFile')
  import qualified Data.Map as Map
  import qualified Unison.Reference as R
  import Unison.Symbol (Symbol)
  import Unison.UnisonFile (UnisonFile)

  test1 = scope "fileparser.test1" . tests . map parses $
    [
      "type Pair a b = Pair a b\n()"
    , "type Optional a = Just a | Nothing\n()"
    , unlines
      ["type Optional2 a"
      ,"  = Just a"
      ,"  | Nothing\n()"]
    ------ -- ,unlines
    ------ --   ["type Optional a b c where"
    ------ --   ,"  Just : a -> Optional a"
    ------ --   ,"  Nothing : Optional Int64"]
    ------ -- , unlines
    ------ --   ["type Optional"
    ------ --   ,"   a"
    ------ --   ,"   b"
    ------ --   ,"   c where"
    ------ --   ,"  Just : a -> Optional a"
    ------ --   ,"  Nothing : Optional Int64"]
    , unlines -- NB: this currently fails because we don't have type AST or parser for effect types yet
      ["effect State s where"
      ,"  get : {State s} s"
      ,"  set : s -> {State s} ()"
      ,"()"]
    , unlines
      ["ping x = pong (x + 1)"
      ,"pong x = ping (x - 1)"
      ,"ping"]
    ]

  test2 = scope "fileparser.test2" $ do
    file <- io $ unsafeReadAndParseFile' "unison-src/test1.u"
    io $ putStrLn (show (file :: UnisonFile Symbol))
    ok

  test = --test2
    test1 <|> test2

  builtins = Map.fromList
    [("Pair", (R.Builtin "Pair", 0)),
     ("State.set", (R.Builtin "State", 0))]

  parses s = scope s $ do
    let p = unsafeGetRight $ Unison.Parser.run (Parser.root $ file []) s Parsers.s0 builtins
        _ = p :: UnisonFile Symbol
    noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
    ok
