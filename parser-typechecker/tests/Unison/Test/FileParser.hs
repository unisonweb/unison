{-# Language BangPatterns, OverloadedStrings #-}

module Unison.Test.FileParser where

  import EasyTest
  import Control.Applicative
  import qualified Unison.Builtin as Builtin
  import Unison.FileParser (file)
  import Unison.Parser
  import qualified Unison.Parser as Parser
  import Unison.Parsers (unsafeGetRightFrom, unsafeReadAndParseFile')
  import qualified Data.Map as Map
  import qualified Unison.Reference as R
  import Unison.Symbol (Symbol)
  import Unison.UnisonFile (UnisonFile)

  test1 = scope "fileparser.test1" . tests . map parses $
    [ "()"
    -- , "type () = ()\n()"
    , "type Pair a b = Pair a b\n()"
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

  test2 = scope "fileparser.test2" $
    (io $ unsafeReadAndParseFile' "unison-src/test1.u") *> ok

  test = --test2
    test1 <|> test2

  builtins = Map.fromList
    [("Pair", (R.Builtin "Pair", 0)),
     ("State.set", (R.Builtin "State", 0))]

  parses s = scope s $ do
    let
      p :: UnisonFile Symbol Ann
      !p = unsafeGetRightFrom s $
             Unison.Parser.run
               (Parser.rootFile $
                 file Builtin.builtinTerms Builtin.builtinTypes)
                 s
                 builtins
    pure p >> ok
