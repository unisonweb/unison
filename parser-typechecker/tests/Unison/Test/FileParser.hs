{-# Language BangPatterns, OverloadedStrings #-}

module Unison.Test.FileParser where

  import EasyTest
  import Data.List (uncons)
  import Data.Set (elems)
  import qualified Text.Megaparsec.Error as MPE
  import Unison.FileParser (file)
  import qualified Unison.Parser as P
  import Unison.Parsers (unsafeGetRightFrom, unsafeReadAndParseFile')
  import qualified Unison.Reference as R
  import qualified Unison.Referent as Referent
  import Unison.Symbol (Symbol)
  import Unison.UnisonFile (UnisonFile)
  import qualified Unison.Names as Names
  import Unison.Names (Names)
  import Unison.Var (Var)

  test1 :: Test ()
  test1 = scope "fileparser.test1" . tests . map parses $
    [
    -- , "type () = ()\n()"
      "type Pair a b = Pair a b\n"
    , "type Optional a = Just a | Nothing\n"
    , unlines
      ["type Optional2 a"
      ,"  = Just a"
      ,"  | Nothing\n"]
    ------ -- ,unlines
    ------ --   ["type Optional a b c where"
    ------ --   ,"  Just : a -> Optional a"
    ------ --   ,"  Nothing : Optional Int"]
    ------ -- , unlines
    ------ --   ["type Optional"
    ------ --   ,"   a"
    ------ --   ,"   b"
    ------ --   ,"   c where"
    ------ --   ,"  Just : a -> Optional a"
    ------ --   ,"  Nothing : Optional Int"]
    , unlines -- NB: this currently fails because we don't have type AST or parser for effect types yet
      ["effect State s where"
      ,"  get : {State s} s"
      ,"  set : s -> {State s} ()"
      ]
    , unlines
      ["ping x = pong (x + 1)"
      ,"pong x = ping (x - 1)"
      ]
    ]

  test2 :: Test ()
  test2 = scope "fileparser.test2" $
    (io $ unsafeReadAndParseFile' "unison-src/test1.u") *> ok

  test :: Test ()
  test = test1

  expectFileParseFailure :: Var e => String -> (P.Error e -> Test ()) -> Test ()
  expectFileParseFailure s expectation = scope s $ do
    let result = P.run (P.rootFile file) s builtins
    case result of
      Right _ -> crash "Parser succeeded"
      Left (MPE.FancyError _ sets) ->
        case (fmap (fst) . uncons . elems) sets of
          Just (MPE.ErrorCustom e) -> expectation e
          Just _ -> crash "Error encountered was not custom"
          Nothing -> crash "No error found"
      Left _ -> crash "Parser failed with an error which was not fancy"

  builtins :: Names
  builtins = Names.fromTerms
    [ ("Pair"     , Referent.Con (R.Builtin "Pair") 0)
    , ("State.set", Referent.Con (R.Builtin "State") 0)
    ]

  parses :: String -> Test ()
  parses s = scope s $ do
    let
      p :: UnisonFile Symbol P.Ann
      !p = snd . unsafeGetRightFrom s $
             P.run (P.rootFile file) s builtins
    pure p >> ok
