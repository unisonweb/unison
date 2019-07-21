module Unison.Test.Codebase.FileCodebase where

import EasyTest
import Unison.Codebase.FileCodebase
import qualified Data.Set as Set
import qualified Unison.Lexer as L
import Data.Foldable (toList)

test :: Test ()
test = scope "FileCodebase" . tests $
  [ scope "encode/decodeFileName" . tests $
    [ encodeDecode "abc"
    , encodeDecode "👍"
    , encodeDecode "\xfff"
    , pending $ encodeDecode ['!'..'~']
    , pending $ specialEncode "."
    , pending $ specialEncode ".."
    , pending $
        tests $ map specialEncodeChar (toList $ Set.delete '.' L.symbolyIdChars)
    , pending $ tests $ map specialEncodeChar unsafeChars
    ]
  ]

specialEncode :: String -> Test ()
specialEncode s =
  scope (s <> " gets special encoding") $ expect (encodeFileName s /= s)

specialEncodeChar :: Char -> Test ()
specialEncodeChar = specialEncode . pure

encodeDecode :: String -> Test ()
encodeDecode s =
  let e = encodeFileName s
      d = decodeFileName e
  in scope s $ expect $ d == s && all (`Set.member` safeChars) e

safeChars :: Set.Set Char
safeChars = Set.fromList $
  ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-._$"

unsafeChars :: [Char]
unsafeChars = toList $ (Set.fromList ['!'..'~'] `Set.difference` safeChars)