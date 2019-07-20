module Unison.Test.Codebase.FileCodebase where

import EasyTest
import Unison.Codebase.FileCodebase
import qualified Data.Set as Set

test :: Test ()
test = scope "FileCodebase" . tests $
  [ scope "encode/decodeFileName" . tests $
    [ encodeDecode "abc"
    , encodeDecode "👍"
    , encodeDecode "\xfff"
    , encodeDecode "."
    , encodeDecode "D@G#&DKUH"
    ]
  ]

encodeDecode s =
  let e = encodeFileName s
      d = decodeFileName e
  in expect $ d == s && all (Set.member safeChars) e

safeChars = Set.fromList $
  ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-._$"
