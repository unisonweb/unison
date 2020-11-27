module Unison.Test.Codebase.FileCodebase where

import Data.Char as Char
import Data.Foldable (toList)
import qualified Data.Set as Set
import EasyTest
import Unison.Codebase.FileCodebase.Common (decodeFileName, encodeFileName)

test :: Test ()
test =
  scope "FileCodebase" . tests $
    [ scope "encode/decodeFileName" . tests $
        [ encodeDecode "abc",
          encodeDecode "👍",
          encodeDecode "\xfff",
          tests $ encodeDecode . (: []) <$> ['!' .. '~'],
          encodeDecode ("Universal." ++ ['!' .. '~']),
          specialEncode ".",
          specialEncode "..",
          tests $ map specialEncodeChar (toList specificallyBadChars),
          specialEncodeChar '👍',
          specialEncodeChar '\xfff'
        ]
    ]

specialEncode :: String -> Test ()
specialEncode s =
  scope (" " <> s <> " gets special encoding") $ expect (encodeFileName s /= s)

specialEncodeChar :: Char -> Test ()
specialEncodeChar = specialEncode . pure

encodeDecode :: String -> Test ()
encodeDecode s =
  let e = encodeFileName s
      d = decodeFileName e
   in scope s $ expect $ d == s && all isSafeChar e

-- In the past we had considered a much smaller set of safe chars:
--   [0-9,a-z,A-Z,-._] from https://superuser.com/a/748264
-- Currently we are going by https://superuser.com/a/358861
isSafeChar :: Char -> Bool
isSafeChar c =
  Set.notMember c specificallyBadChars
    && Char.isPrint c
    && Char.isAscii c

specificallyBadChars :: Set.Set Char
specificallyBadChars = Set.fromList "\\/:*?\"<>|"
