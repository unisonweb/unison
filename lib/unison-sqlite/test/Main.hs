module Main (main) where

import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Sqlite.Internal (internalParseSql)

main :: IO ()
main =
  withCP65001 (run (scope "sqlite" test))

test :: Test ()
test =
  tests
    [ scope "internalParseSql" do
        let sql = "   foo :a\n   'foo''foo' :b\n   \"foo\"\"foo\" :c\n   `foo``foo`   \n[foo] :d  "
        let expected = Right ("foo :a 'foo''foo' :b \"foo\"\"foo\" :c `foo``foo` [foo] :d", ["a", "b", "c", "d"])
        let actual = internalParseSql sql
        expectEqual expected actual
    ]
