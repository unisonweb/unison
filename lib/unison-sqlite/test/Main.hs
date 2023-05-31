module Main (main) where

import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Sqlite.Internal (Param (..), ParsedLump (..), internalParseSql)

main :: IO ()
main =
  withCP65001 (run (scope "sqlite" test))

test :: Test ()
test =
  tests
    [ scope "internalParseSql" . tests $
        [ do
            let sql = "   foo :a\n   'foo''foo' :b\n   \"foo\"\"foo\" $c\n   `foo``foo`   \n[foo] $d  "
            let expected =
                  Right
                    [ ParsedOuterLump "foo ? 'foo''foo' ? \"foo\"\"foo\" " [FieldParam "a", FieldParam "b"],
                      ParsedInnerLump "c",
                      ParsedOuterLump " `foo``foo` [foo] " [],
                      ParsedInnerLump "d"
                    ]
            let actual = internalParseSql sql
            expectEqual expected actual,
          scope "parses @param syntax" do
            let sql = "@foo @bar @"
            let expected = Right [ParsedOuterLump "? ? ?" [RowParam "foo" 1, RowParam "bar" 2]]
            let actual = internalParseSql sql
            expectEqual expected actual,
          scope "parses VALUES :param syntax" do
            let sql = "VALUES :foo"
            let expected = Right [ParsedValuesLump "foo"]
            let actual = internalParseSql sql
            expectEqual expected actual,
          scope "strips line comments" do
            let sql = "foo -- bar \n baz"
            let expected = Right [ParsedOuterLump "foo baz" []]
            let actual = internalParseSql sql
            expectEqual expected actual,
          scope "strips block comments" do
            let sql = "foo /* bar baz \n */ qux"
            let expected = Right [ParsedOuterLump "foo qux" []]
            let actual = internalParseSql sql
            expectEqual expected actual
        ]
    ]
