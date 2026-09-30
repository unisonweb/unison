module Unison.Test.NamespaceNames (test) where

import Data.Text qualified as Text
import EasyTest
import Text.Megaparsec qualified as P
import Unison.Prelude
import Unison.Syntax.HashQualified qualified as HQ
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Syntax.Name qualified as Name

test :: Test ()
test =
  scope "namespace-names" . tests $
    [ scope "reserved-segments" . tests $
        [ scope (Text.unpack raw) do
            let expected = Name.parseText escaped
            expect (isJust expected)
            expectEqual expected (Name.parseText raw)
            expectEqual (HQ.parseText escaped) (HQ.parseText raw)
            expectEqual (HQ'.parseText escaped) (HQ'.parseText raw)
            for_ ["#abc", "#abc#0"] \suffix -> do
              let expectedHQ = HQ.parseText (escaped <> suffix)
              expect (isJust expectedHQ)
              expectEqual expectedHQ (HQ.parseText (raw <> suffix))
              expectEqual (HQ'.parseText (escaped <> suffix)) (HQ'.parseText (raw <> suffix))
            -- The source parser still requires keywords to be escaped.
            expect (isLeft (P.runParser (Name.nameP <* P.eof) "" (Text.unpack raw)))
            expect (isRight (P.runParser (Name.nameP <* P.eof) "" (Text.unpack escaped)))
        | (raw, escaped) <-
            [ ("type", "`type`"),
              ("foo.match", "foo.`match`"),
              (".if.then", ".`if`.`then`"),
              ("a.->", "a.`->`"),
              ("=.else", "`=`.`else`"),
              ("foo.`.~`.type", "foo.`.~`.`type`")
            ]
        ],
      scope "ordinary-and-escaped-names" . tests $
        [ scope (Text.unpack name) do
            parsed <- maybe (crash "expected a name") pure (Name.parseText name)
            expectEqual (Just parsed) (Name.parseText (Name.toText parsed))
        | name <- ["foo", ".foo.bar", "class", "given", "a.`(+)`", "foo.`.~`"]
        ],
      scope "reject-malformed-names" . tests $
        [ scope (Text.unpack name) do
            expectEqual Nothing (Name.parseText name)
            expectEqual Nothing (HQ.parseText name)
            expectEqual Nothing (HQ'.parseText name)
        | name <- ["", "type..foo", "foo.", "`type", "type`", "type match"]
        ]
    ]
