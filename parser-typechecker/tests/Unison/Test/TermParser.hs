module Unison.Test.TermParser where

import EasyTest
import Unison.Test.Common
import Unison.Term (Term)
import Unison.Symbol (Symbol)
import Unison.Parsers (unsafeParseTerm)

test = scope "termparser" . tests . map parses $
  [ "1"
  , "1.0"
  , "4th"
  , "forty"
  , "forty two"
  , "\"forty two\""
  , "{ one }"
  , "{ one ; two }"
  , "{ one ; two ; }"
  , "{ one ; two ; three }"
  ]

parses s = scope s $ do
  let p = unsafeParseTerm s :: Term Symbol
  note (show p)
  ok
