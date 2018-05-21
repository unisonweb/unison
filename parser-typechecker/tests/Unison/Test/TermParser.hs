module Unison.Test.TermParser where

import EasyTest
import Unison.Test.Common
import Unison.Term (Term)
import Unison.Symbol (Symbol)
import Unison.Parsers (unsafeParseTerm)

test = scope "termparser" . tests . map parses $
  [ "1"
  , "1.0"
  , "+1"
  , "-1"
  , "-1.0"
  , "4th"
  , "forty"
  , "forty two"
  , "\"forty two\""
  , "{ one }"
  , "{ one ; two }"
  , "{ one ; two ; }"
  , "{ one ; two ; three }"
  , "{ one ; two ; 42 }"
  , "{ one ; two ; three; }"
  , "x + 1"
  , "{ x + 1 }"
  , "{ x + 1; }"
  , "{ y = x; 24; }"
  , "{ y = x + 1; 24 }"
  , "{ x = 42 ; y = x + 1 ; 24 }"
  , "{ x = \n" ++
    "   z = 13 \n" ++
    "   z + 1 \n" ++
    "  91.0 \n" ++
    "}"
  , "foo 42"
  ]

parses s = do
  let p = unsafeParseTerm s :: Term Symbol
  noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
  ok
