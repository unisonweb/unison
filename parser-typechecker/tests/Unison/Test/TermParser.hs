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
  , "( one ; two )"
  , "( one ; two )"
  , "( one ; two ; three )"
  , "( one ; two ; 42 )"
  , "x + 1"
  , "( x + 1 )"
  , "foo 42"
  , "let x = 1\n" ++
    "    x"
  , "let\n" ++
    " y = 1\n" ++
    " x"
  , "let y = 1  \n" ++
    "    x = 2  \n" ++
    "    x + y"
  , "(let \n" ++
    "  x = 23 + 42\n" ++
    "  x + 1 \n)"
  ,"handle foo in \n" ++
    "  x = 23 + 42\n" ++
    "  x + foo 8 102.0 +4"
  ,"handle foo in \n" ++
    "  x = 1\n" ++
    "  x"
  , "handle foo in x"
  ]

parses s = scope s $ do
  let p = unsafeParseTerm s :: Term Symbol
  noteScoped $ "parsing: " ++ s ++ "\n  " ++ show p
  ok
