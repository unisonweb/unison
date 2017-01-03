{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Html where

import Data.Text (pack)
import EasyTest
import Unison.Note (Noted)
import Unison.Parsers (unsafeParseTerm)
import Unison.Runtime.Html as Html
import Unison.Test.Util
import qualified Data.Vector as Vector
import qualified Unison.Codebase as Codebase
import qualified Unison.Note as Note
import qualified Unison.Paths as P
import qualified Unison.Reference as R
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Term as Term

testHTML = concat
  [ "<html><body>"
  , "<a href=\"t1.html\">simple link</a>"
  , "<a href=\"t2.html\">Inside one <b>Inside other</a> outside one</b>"
  , "<p><ul>"
  , "  <li><A HREF=\"t3\">inside list</A></li>"
  , "  <li><a>Empty link</a></li>"
  , "</ul></p>"
  , "</body></html>"
  ]

testHTML2 = "<html><body><a href='link.html'>description</a></body</html>"

numlinks :: Test ()
numlinks = let found = getLinks $ pack testHTML in if 3 == length found
  then ok
  else crash $ "expected 3 links, got " ++ show found

plainText :: Test ()
plainText = let expected = "simple linkInside one Inside other outside one inside list Empty link"
                result = toPlainText $ pack testHTML
            in if expected == result
               then ok
               else crash $ "got unclean html: " ++ show result

test :: Test ()
test = scope "html" . tests $
  [ scope "numlinks" numlinks
  , scope "plainText" plainText ]
