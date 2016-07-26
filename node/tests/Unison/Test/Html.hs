module Unison.Test.Html where

import Data.Text (pack)
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Runtime.Html as Html

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

numlinks :: Assertion
numlinks = let found = getLinks $ pack testHTML in if 3 == length found
  then pure ()
  else fail $ "expected 3 links, got " ++ show found

tests :: TestTree
tests = testGroup "html"
  [ testCase "numlinks" numlinks
  ]
