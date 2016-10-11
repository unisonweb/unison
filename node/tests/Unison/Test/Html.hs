{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Html where

import Data.Text (pack)
import Test.Tasty
import Test.Tasty.HUnit
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

numlinks :: Assertion
numlinks = let found = getLinks $ pack testHTML in if 3 == length found
  then pure ()
  else fail $ "expected 3 links, got " ++ show found

plainText :: Assertion
plainText = let expected = "simple linkInside one Inside other outside one inside list Empty link"
                result = toPlainText $ pack testHTML
            in if expected == result
               then pure ()
               else fail $ "got unclean html: " ++ show result

tests :: TestTree
tests = testGroup "html"
  [ testCase "numlinks" numlinks
  ]

run :: (TestCodebase, String -> TermV, TermV -> Noted IO TermV) -> Assertion
run (codebase, parse, eval) = do
  let getLinksTerm = parse $ "Html.get-links \"" ++ testHTML2 ++ "\""
      linkTerm = EB.link (Term.text "link.html") (Term.text "description")
      getLink = Term.ref (R.Builtin "Html.get-href") `Term.app` linkTerm
      getDescription = Term.ref (R.Builtin "Html.get-description") `Term.app` linkTerm
      desiredLinks = Term.vector [linkTerm]
      desiredHref = Term.text "link.html"
      desiredDescription = Term.text "description"
      result = traverse eval [getLinksTerm, getLink, getDescription]
  evaluatedResult <- Note.unnote result

  case evaluatedResult of
    Left n -> fail $ "could not evaluate " ++ show n
    Right [links, href, description] ->
      if links == desiredLinks && href == desiredHref && description == desiredDescription
      then pure ()
      else fail $ concat
        [ "links match ", show (links == desiredLinks)
        , "href match ", show (href == desiredHref)
        , "description match ", show (description == desiredDescription)
        ]

tests' :: (TestCodebase, String -> TermV, TermV -> Noted IO TermV) -> TestTree
tests' codebase = testGroup "html"
  [ testCase "numlinks" numlinks
  , testCase "plainText" plainText
  , testCase "run" (run codebase)
  ]

main :: IO ()
main = do
  codebase <- makeTestCodebase
  defaultMain (tests' codebase)
