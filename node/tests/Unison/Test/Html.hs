{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Html where

import Data.Text (pack)
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Parsers (unsafeParseTerm)
import Unison.Runtime.Html as Html
import Unison.Test.NodeUtil
import qualified Data.Vector as Vector
import qualified Unison.Node as Node
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

tests :: TestTree
tests = testGroup "html"
  [ testCase "numlinks" numlinks
  ]

-- evaluateTerms :: [(Path, e)] -> Noted m [(Path,e,e)],
unisonEvaluate :: (TestNode, String -> TermV) -> Assertion
unisonEvaluate (testNode, parse) = do
  let inputPath = [P.Fn]
      getLinksTerm = parse $ "Html.get-links \"" ++ testHTML2 ++ "\""
      linkTerm = EB.link (Term.text "link.html") (Term.text "description")
      getLink = Term.ref (R.Builtin "Html.get-href") `Term.app` linkTerm
      getDescription = Term.ref (R.Builtin "Html.get-description") `Term.app` linkTerm
      desiredLinks = Term.vector [linkTerm]
      desiredHref = Term.text "link.html"
      desiredDescription = Term.text "description"
      result = Node.evaluateTerms testNode
        [(inputPath, getLinksTerm), (inputPath, getLink), (inputPath, getDescription)]
  evaluatedResult <- Note.unnote result

  case evaluatedResult of
    Left n -> fail $ "could not evaluate " ++ show n
    Right [(_,_,links), (_,_,href), (_,_,description)] ->
      if links == desiredLinks && href == desiredHref && description == desiredDescription
      then pure ()
      else fail $ concat
        [ "links match ", show (links == desiredLinks)
        , "href match ", show (href == desiredHref)
        , "description match ", show (description == desiredDescription)
        ]

nodeTests :: (TestNode, String -> TermV) -> TestTree
nodeTests testNode = testGroup "html"
  [ testCase "numlinks" numlinks
  , testCase "unisonEvaluate" (unisonEvaluate testNode)
  ]

main :: IO ()
main = do
  testNode <- makeTestNode
  defaultMain (nodeTests testNode)
