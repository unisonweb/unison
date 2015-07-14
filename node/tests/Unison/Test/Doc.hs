module Unison.Test.Doc where

import Unison.Doc
import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Doc"
  [ testCase "doc1" $ assertEqual "ex1"
      0 -- todo
      0
  ]

main = defaultMain tests
