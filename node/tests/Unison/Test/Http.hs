{-# Language OverloadedStrings #-}
module Unison.Test.Http where

import Data.Text (isPrefixOf, unpack)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Unison.Runtime.Http as Http

testGet :: Assertion
testGet = do
  let testUrl = "http://www.haskell.org/"
  result <- Http.get testUrl
  case result of
    Right x -> if isPrefixOf "<!DOCTYPE HTML>" x
      then pure ()
      else fail $ "Got unexpected body " ++ unpack x
    Left x -> fail $ show x

testGetTLS :: Assertion
testGetTLS = do
  let testUrl = "https://www.google.com"
  result <- Http.get testUrl
  case result of
    Right x -> if isPrefixOf "<!doctype html>" x
      then pure ()
      else fail $ "Got unexpected body " ++ unpack x
    Left x -> fail $ show x

test404 :: Assertion
test404 = do
  let testUrl = "https://www.google.com/ireallyhopethisurlwillneverexist"
  result <- Http.get testUrl
  case result of
    Left x -> pure ()
    Right x -> fail $ "expected error code, got response body " ++ unpack x

tests :: TestTree
tests = testGroup "HTTP"
  [ testCase "testGet" testGet
  , testCase "testGetTLS" testGetTLS
  , testCase "test404" test404
  ]
