{-# Language OverloadedStrings #-}
module Unison.Test.Http where

import EasyTest
import Data.Text (isPrefixOf, unpack)
import qualified Unison.Runtime.Http as Http

testGet :: Test ()
testGet = do
  let testUrl = "http://www.haskell.org/"
  result <- io (Http.get testUrl)
  result <- expectRight result
  expect ("<!DOCTYPE HTML>" `isPrefixOf` result)

testGetTLS :: Test ()
testGetTLS = do
  let testUrl = "https://www.google.com"
  result <- io (Http.get testUrl)
  result <- expectRight result
  expect ("<!doctype html>" `isPrefixOf` result)

test404 :: Test ()
test404 = do
  let testUrl = "https://www.google.com/ireallyhopethisurlwillneverexist"
  result <- io (Http.get testUrl)
  case result of
    Left x -> ok
    Right x -> crash $ "expected error code, got response body " ++ unpack x

test :: Test ()
test = scope "Http" . tests $
  [ scope "get" testGet
  , scope "getTLS" testGetTLS
  , scope "404" test404
  ]
