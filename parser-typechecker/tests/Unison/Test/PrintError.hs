{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.PrintError where

import EasyTest
import Unison.PrintError (ordinal)

test :: Test ()
test =
  scope "ordinal" do
    let expectOrdinal = \ord -> expectEqual @String ord . ordinal
    expectOrdinal "1st" 1
    expectOrdinal "2nd" 2
    expectOrdinal "3rd" 3
    expectOrdinal "4th" 4
    expectOrdinal "5th" 5
    expectOrdinal "10th" 10
    expectOrdinal "11th" 11
    expectOrdinal "12th" 12
    expectOrdinal "13th" 13
    expectOrdinal "14th" 14
    expectOrdinal "21st" 21
    expectOrdinal "22nd" 22
    expectOrdinal "23rd" 23
    expectOrdinal "24th" 24
    expectOrdinal "111th" 111
    expectOrdinal "112th" 112
    expectOrdinal "113th" 113
    expectOrdinal "121st" 121
    expectOrdinal "122nd" 122
    expectOrdinal "123rd" 123
