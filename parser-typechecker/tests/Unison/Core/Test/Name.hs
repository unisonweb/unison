{-# Language OverloadedStrings #-}

module Unison.Core.Test.Name where

import           EasyTest
import           Unison.Name                   as Name

test :: Test ()
test = scope "name" $ tests [
  scope "suffixes" $
    tests
      [ scope "empty" $ expectEqual (suffixes "") []
      , scope "one namespace" $ expectEqual (suffixes "bar") ["bar"]
      , scope "two namespaces"
        $ expectEqual (suffixes "foo.bar") ["foo.bar", "bar"]
      , scope "multiple namespaces"
        $ expectEqual (suffixes "foo.bar.baz") ["foo.bar.baz", "bar.baz", "baz"]
      ]
  ]
