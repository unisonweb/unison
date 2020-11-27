{-# LANGUAGE OverloadedStrings #-}

module Unison.Core.Test.Name where

import Data.List (intercalate)
import Data.Text (pack)
import EasyTest
import Unison.Name as Name
import Unison.NameSegment as NameSegment

test :: Test ()
test =
  scope "name" $
    tests
      [ scope "suffixes" $
          tests
            [ scope "empty" $ expectEqual (suffixes "") [],
              scope "one namespace" $ expectEqual (suffixes "bar") ["bar"],
              scope "two namespaces" $
                expectEqual (suffixes "foo.bar") ["foo.bar", "bar"],
              scope "multiple namespaces" $
                expectEqual (suffixes "foo.bar.baz") ["foo.bar.baz", "bar.baz", "baz"],
              scope "terms named `.`" $ expectEqual (suffixes "base..") ["base..", "."]
            ],
        scope "segments" $ do
          numDots <- int' 0 10
          numSegs <- int' 0 10
          n <- int' 0 10
          segs <- listOf n . pick $ replicate numDots "." ++ replicate numSegs "foo"
          expectEqual
            (segments $ Name . pack $ intercalate "." segs)
            (NameSegment . pack <$> segs)
      ]
