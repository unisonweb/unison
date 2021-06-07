{-# Language OverloadedStrings #-}

module Unison.Core.Test.Name where

import           EasyTest
import           Unison.Name                   as Name
import           Unison.NameSegment            as NameSegment
import           Data.List                      ( intercalate )
import           Data.Text                      ( pack )

test :: Test ()
test = scope "name" $ tests
  [ scope "suffixes" $ tests
    [ scope "empty" $ expectEqual (suffixes "") []
    , scope "one namespace" $ expectEqual (suffixes "bar") ["bar"]
    , scope "two namespaces"
      $ expectEqual (suffixes "foo.bar") ["foo.bar", "bar"]
    , scope "multiple namespaces"
      $ expectEqual (suffixes "foo.bar.baz") ["foo.bar.baz", "bar.baz", "baz"]
    , scope "terms named `.`" $ expectEqual (suffixes "base..") ["base..", "."]
    ]
  , scope "segments" $ do
    n    <- int' 0 10
    segs <- listOf n $ pick [".", "foo"]
    expectEqual (segments $ Name . pack $ intercalate "." segs)
                (NameSegment . pack <$> segs)
  ]
