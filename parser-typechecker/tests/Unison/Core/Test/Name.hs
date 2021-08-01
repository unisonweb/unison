{-# Language OverloadedStrings #-}

module Unison.Core.Test.Name where

import           EasyTest
import           Unison.Name                   as Name
import           Unison.NameSegment            as NameSegment
import qualified Unison.Util.Relation          as R
import           Data.List                      ( intercalate )
import           Data.Text                      ( pack )

import qualified Data.Set                      as Set

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
    expectEqual' (segments $ Name . pack $ intercalate "." segs)
                 (NameSegment . pack <$> segs)
    ok
  , scope "suffixSearch" $ do
    let rel = R.fromList [
                (n "base.List.map", 1),
                (n "base.Set.map", 2),
                (n "foo.bar.baz", 3),
                (n "a.b.c", 4),
                (n "a1.b.c", 5)
                ]
        n = Name.unsafeFromText
    expectEqual' (Set.fromList [1,2])
                 (R.searchDom (Name.compareSuffix (n "map")) rel)
    expectEqual' (n "List.map")
                 (Name.shortestUniqueSuffix (n "base.List.map") 1 rel)
    expectEqual' (n "Set.map")
                 (Name.shortestUniqueSuffix (n "base.Set.map") 2 rel)
    expectEqual' (n "baz")
                 (Name.shortestUniqueSuffix (n "foo.bar.baz") 3 rel)
    expectEqual' (n "a.b.c")
                 (Name.shortestUniqueSuffix (n "a.b.c") 3 rel)
    expectEqual' (n "a1.b.c")
                 (Name.shortestUniqueSuffix (n "a1.b.c") 3 rel)
    ok
  ]
