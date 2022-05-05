{-# LANGUAGE OverloadedStrings #-}

module Unison.Core.Test.Name where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Set as Set
import EasyTest
import Unison.Name as Name
import qualified Unison.Util.Relation as R

test :: Test ()
test =
  scope "name" $
    tests
      [ scope "compareSuffix" (tests testCompareSuffix),
        scope "endsWithReverseSegments" (tests testEndsWithReverseSegments),
        scope "endsWithSegments" (tests testEndsWithSegments),
        scope "segments" (tests testSegments),
        scope "splitName" (tests testSplitName),
        scope "suffixSearch" (tests testSuffixSearch),
        scope "suffixes" (tests testSuffixes),
        scope "unsafeFromString" (tests testUnsafeFromString)
      ]

testCompareSuffix :: [Test ()]
testCompareSuffix =
  [ scope "[b.c a.b.c]" (expectEqual (compareSuffix "b.c" "a.b.c") EQ),
    scope "[a.b.c a.b.c]" (expectEqual (compareSuffix "a.b.c" "a.b.c") EQ),
    scope "[b.c a.b.b]" (expectEqual (compareSuffix "b.c" "a.b.b") LT),
    scope "[a.b.c b.c]" (expectEqual (compareSuffix "a.b.c" "b.c") LT),
    scope "[b.b a.b.c]" (expectEqual (compareSuffix "b.b" "a.b.c") GT)
  ]

testEndsWithReverseSegments :: [Test ()]
testEndsWithReverseSegments =
  [ scope "a.b.c ends with []" (expectEqual True (endsWithReverseSegments "a.b.c" [])),
    scope "a.b.c ends with [c, b]" (expectEqual True (endsWithReverseSegments "a.b.c" ["c", "b"])),
    scope "a.b.c doesn't end with [d]" (expectEqual False (endsWithReverseSegments "a.b.c" ["d"]))
  ]

testEndsWithSegments :: [Test ()]
testEndsWithSegments =
  [ scope "a.b.c ends with []" (expectEqual True (endsWithSegments "a.b.c" [])),
    scope "a.b.c ends with [b, c]" (expectEqual True (endsWithSegments "a.b.c" ["b", "c"])),
    scope "a.b.c doesn't end with [d]" (expectEqual False (endsWithSegments "a.b.c" ["d"]))
  ]

testSegments :: [Test ()]
testSegments =
  [ do
      n <- int' 1 10
      segs <- List.NonEmpty.fromList <$> listOf n (pick [".", "foo"])
      expectEqual (segments (fromSegments segs)) segs
  ]

testSplitName :: [Test ()]
testSplitName =
  [ scope "x" (expectEqual (splits "x") [([], "x")]),
    scope "A.x" (expectEqual (splits "A.x") [([], "A.x"), (["A"], "x")]),
    scope "A.B.x" (expectEqual (splits "A.B.x") [([], "A.B.x"), (["A"], "B.x"), (["A", "B"], "x")])
  ]

testSuffixes :: [Test ()]
testSuffixes =
  [ scope "one namespace" $ expectEqual (suffixes "bar") ["bar"],
    scope "two namespaces" $
      expectEqual (suffixes "foo.bar") ["foo.bar", "bar"],
    scope "multiple namespaces" $
      expectEqual (suffixes "foo.bar.baz") ["foo.bar.baz", "bar.baz", "baz"],
    scope "terms named `.`" $ expectEqual (suffixes "base..") ["base..", "."]
  ]

testSuffixSearch :: [Test ()]
testSuffixSearch =
  [ do
      let rel :: R.Relation Name Int
          rel =
            R.fromList
              [ (n "base.List.map", 1),
                (n "base.Set.map", 2),
                (n "foo.bar.baz", 3),
                (n "a.b.c", 4),
                (n "a1.b.c", 5),
                (n "..", 6)
              ]
          n = Name.unsafeFromText
      expectEqual' ("." :| []) (Name.segments (n ".."))
      expectEqual' ("." :| []) (Name.reverseSegments (n ".."))

      expectEqual'
        (Set.fromList [1, 2])
        (Name.searchBySuffix (n "map") rel)
      expectEqual'
        (n "List.map")
        (Name.shortestUniqueSuffix (n "base.List.map") 1 rel)
      expectEqual'
        (n "Set.map")
        (Name.shortestUniqueSuffix (n "base.Set.map") 2 rel)
      expectEqual'
        (n "baz")
        (Name.shortestUniqueSuffix (n "foo.bar.baz") 3 rel)
      expectEqual'
        (n "a.b.c")
        (Name.shortestUniqueSuffix (n "a.b.c") 3 rel)
      expectEqual'
        (n "a1.b.c")
        (Name.shortestUniqueSuffix (n "a1.b.c") 3 rel)
      note . show $ Name.reverseSegments (n ".")
      note . show $ Name.reverseSegments (n "..")
      tests
        [ scope "(.) shortest unique suffix" $
            expectEqual' (n ".") (Name.shortestUniqueSuffix (n "..") 6 rel),
          scope "(.) search by suffix" $
            expectEqual' (Set.fromList [6]) (Name.searchBySuffix (n ".") rel)
        ]
      ok
  ]

testUnsafeFromString :: [Test ()]
testUnsafeFromString =
  [ scope "." do
      expectEqual' (isAbsolute ".") False
      expectEqual' (segments ".") ("." :| [])
      ok,
    scope ".." do
      expectEqual' (isAbsolute "..") True
      expectEqual' (segments "..") ("." :| [])
      ok,
    scope "foo.bar" do
      expectEqual' (isAbsolute "foo.bar") False
      expectEqual' (segments "foo.bar") ("foo" :| ["bar"])
      ok,
    scope ".foo.bar" do
      expectEqual' (isAbsolute ".foo.bar") True
      expectEqual' (segments ".foo.bar") ("foo" :| ["bar"])
      ok,
    scope "foo.." do
      expectEqual' (isAbsolute "foo..") False
      expectEqual' (segments "foo..") ("foo" :| ["."])
      ok
  ]
