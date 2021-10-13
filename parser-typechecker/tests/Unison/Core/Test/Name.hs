{-# LANGUAGE OverloadedStrings #-}

module Unison.Core.Test.Name where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Set as Set
import Data.Text (Text)
import EasyTest
import Unison.Name as Name
import Unison.OldName as Name
import Unison.Symbol (Symbol)
import qualified Unison.Util.Relation as R

test :: Test ()
test =
  scope "name" $
    tests
      [ scope "compareSuffix" (tests testCompareSuffix),
        scope "segments" (tests testSegments),
        scope "splitName" (tests testSplitName),
        scope "suffixSearch" (tests testSuffixSearch),
        scope "suffixes" (tests testSuffixes),
        scope "unsafeFromString" (tests testUnsafeFromString),
        scope "temporary regression tests" do
          let old :: Name -> OldName
              old =
                Name.toText

          let rname :: Test Name
              rname =
                pick
                  [ Name.unsafeFromText "foo",
                    Name.unsafeFromText ".foo",
                    Name.unsafeFromText "bar",
                    Name.unsafeFromText ".bar",
                    Name.unsafeFromText "foo.bar",
                    Name.unsafeFromText ".foo.bar",
                    Name.unsafeFromText "|>",
                    Name.unsafeFromText ".|>",
                    Name.unsafeFromText "|>.foo",
                    Name.unsafeFromText ".|>.foo",
                    Name.unsafeFromText ".."
                  ]

          let rstring :: Test String
              rstring =
                Name.toString <$> rname

          let rtext :: Test Text
              rtext =
                Name.toText <$> rname

          tests
            [ scope "compareSuffix" do
                n1 <- rname
                n2 <- rname
                Name.compareSuffix n1 n2 `expectEqual` Name.oldCompareSuffix (old n1) (old n2),
              scope "countSegments" do
                n1 <- rname
                Name.countSegments n1 `expectEqual` Name.oldCountSegments (old n1),
              scope "endsWithSegments" do
                n1 <- rname
                n2 <- rname
                Name.endsWithSegments n1 n2 `expectEqual` Name.oldEndsWithSegments (old n1) (old n2),
              scope "isPrefixOf" do
                n1 <- rname
                n2 <- rname
                Name.isPrefixOf n1 n2 `expectEqual` Name.oldIsPrefixOf (old n1) (old n2),
              scope "joinDot" do
                n1 <- rname
                n2 <- rname
                if Name.isAbsolute n2
                  then skip
                  else old (Name.joinDot n1 n2) `expectEqual` Name.oldJoinDot (old n1) (old n2),
              scope "makeAbsolute" do
                n1 <- rname
                old (Name.makeAbsolute n1) `expectEqual` Name.oldMakeAbsolute (old n1),
              scope "parent" do
                n1 <- rname
                if Name.isAbsolute n1
                  then skip
                  else fmap old (Name.parent n1) `expectEqual` Name.oldParent (old n1),
              scope "reverseSegments" do
                n1 <- rname
                List.NonEmpty.toList (Name.reverseSegments n1) `expectEqual` Name.oldReverseSegments (old n1),
              scope "searchBySuffix" do
                ok,
              scope "segments" do
                n1 <- rname
                List.NonEmpty.toList (Name.segments n1) `expectEqual` Name.oldSegments (old n1),
              scope "shortestUniqueSuffix" do
                ok,
              scope "sortNames" do
                ns1 <- listOf 5 rname
                fmap old (Name.sortNames ns1) `expectEqual` Name.oldSortNames (map old ns1),
              scope "stripNamePrefix" do
                n1 <- rname
                n2 <- rname
                fmap old (Name.stripNamePrefix n1 n2) `expectEqual` Name.oldStripNamePrefix (old n1) (old n2),
              scope "suffixes" do
                n1 <- rname
                map old (Name.suffixes n1) `expectEqual` Name.oldSuffixes (old n1),
              scope "suffixFrom" do
                n1 <- rname
                n2 <- rname
                note' n1
                note' n2
                fmap old (Name.suffixFrom n1 n2) `expectEqual` Name.oldSuffixFrom (old n1) (old n2),
              scope "toString" do
                n1 <- rname
                Name.toString n1 `expectEqual` Name.oldToString (old n1),
              scope "toVar" do
                n1 <- rname
                Name.toVar @Symbol n1 `expectEqual` Name.oldToVar (old n1),
              scope "unqualified" do
                n1 <- rname
                old (Name.unqualified n1) `expectEqual` Name.oldUnqualified (old n1),
              scope "unsafeFromString" do
                s1 <- rstring
                old (Name.unsafeFromString s1) `expectEqual` Name.oldUnsafeFromString s1,
              scope "unsafeFromText" do
                t1 <- rtext
                old (Name.unsafeFromText t1) `expectEqual` Name.oldUnsafeFromText t1
            ]
      ]

testCompareSuffix :: [Test ()]
testCompareSuffix =
  [ scope "[b.c a.b.c]" (expectEqual (compareSuffix "b.c" "a.b.c") EQ),
    scope "[a.b.c a.b.c]" (expectEqual (compareSuffix "a.b.c" "a.b.c") EQ),
    scope "[b.c a.b.b]" (expectEqual (compareSuffix "b.c" "a.b.b") LT),
    scope "[a.b.c b.c]" (expectEqual (compareSuffix "a.b.c" "b.c") LT),
    scope "[b.b a.b.c]" (expectEqual (compareSuffix "b.b" "a.b.c") GT)
  ]

testSegments :: [Test ()]
testSegments =
  [ do
      n <- int' 1 10
      segs <- List.NonEmpty.fromList <$> listOf n (pick [".", "foo"])
      expectEqual (segments (relativeFromSegments segs)) segs
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
      let rel =
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
