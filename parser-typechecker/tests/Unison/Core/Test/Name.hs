{-# Language OverloadedStrings #-}

module Unison.Core.Test.Name where

import           EasyTest
import           Unison.Name                   as Name
import           Unison.NameSegment            as NameSegment
import           Unison.Symbol                  ( Symbol )
import qualified Unison.Util.Relation          as R
import           Data.List                      ( intercalate )
import           Data.Text                      ( Text, pack )

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
    expectEqual' (segments $ Name.unsafeFromText . pack $ intercalate "." segs)
                 (NameSegment . pack <$> segs)
    ok
  , scope "suffixSearch" $ do
    let rel = R.fromList [
                (n "base.List.map", 1),
                (n "base.Set.map", 2),
                (n "foo.bar.baz", 3),
                (n "a.b.c", 4),
                (n "a1.b.c", 5),
                (n "..", 6)
                ]
        n = Name.unsafeFromText
    expectEqual' ([n "."]) (Name.convert <$> Name.segments (n ".."))
    expectEqual' ([n "."]) (Name.convert <$> Name.reverseSegments (n ".."))

    expectEqual' (Set.fromList [1,2])
                 (Name.searchBySuffix (n "map") rel)
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
    note . show $ Name.reverseSegments (n ".")
    note . show $ Name.reverseSegments (n "..")
    tests [ scope "(.) shortest unique suffix" $
            expectEqual' (n ".") (Name.shortestUniqueSuffix (n "..") 6 rel)
          , scope "(.) search by suffix" $
            expectEqual' (Set.fromList [6]) (Name.searchBySuffix (n ".") rel) ]
    ok

  , scope "OldName" do
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

      let rsegment :: Test NameSegment
          rsegment =
            pick
              [ NameSegment "foo"
              , NameSegment "bar"
              , NameSegment "|>"
              , NameSegment "."
              ]

      let rstring :: Test String
          rstring =
            Name.toString <$> rname

      let rtext :: Test Text
          rtext =
            Name.toText <$> rname

      scope "compareSuffix" do
        n1 <- rname
        n2 <- rname
        Name.compareSuffix n1 n2 `expectEqual` Name.oldCompareSuffix (old n1) (old n2)

      scope "countSegments" do
        n1 <- rname
        Name.countSegments n1 `expectEqual` Name.oldCountSegments (old n1)

      scope "endsWithSegments" do
        n1 <- rname
        n2 <- rname
        Name.endsWithSegments n1 n2 `expectEqual` Name.oldEndsWithSegments (old n1) (old n2)

      scope "fromSegment" do
        s1 <- rsegment
        old (Name.fromSegment s1) `expectEqual` Name.oldFromSegment s1

      scope "fromString" do
        s1 <- rstring
        Name.fromString s1 `expectEqual` Name.oldFromString s1

      scope "isPrefixOf" do
        n1 <- rname
        n2 <- rname
        Name.isPrefixOf n1 n2 `expectEqual` Name.oldIsPrefixOf (old n1) (old n2)

      scope "joinDot" do
        n1 <- rname
        n2 <- rname
        old (Name.joinDot n1 n2) `expectEqual` Name.oldJoinDot (old n1) (old n2)

      scope "makeAbsolute" do
        n1 <- rname
        old (Name.makeAbsolute n1) `expectEqual` Name.oldMakeAbsolute (old n1)

      scope "parent" do
        n1 <- rname
        fmap old (Name.parent n1) `expectEqual` Name.oldParent (old n1)

      scope "reverseSegments" do
        n1 <- rname
        Name.reverseSegments n1 `expectEqual` Name.oldReverseSegments (old n1)

      scope "searchBySuffix" do
        ok

      scope "segments" do
        n1 <- rname
        Name.segments n1 `expectEqual` Name.oldSegments (old n1)

      scope "shortestUniqueSuffix" do
        ok

      scope "sortNames" do
        ns1 <- listOf 5 rname
        fmap old (Name.sortNames ns1) `expectEqual` Name.oldSortNames (map old ns1)

      scope "sortNamed'" do
        ns1 <- listOf 5 rname
        map old (Name.sortNamed' id compare ns1) `expectEqual` Name.oldSortNamed' id compare (map old ns1)

      scope "stripNamePrefix" do
        n1 <- rname
        n2 <- rname
        fmap old (Name.stripNamePrefix n1 n2) `expectEqual` Name.oldStripNamePrefix (old n1) (old n2)

      scope "stripPrefixes" do
        n1 <- rname
        old (Name.stripPrefixes n1) `expectEqual` Name.oldStripPrefixes (old n1)

      scope "suffixes" do
        n1 <- rname
        map old (Name.suffixes n1) `expectEqual` Name.oldSuffixes (old n1)

      scope "suffixFrom" do
        n1 <- rname
        n2 <- rname
        fmap old (Name.suffixFrom n1 n2) `expectEqual` Name.oldSuffixFrom (old n1) (old n2)

      scope "toString" do
        n1 <- rname
        Name.toString n1 `expectEqual` Name.oldToString (old n1)

      scope "toVar" do
        n1 <- rname
        Name.toVar @Symbol n1 `expectEqual` Name.oldToVar (old n1)

      scope "unqualified" do
        n1 <- rname
        old (Name.unqualified n1) `expectEqual` Name.oldUnqualified (old n1)

      scope "unsafeFromString" do
        s1 <- rstring
        old (Name.unsafeFromString s1) `expectEqual` Name.oldUnsafeFromString s1

      scope "unsafeFromText" do
        t1 <- rtext
        old (Name.unsafeFromText t1) `expectEqual` Name.oldUnsafeFromText t1
  ]
