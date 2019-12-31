{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Unison.Test.Codebase.Path where

import EasyTest
import Unison.Codebase.Path
import Data.Sequence
import Data.Text
import Data.Either
import qualified Unison.Codebase.NameSegment as NameSegment
import qualified Unison.HashQualified' as HQ'
import qualified Unison.ShortHash as SH

test :: Test ()
test = scope "path" . tests $
  [ scope "parsePath'Impl" . tests $
    [ let s = "foo.bar.baz.34"  in scope s . expect $ parsePath'Impl s == Right (relative ["foo","bar","baz"], "34")
    , let s = "foo.bar.baz" in scope s . expect $ parsePath'Impl s == Right (relative ["foo", "bar"], "baz")
    , let s = "baz" in scope s . expect $ parsePath'Impl s == Right (relative [], "baz")
    , let s = "34" in scope s . pending . expect $ parsePath'Impl s == Right (relative [], "34")

--    , let s = "foo.bar.baz#a8fj" in scope s . expect $ parsePath'Impl s == Right (relative ["foo", "bar"], "baz#a8fj")
    , let s = "foo.bar.baz#a8fj" in scope s . expect $ isLeft $ parsePath'Impl s
    ]
  , scope "parseSplit'" . tests $
    [ scope "wordyNameSegment" . tests $
      [ let s = "foo.bar.baz" in scope s . expect $
        parseSplit' wordyNameSegment s == Right (relative ["foo", "bar"], NameSegment.unsafeFromText "baz")

      , let s = "foo.bar.baz#abc" in scope s . expect $ isLeft $ parseSplit' wordyNameSegment s

      , let s = "foo.bar.+" in scope s . expect $
        isLeft $ parseSplit' wordyNameSegment s
      ]

    , scope "definitionNameSegment" . tests $
      [ let s = "foo.bar.+" in scope s . expect $
        parseSplit' definitionNameSegment s == Right (relative ["foo", "bar"], NameSegment.unsafeFromText "+")
      ]
    ]
  , scope "parseShortHashOrHQSplit'" . tests $
    [ let s = "foo.bar#34" in scope s . expect $
      parseShortHashOrHQSplit' s ==
        (Right . Right)
          (relative ["foo"], HQ'.HashQualified (NameSegment.unsafeFromText "bar") (SH.unsafeFromText "#34"))

    , let s = "foo.bar.+" in scope s . expect $
      parseShortHashOrHQSplit' s ==
        (Right . Right)
          (relative ["foo", "bar"], HQ'.NameOnly (NameSegment.unsafeFromText "+"))

    , let s = "#123" in scope s . expect $
      parseShortHashOrHQSplit' s ==
        (Right . Left) (SH.unsafeFromText "#123")
    ]
  , scope "parseHQ'Split'" . tests $
    [ let s = "foo.bar#34" in scope s . expect $
      parseHQSplit' s == Right (relative ["foo"], HQ'.HashQualified (NameSegment.unsafeFromText "bar") (SH.unsafeFromText "#34"))
    , let s = "foo.bar.+" in scope s . expect $
      parseHQSplit' s == Right (relative ["foo", "bar"], HQ'.NameOnly (NameSegment.unsafeFromText "+"))
    , let s = "#123" in scope s . expect $ isLeft $ parseHQSplit' s
    ]
  ]


relative :: Seq Text -> Path'
relative = Path' . Right . Relative . Path . fmap NameSegment.unsafeFromText
