{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Unison.Test.Codebase.Path where

import EasyTest
import Unison.Codebase.Path
import Unison.Codebase.Path.Parse
import Data.Sequence
import Data.Text
import Unison.NameSegment
import Data.Either
import qualified Unison.HashQualified' as HQ'
import qualified Unison.ShortHash as SH

test :: Test ()
test = scope "path" . tests $
  [ scope "parsePathImpl'" . tests $
    [ let s = "foo.bar.baz.34"  in scope s . expect $ parsePathImpl' s == Right (relative ["foo","bar","baz"], "34")
    , let s = "foo.bar.baz" in scope s . expect $ parsePathImpl' s == Right (relative ["foo", "bar"], "baz")
    , let s = "baz" in scope s . expect $ parsePathImpl' s == Right (relative [], "baz")
    , let s = "-" in scope s . expect $ parsePathImpl' s == Right (relative [], "-")
    , let s = "34" in scope s . pending . expect $ parsePathImpl' s == Right (relative [], "34")
    , let s = "foo.bar.baz#a8fj" in scope s . expect $ isLeft $ parsePathImpl' s
    ]
  , scope "parseSplit'" . tests $
    [ scope "wordyNameSegment" . tests $
      [ let s = "foo.bar.baz" in scope s . expect $
        parseSplit' wordyNameSegment s == Right (relative ["foo", "bar"], NameSegment "baz")

      , let s = "foo.bar.baz#abc" in scope s . expect $ isLeft $ parseSplit' wordyNameSegment s

      , let s = "foo.bar.+" in scope s . expect $
        isLeft $ parseSplit' wordyNameSegment s
      ]

    , scope "definitionNameSegment" . tests $
      [ let s = "foo.bar.+" in scope s . expect $
        parseSplit' definitionNameSegment s == Right (relative ["foo", "bar"], NameSegment "+")
      ]
    ]
  , scope "parseShortHashOrHQSplit'" . tests $
    [ let s = "foo.bar#34" in scope s . expect $
      parseShortHashOrHQSplit' s ==
        (Right . Right)
          (relative ["foo"], HQ'.HashQualified (NameSegment "bar") (SH.unsafeFromText "#34"))

    , let s = "foo.bar.+" in scope s . expect $
      parseShortHashOrHQSplit' s ==
        (Right . Right)
          (relative ["foo", "bar"], HQ'.NameOnly (NameSegment "+"))

    , let s = "#123" in scope s . expect $
      parseShortHashOrHQSplit' s ==
        (Right . Left) (SH.unsafeFromText "#123")
    ]
  , scope "parseHQ'Split'" . tests $
    [ let s = "foo.bar#34" in scope s . expect $
      parseHQSplit' s == Right (relative ["foo"], HQ'.HashQualified (NameSegment "bar") (SH.unsafeFromText "#34"))
    , let s = "foo.bar.+" in scope s . expect $
      parseHQSplit' s == Right (relative ["foo", "bar"], HQ'.NameOnly (NameSegment "+"))
    , let s = "#123" in scope s . expect $ isLeft $ parseHQSplit' s
    ]
  ]


relative :: Seq Text -> Path'
relative = Path' . Right . Relative . Path . fmap NameSegment
