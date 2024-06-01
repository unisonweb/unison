{-# LANGUAGE OverloadedLists #-}

module Unison.Test.Codebase.Path where

import Data.Maybe (fromJust)
import EasyTest
import Unison.Codebase.Path (Path (..), Path' (..), Relative (..))
import Unison.Codebase.Path.Parse (parseHQSplit', parseShortHashOrHQSplit')
import Unison.HashQualified' qualified as HQ'
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Prelude
import Unison.ShortHash qualified as SH

test :: Test ()
test =
  scope "path" . tests $
    [ scope "parseShortHashOrHQSplit'" . tests $
        [ let s = "foo.bar#34"
           in scope s . expect $
                parseShortHashOrHQSplit' s
                  == (Right . Right)
                    (relative ["foo"], HQ'.HashQualified (NameSegment "bar") (fromJust (SH.fromText "#34"))),
          let s = "foo.bar.+"
           in scope s . expect $
                parseShortHashOrHQSplit' s
                  == (Right . Right)
                    (relative ["foo", "bar"], HQ'.NameOnly (NameSegment "+")),
          let s = "#123"
           in scope s . expect $
                parseShortHashOrHQSplit' s
                  == (Right . Left) (fromJust (SH.fromText "#123"))
        ],
      scope "parseHQ'Split'" . tests $
        [ let s = "foo.bar#34"
           in scope s . expect $
                parseHQSplit' s == Right (relative ["foo"], HQ'.HashQualified (NameSegment "bar") (fromJust (SH.fromText "#34"))),
          let s = "foo.bar.+"
           in scope s . expect $
                parseHQSplit' s == Right (relative ["foo", "bar"], HQ'.NameOnly (NameSegment "+")),
          let s = "#123" in scope s . expect $ isLeft $ parseHQSplit' s
        ]
    ]

relative :: Seq Text -> Path'
relative = Path' . Right . Relative . Path . fmap NameSegment
