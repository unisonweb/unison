{-# Language OverloadedStrings #-}

module Unison.Test.Referent where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Unison.Referent as R
import qualified Unison.ShortHash as SH
import EasyTest

test :: Test ()
test = scope "hashparsing" . tests $
  [ scope "Referent" $ tests
    [ r h
    , r $ h <> ".y6"
    , r $ h <> "#d10"
    , r $ h <> "#e0"
    , r $ h <> ".y6" <> "#d6"
    , r $ h <> ".y6" <> "#e9" ],

    scope "ShortHash" $ tests
    [ sh "#abcd"
    , sh h
    , sh "#abcd.y6"
    , sh "#abcd#d10"
    , sh "#abcd#e3"
    , sh "#abcd.y6#d10"
    , sh "#abcd.y6#e5" ]
  ]
  where
  h = "#zNRtcQJ2LpPxdtU8jDT7jrbR7JLC65o3GtPJXAw4Pz963kdBWB6FYNx8tatkzWj8HEA8eKZohsercgKDn9uYhbB"
  r :: Text -> Test ()
  r txt = scope (Text.unpack txt) $ case R.fromText txt of
    Nothing -> fail "oh noes"
    Just referent -> case R.fromText (R.toText referent) of
      Nothing -> fail "oh noes"
      Just referent2 -> expect (referent == referent2)
  sh :: Text -> Test ()
  sh txt = scope (Text.unpack txt) $ case SH.fromText txt of
    Nothing -> fail "oh noes"
    Just shorthash -> case SH.fromText (SH.toText shorthash) of
      Nothing -> fail "oh noes"
      Just shorthash2 -> expect (shorthash == shorthash2)
