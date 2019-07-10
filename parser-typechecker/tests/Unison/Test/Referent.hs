{-# Language OverloadedStrings #-}

module Unison.Test.Referent where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Unison.Referent as R
import qualified Unison.ShortHash as SH
import qualified Unison.Reference as Rf
import EasyTest

test :: Test ()
test = scope "hashparsing" . tests $
  [
    scope "Reference" $ tests
    [ ref h
    , ref (h <> "." <> suffix1)
    , ref (h <> "." <> suffix2) ],

    scope "Referent" $ tests
    [ r h
    , r $ h <> "." <> suffix1
    , r $ h <> "#d10"
    , r $ h <> "#a0"
    , r $ h <> "." <> suffix2 <> "#d6"
    , r $ h <> "." <> suffix1 <> "#a9" ],

    scope "ShortHash" $ tests
    [ sh h
    , sh "#abcd"
    , sh $ "#abcd." <> suffix1
    , sh "#abcd#d10"
    , sh "#abcd#a3"
    , sh $ "#abcd." <> suffix2 <> "#d10"
    , sh $ "#abcd.y6#a5" ]
  ]
  where
  h = "#zNRtcQJ2LpPxdtU8jDT7jrbR7JLC65o3GtPJXAw4Pz963kdBWB6FYNx8tatkzWj8HEA8eKZohsercgKDn9uYhbB"
  suffix1 = Rf.showSuffix 0 10
  suffix2 = Rf.showSuffix 3 6
  ref txt = scope (Text.unpack txt) $ case Rf.fromText txt of
    Left e -> fail e
    Right r1 -> case Rf.fromText (Rf.toText r1) of
      Left e -> fail e
      Right r2 -> expect (r1 == r2)
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
