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
    , sh $ "#abcd.y6#a5"
    , scope "builtin" $
        expect (SH.fromText "##Text.take" == Just (SH.Builtin "Text.take"))
    , pending $ scope "builtins don't have CIDs" $
        expect (SH.fromText "##FileIO#3" == Nothing)
    , scope "term ref, no cycle" $
        expect (SH.fromText "#2tWjVAuc7" ==
                  Just (SH.ShortHash "2tWjVAuc7" Nothing Nothing))
    , scope "term ref, part of cycle" $
        expect (SH.fromText "#y9ycWkiC1.y9" ==
                  Just (SH.ShortHash "y9ycWkiC1" (Just "y9") Nothing))
    , scope "constructor" $
        expect (SH.fromText "#cWkiC1x89#1" ==
                  Just (SH.ShortHash "cWkiC1x89" Nothing (Just "1")))
    , scope "constructor of a type in a cycle" $
        expect (SH.fromText "#DCxrnCAPS.WD#0" ==
                  Just (SH.ShortHash "DCxrnCAPS" (Just "WD") (Just "0")))
    , scope "Anything to the left of the first # is ignored" $
        expect (SH.fromText "foo#abc" ==
                  Just (SH.ShortHash "abc" Nothing Nothing))
    , pending $ scope "Anything including and following a third # is rejected" $
        expect (SH.fromText "foo#abc#2#hello" == Nothing)
    , scope "Anything after a second . before a second # is ignored" $
        expect (SH.fromText "foo#abc.1f.x" ==
                  Just (SH.ShortHash "abc" (Just "1f") Nothing))
    ]
  ]
  where
  h = "#1tdqrgl90qnmqvrff0j76kg2rnajq7n8j54e9cbk4p8pdi41q343bnh8h2rv6nadhlin8teg8371d445pvo0as7j2sav8k401d2s3no"
  suffix1 = Rf.showSuffix 0
  suffix2 = Rf.showSuffix 3
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
