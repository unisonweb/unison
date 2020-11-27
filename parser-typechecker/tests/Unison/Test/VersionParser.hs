{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.VersionParser where

import Control.Error.Safe (rightMay)
import Data.Text
import EasyTest
import Text.Megaparsec
import Unison.Codebase.Editor.RemoteRepo
import Unison.Codebase.Editor.VersionParser
import qualified Unison.Codebase.Path as Path

test :: Test ()
test =
  scope "versionparser" . tests . fmap makeTest $
    [ ("release/M1j", "releases._M1j"),
      ("release/M1j.2", "releases._M1j"),
      ("devel/M1k", "trunk")
    ]

makeTest :: (Text, Text) -> Test ()
makeTest (version, path) =
  scope (unpack version) $
    expectEqual
      (rightMay $ runParser defaultBaseLib "versionparser" version)
      ( Just
          ( GitRepo "https://github.com/unisonweb/base" Nothing,
            Nothing,
            Path.fromText path
          )
      )
