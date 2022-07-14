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
    [ ("latest-abc", "main"),
      ("dev/M4", "main"), -- or should this be "releases.M4"?
      ("dev/M4-1-g22ccb0b3b", "main"), -- and should this also be "releases.m4"?
      ("release/M4", "releases.M4"),
      ("release/M2i_3", "releases.M2i_3"),
      ("release/M2i-HOTFIX", "releases.M2i_HOTFIX")
    ]

makeTest :: (Text, Text) -> Test ()
makeTest (version, path) =
  scope (unpack version) $
    expectEqual
      (rightMay $ runParser defaultBaseLib "versionparser" version)
      ( Just
          ( ReadShareRemoteNamespace
              { server = DefaultCodeserver,
                repo = "unison",
                path = Path.fromList ["public", "base"] <> Path.fromText path
              }
          )
      )
