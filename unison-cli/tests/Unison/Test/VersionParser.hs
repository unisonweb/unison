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
      ("release/M1j.2", "releases._M1j_2"),
      ("latest-abc", "trunk"),
      ("release/M2i_3", "releases._M2i_3"),
      ("release/M2i-HOTFIX", "releases._M2i_HOTFIX")
    ]

makeTest :: (Text, Text) -> Test ()
makeTest (version, path) =
  scope (unpack version) $
    expectEqual
      (rightMay $ runParser defaultBaseLib "versionparser" version)
      ( Just
          ( ReadShareRemoteNamespace
              { server = DefaultShare,
                repo = "unison",
                path = Path.fromList ["public", "dev", "base"] <> Path.fromText path
              }
          )
      )
