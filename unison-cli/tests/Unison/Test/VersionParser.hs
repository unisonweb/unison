{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.VersionParser where

import EasyTest
import Data.Text
import Unison.Codebase.Editor.VersionParser
import qualified Unison.Codebase.Path as Path
import Control.Error.Safe (rightMay)
import Unison.Codebase.Editor.RemoteRepo
import Text.Megaparsec

test :: Test ()
test = scope "versionparser" . tests . fmap makeTest $
  [ ("release/M1j", "releases._M1j")
  , ("release/M1j.2", "releases._M1j_2")
  , ("latest-abc", "trunk")
  , ("release/M2i_3", "releases._M2i_3")
  , ("release/M2i-HOTFIX", "releases._M2i_HOTFIX")
  ]

makeTest :: (Text, Text) -> Test ()
makeTest (version, path) =
  scope (unpack version) $ expectEqual
    (rightMay $ runParser defaultBaseLib "versionparser" version)
    (Just
      -- We've hard-coded the v2 branch for base for now. See 'defaultBaseLib'
      ( ReadGitRepo "https://github.com/unisonweb/base" (Just "v2")
      , Nothing
      , Path.fromText path ))
