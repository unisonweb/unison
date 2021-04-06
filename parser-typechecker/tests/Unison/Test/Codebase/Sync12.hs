{-# LANGUAGE TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Codebase.Sync12 where

-- import Control.Error (minimumMay)
-- import Control.Lens (view, _1)
-- import qualified Data.Char as Char
-- import Data.Maybe (fromMaybe)
-- import EasyTest (Test, scope, tests, io)
-- import qualified System.IO.Temp as Temp
-- import Unison.Test.Ucm (CodebaseFormat, Runtime)
-- import qualified Data.Text as Text
-- import qualified U.Util.Text as Text
-- import Data.String.Here.Interpolated (iTrim)
-- import qualified Unison.Test.Ucm as Ucm
-- import Shellmet ()
-- import System.FilePath ((</>))
-- import UnliftIO (MonadIO (liftIO))


-- -- test = scope "Sync" $ tests [typeAlias, topLevelTerm, subNamespace, accessPatch, history]

-- typeAlias :: Test ()
-- typeAlias = makeTest "typeAlias" do
--   tmp <- Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "typeAlias"
--   c1 <- Ucm.initCodebase Ucm.CodebaseFormat1 tmp
--   runTranscript c1 [iTrim|
--       ```ucm
--       .> alias.type ##Nat builtin.Nat
--       ```
--     |]
--   c2 <- runConversion12 c
--   runTranscript c2 $ Text.stripMargin [iTrim|
--     ```unison
--     x :: Nat
--     x = 3
--     ```
--   |]

-- topLevelTerm :: Test ()
-- topLevelTerm = makeTest "topLevelTerm" do
--   tmp <- Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "typeAlias"
--   c1 <- initV1Codebase tmp
--   runTranscript c1 [iTrim|
--       ```unison
--       y = 3
--       ```
--       ```ucm
--       .> add
--       ```
--     |]
--   runTranscript c2 [iTrim|
--     ```ucm
--     .> find
--     ```
--     ```unison
--     > y
--     ```
--   |]

-- subNamespace :: Test ()
-- subNamespace = makeTest "subNamespace" do
--   tmp <- Temp.getCanonicalTemporaryDirectory >>= flip Temp.createTempDirectory "subNamespace"
--   runTranscript tmp "src" $ Text.stripMargin [iTrim|
--       ```ucm
--       .> alias.type ##Nat builtin.Nat
--       ```
--       ```unison
--       type myLib.X = X Nat
--       ```
--       ```ucm
--       .> push ${repo} myLib
--       ```
--     |]
--   runTranscript tmp "dest" $ Text.stripMargin [iTrim|
--     ```ucm
--     .> pull ${repo} yourLib
--     .> find
--     ```
--     ```unison
--     > X 3
--     ```
--   |]

