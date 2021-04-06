{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unison.Test.Ucm where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.State (MonadState, StateT)
import qualified Control.Monad.State as State
import Control.Monad.Writer (MonadWriter, WriterT)
import qualified Control.Monad.Writer as Writer
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import EasyTest
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import U.Util.Text (stripMargin)
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Prelude
import qualified Unison.Util.Pretty as P
import UnliftIO (MonadUnliftIO)

data Runtime = Runtime1 | Runtime2

data CodebaseFormat = CodebaseFormat1 | CodebaseFormat2 deriving Show

data Codebase = Codebase CodebasePath CodebaseFormat deriving Show

newtype Transcript = Transcript {unTranscript :: Text} deriving (IsString) via Text

type TranscriptOutput = String

type Cleanup m = Seq (m ())

initCodebase :: (MonadIO m, MonadCatch m, MonadWriter (Cleanup m) m) => CodebaseFormat -> m Codebase
initCodebase fmt = do
  let cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  tmp <-
    liftIO $
      Temp.getCanonicalTemporaryDirectory
        >>= flip Temp.createTempDirectory ("ucm-test")
  void $
    Codebase.Init.createCodebase cbInit tmp >>= \case
      Left e -> error $ P.toANSI 80 e
      Right (cleanup, c) -> do
        Writer.tell . Seq.singleton $ cleanup
        pure c
  pure $ Codebase tmp fmt

upgradeCodebase :: MonadIO m => Codebase -> m Codebase
upgradeCodebase = \case
  c@(Codebase _ CodebaseFormat2) -> error $ show c ++ " already in V2 format."
  Codebase path CodebaseFormat1 -> undefined

-- type UcmTest m = (MonadIO m, MonadWriter (Cleanup m) m, MonadState Args m)
-- runTranscript ::
--   UcmTest m =>
--   FilePath ->
--   CodebaseName ->
--   CodebaseFormat ->
--   Runtime ->
--   (Args -> Transcript) ->
--   m TranscriptOutput
-- runTranscript tmpDir codebaseName fmt rt mkTranscript = do
--   let configFile = tmpDir </> ".unisonConfig"
--       codebasePath = tmpDir </> unCodebaseName codebaseName
--   let err err = error $ "Parse error: \n" <> show err
--       cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
--   codebase <-
--     (Codebase.Init.createCodebase cbInit codebasePath) >>= \case
--       Left e -> error $ P.toANSI 80 e
--       Right (cleanup, c) -> do
--         Writer.tell . Seq.singleton $ cleanup
--         pure c
--   -- parse and run the transcript
--   args <- State.get
--   flip (either err) (TR.parse "transcript" (stripMargin . unTranscript $ mkTranscript args)) $ \stanzas ->
--     liftIO . fmap Text.unpack $
--       TR.run
--         (case rt of Runtime1 -> Just False; Runtime2 -> Just True)
--         codebasePath
--         configFile
--         stanzas
--         codebase

-- runTests :: UcmTest m => m a -> Test a
-- runTests a = do
--   ((result, cleanup), _args) <- flip State.runStateT mempty $ Writer.runWriterT a
--   sequence_ cleanup
--   pure result
--   pure result
