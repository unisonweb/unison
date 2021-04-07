{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unison.Test.Ucm
  ( initCodebase,
    runTranscript,
    upgradeCodebase,
    CodebaseFormat (..),
    Runtime (..),
  )
where

import Control.Monad.Catch (MonadCatch)
import qualified Data.Text as Text
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import U.Util.Text (stripMargin)
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase.Conversion.Upgrade12 as Upgrade12
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Prelude
import qualified Unison.Util.Pretty as P

data Runtime = Runtime1 | Runtime2

data CodebaseFormat = CodebaseFormat1 | CodebaseFormat2 deriving (Show)

data Codebase = Codebase CodebasePath CodebaseFormat deriving (Show)

newtype Transcript = Transcript {unTranscript :: Text} deriving (IsString) via Text

type TranscriptOutput = String

initCodebase :: (MonadIO m, MonadCatch m) => CodebaseFormat -> m Codebase
initCodebase fmt = do
  let cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  tmp <-
    liftIO $
      Temp.getCanonicalTemporaryDirectory
        >>= flip Temp.createTempDirectory ("ucm-test")
  Codebase.Init.createCodebase cbInit tmp >>= \case
    Left e -> error $ P.toANSI 80 e
    Right {} -> pure ()
  pure $ Codebase tmp fmt

upgradeCodebase :: (MonadIO m, MonadCatch m) => Codebase -> m Codebase
upgradeCodebase = \case
  c@(Codebase _ CodebaseFormat2) -> error $ show c ++ " already in V2 format."
  Codebase path CodebaseFormat1 -> do
    Upgrade12.upgradeCodebase path
    pure $ Codebase path CodebaseFormat2

runTranscript :: (Monad m, MonadIO m, MonadCatch m) => Codebase -> Runtime -> Transcript -> m TranscriptOutput
runTranscript (Codebase codebasePath fmt) rt transcript = do
  -- this configFile ought to be optional
  configFile <- do
    tmpDir <-
      liftIO $
        Temp.getCanonicalTemporaryDirectory
          >>= flip Temp.createTempDirectory ("ucm-test")
    pure $ tmpDir </> ".unisonConfig"
  let err err = error $ "Parse error: \n" <> show err
      cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  (cleanup, codebase) <-
    liftIO (Codebase.Init.createCodebase cbInit codebasePath) >>= \case
      Left e -> error $ P.toANSI 80 e
      Right x -> pure x
  -- parse and run the transcript
  output <- liftIO $
    flip (either err) (TR.parse "transcript" (stripMargin $ unTranscript transcript)) $ \stanzas ->
      fmap Text.unpack $
        TR.run
          (case rt of Runtime1 -> Just False; Runtime2 -> Just True)
          codebasePath
          configFile
          stanzas
          codebase
  liftIO cleanup
  pure output
