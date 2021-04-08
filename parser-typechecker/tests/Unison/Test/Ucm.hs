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

import Control.Monad (when)
import qualified Data.Text as Text
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase.Conversion.Upgrade12 as Upgrade12
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Prelude (IsString, Text, traceM)
import qualified Unison.Util.Pretty as P

data Runtime = Runtime1 | Runtime2

data CodebaseFormat = CodebaseFormat1 | CodebaseFormat2 deriving (Show)

data Codebase = Codebase CodebasePath CodebaseFormat deriving (Show)

newtype Transcript = Transcript {unTranscript :: Text}
  deriving Show
  deriving (IsString) via Text

type TranscriptOutput = String
debugTranscriptOutput :: Bool
debugTranscriptOutput = False

initCodebase :: CodebaseFormat -> IO Codebase
initCodebase fmt = do
  let cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  tmp <-
    Temp.getCanonicalTemporaryDirectory
      >>= flip Temp.createTempDirectory ("ucm-test")
  Codebase.Init.createCodebase cbInit tmp >>= \case
    Left e -> fail $ P.toANSI 80 e
    Right {} -> pure ()
  pure $ Codebase tmp fmt

upgradeCodebase :: Codebase -> IO Codebase
upgradeCodebase = \case
  c@(Codebase _ CodebaseFormat2) -> fail $ show c ++ " already in V2 format."
  Codebase path CodebaseFormat1 -> do
    Upgrade12.upgradeCodebase path
    pure $ Codebase path CodebaseFormat2

runTranscript :: Codebase -> Runtime -> Transcript -> IO TranscriptOutput
runTranscript (Codebase codebasePath fmt) rt transcript = do
  traceM $ show transcript
  -- this configFile ought to be optional
  configFile <- do
    tmpDir <-
      Temp.getCanonicalTemporaryDirectory
        >>= flip Temp.createTempDirectory ("ucm-test")
    pure $ tmpDir </> ".unisonConfig"
  let err err = fail $ "Parse error: \n" <> show err
      cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  (closeCodebase, codebase) <-
    Codebase.Init.openCodebase cbInit codebasePath >>= \case
      Left e -> fail $ P.toANSI 80 e
      Right x -> pure x
  -- parse and run the transcript
  output <-
    flip (either err) (TR.parse "transcript" ({-stripMargin $-} unTranscript transcript)) $ \stanzas ->
      fmap Text.unpack $
        TR.run
          (case rt of Runtime1 -> Just False; Runtime2 -> Just True)
          codebasePath
          configFile
          stanzas
          codebase
  closeCodebase
  when debugTranscriptOutput $ traceM output
  pure output
