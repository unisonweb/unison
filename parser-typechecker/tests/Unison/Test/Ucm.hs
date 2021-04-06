{-# LANGUAGE DerivingVia #-}

module Unison.Test.Ucm where

import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as Writer
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import System.FilePath ((</>))
import U.Util.Text (stripMargin)
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Prelude
import qualified Unison.Util.Pretty as P

data Runtime = Runtime1 | Runtime2

data CodebaseFormat = CodebaseFormat1 | CodebaseFormat2

newtype CodebaseName = CodebaseName {unCodebaseName :: String}

type Args = Map String String

newtype Transcript = Transcript {unTranscript :: Text} deriving (IsString) via Text

type TranscriptOutput = String

type Cleanup = Seq (IO ())

runTranscript ::
  FilePath ->
  CodebaseName ->
  CodebaseFormat ->
  Runtime ->
  Args ->
  (Args -> Transcript) ->
  WriterT Cleanup IO TranscriptOutput
runTranscript tmpDir codebaseName fmt rt args mkTranscript = do
  let configFile = tmpDir </> ".unisonConfig"
      codebasePath = tmpDir </> unCodebaseName codebaseName
  let err err = error $ "Parse error: \n" <> show err
      cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  codebase <-
    lift (Codebase.Init.createCodebase cbInit codebasePath) >>= \case
      Left e -> fail $ P.toANSI 80 e
      Right (cleanup, c) -> do
        Writer.tell . Seq.singleton $ cleanup
        pure c
  -- parse and run the transcript
  flip (either err) (TR.parse "transcript" (stripMargin . unTranscript $ mkTranscript args)) $ \stanzas ->
    liftIO . fmap Text.unpack $
      TR.run
        (case rt of Runtime1 -> Just False; Runtime2 -> Just True)
        codebasePath
        configFile
        stanzas
        codebase
