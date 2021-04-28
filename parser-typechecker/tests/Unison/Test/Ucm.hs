{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unison.Test.Ucm
  ( initCodebase,
    deleteCodebase,
    runTranscript,
    upgradeCodebase,
    CodebaseFormat (..),
    Transcript,
    unTranscript,
  )
where

import Control.Monad (when)
import qualified Data.Text as Text
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import U.Util.String (stripMargin)
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Conversion.Upgrade12 as Upgrade12
import qualified Unison.Codebase.FileCodebase as FC
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Prelude (traceM)
import qualified Unison.Util.Pretty as P

data CodebaseFormat = CodebaseFormat1 | CodebaseFormat2 deriving (Show)

data Codebase = Codebase CodebasePath CodebaseFormat deriving (Show)

-- newtype Transcript = Transcript {unTranscript :: Text}
--   deriving (IsString, Show, Semigroup) via Text
type Transcript = String
unTranscript :: a -> a
unTranscript = id

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

deleteCodebase :: Codebase -> IO ()
deleteCodebase (Codebase path _) = removeDirectoryRecursive path

upgradeCodebase :: Codebase -> IO Codebase
upgradeCodebase = \case
  c@(Codebase _ CodebaseFormat2) -> fail $ show c ++ " already in V2 format."
  Codebase path CodebaseFormat1 -> do
    Upgrade12.upgradeCodebase path
    pure $ Codebase path CodebaseFormat2

runTranscript :: Codebase -> Transcript -> IO TranscriptOutput
runTranscript (Codebase codebasePath fmt) transcript = do
  -- this configFile ought to be optional
  configFile <- do
    tmpDir <-
      Temp.getCanonicalTemporaryDirectory
        >>= flip Temp.createTempDirectory ("ucm-test")
    pure $ tmpDir </> ".unisonConfig"
  let err err = fail $ "Parse error: \n" <> show err
      cbInit = case fmt of CodebaseFormat1 -> FC.init; CodebaseFormat2 -> SC.init
  codebase <-
    Codebase.Init.openCodebase cbInit codebasePath >>= \case
      Left e -> fail $ P.toANSI 80 e
      Right x -> pure x
  Codebase.installUcmDependencies codebase
  -- parse and run the transcript
  output <-
    flip (either err) (TR.parse "transcript" (Text.pack . stripMargin $ unTranscript transcript)) $ \stanzas ->
      fmap Text.unpack $
        TR.run
          codebasePath
          configFile
          stanzas
          codebase
  when debugTranscriptOutput $ traceM output
  pure output
