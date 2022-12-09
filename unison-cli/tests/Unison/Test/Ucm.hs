{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unison.Test.Ucm
  ( initCodebase,
    deleteCodebase,
    runTranscript,
    lowLevel,
    CodebaseFormat (..),
    Transcript,
    unTranscript,
    Codebase (..),
  )
where

import Control.Monad (when)
import qualified Data.Text as Text
import System.Directory (removeDirectoryRecursive)
import qualified System.IO.Temp as Temp
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Init as Codebase.Init
import Unison.Codebase.Init.CreateCodebaseError (CreateCodebaseError (..))
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Parser.Ann (Ann)
import Unison.Prelude (traceM)
import qualified Unison.PrettyTerminal as PT
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import Unison.Util.Text (stripMargin)

data CodebaseFormat = CodebaseFormat2 deriving (Show, Enum, Bounded)

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
  let cbInit = case fmt of CodebaseFormat2 -> SC.init
  tmp <-
    Temp.getCanonicalTemporaryDirectory
      >>= flip Temp.createTempDirectory "ucm-test"
  result <- Codebase.Init.withCreatedCodebase cbInit "ucm-test" tmp SC.DoLock (const $ pure ())
  case result of
    Left CreateCodebaseAlreadyExists -> fail $ P.toANSI 80 "Codebase already exists"
    Right _ -> pure $ Codebase tmp fmt

deleteCodebase :: Codebase -> IO ()
deleteCodebase (Codebase path _) = removeDirectoryRecursive path

runTranscript :: Codebase -> Transcript -> IO TranscriptOutput
runTranscript (Codebase codebasePath fmt) transcript = do
  let err e = fail $ "Parse error: \n" <> show e
      cbInit = case fmt of CodebaseFormat2 -> SC.init
  TR.withTranscriptRunner "Unison.Test.Ucm.runTranscript Invalid Version String" configFile $ \runner -> do
    result <- Codebase.Init.withOpenCodebase cbInit "transcript" codebasePath SC.DoLock SC.DontMigrate \codebase -> do
      Codebase.runTransaction codebase (Codebase.installUcmDependencies codebase)
      let transcriptSrc = stripMargin . Text.pack $ unTranscript transcript
      output <- either err Text.unpack <$> runner "transcript" transcriptSrc (codebasePath, codebase)
      when debugTranscriptOutput $ traceM output
      pure output
    case result of
      Left e -> fail $ P.toANSI 80 (P.shown e)
      Right x -> pure x
  where
    configFile = Nothing

lowLevel :: Codebase -> (Codebase.Codebase IO Symbol Ann -> IO a) -> IO a
lowLevel (Codebase root fmt) action = do
  let cbInit = case fmt of CodebaseFormat2 -> SC.init
  result <- Codebase.Init.withOpenCodebase cbInit "lowLevel" root SC.DoLock SC.DontMigrate action
  case result of
    Left e -> PT.putPrettyLn (P.shown e) *> pure (error "This really should have loaded")
    Right a -> pure a
