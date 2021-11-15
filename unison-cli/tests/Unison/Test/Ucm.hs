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
import System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import U.Util.String (stripMargin)
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Codebase.TranscriptParser as TR
import Unison.Prelude (traceM)
import qualified Unison.PrettyTerminal as PT
import qualified Unison.Util.Pretty as P
import Unison.Parser.Ann (Ann)
import Unison.Symbol (Symbol)
import UnliftIO.Exception (bracket)

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
  Codebase.Init.createCodebase cbInit "ucm-test" tmp >>= \case
    Left e -> fail $ P.toANSI 80 e
    Right (close, _cb) -> close
  pure $ Codebase tmp fmt

deleteCodebase :: Codebase -> IO ()
deleteCodebase (Codebase path _) = removeDirectoryRecursive path

runTranscript :: Codebase -> Transcript -> IO TranscriptOutput
runTranscript (Codebase codebasePath fmt) transcript = do
  -- this configFile ought to be optional
  configFile <- do
    tmpDir <-
      Temp.getCanonicalTemporaryDirectory
        >>= flip Temp.createTempDirectory ("ucm-test")
    pure $ tmpDir </> ".unisonConfig"
  let err err = fail $ "Parse error: \n" <> show err
      cbInit = case fmt of CodebaseFormat2 -> SC.init
  let initCodebase =
        Codebase.Init.openCodebase cbInit "transcript" codebasePath >>= \case
          Left e -> fail $ P.toANSI 80 e
          Right x -> pure x
  bracket initCodebase (\(closeCodebase, _) -> closeCodebase) $ \(_, codebase) -> do
    Codebase.installUcmDependencies codebase
    -- parse and run the transcript
    output <-
      flip (either err) (TR.parse "transcript" (Text.pack . stripMargin $ unTranscript transcript)) $ \stanzas ->
        fmap Text.unpack $
          TR.run "Unison.Test.Ucm.runTranscript Invalid Version String"
            codebasePath
            configFile
            stanzas
            codebase
    when debugTranscriptOutput $ traceM output
    pure output

lowLevel :: Codebase -> (Codebase.Codebase IO Symbol Ann -> IO a) -> IO a
lowLevel (Codebase root fmt) f = do
  let cbInit = case fmt of CodebaseFormat2 -> SC.init
  Codebase.Init.openCodebase cbInit "lowLevel" root >>= \case
    Left p -> PT.putPrettyLn p *> pure (error "This really should have loaded")
    Right (close, cb) -> f cb <* close
