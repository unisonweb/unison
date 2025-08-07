{-# LANGUAGE OverloadedStrings #-}

-- | Command-line fuzzy selection of arbitrary values.
--     Shells out to fzf for the actual selection.
module Unison.CommandLine.FuzzySelect
  ( fuzzySelect,
    isFZFInstalled,
    fzfPathEnvVar,
    Options (..),
    FuzzySelections (..),
    defaultOptions,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Environment (lookupEnv)
import System.IO (BufferMode (NoBuffering), hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Prelude
import Unison.Util.Monoid qualified as Monoid
import UnliftIO qualified
import UnliftIO.Directory (findExecutable)
import UnliftIO.Exception (bracket)
import UnliftIO.IO (Handle, hGetBuffering, hSetBuffering, stdin)
import UnliftIO.Process qualified as Proc

-- | An environment variable that can be set to override the default fzf executable.
fzfPathEnvVar :: String
fzfPathEnvVar = "UNISON_FZF_PATH"

fzfExecutable :: IO (Maybe FilePath)
fzfExecutable = do
  envPath <- lookupEnv fzfPathEnvVar
  case (envPath) of
    Just path
      | Text.toUpper (Text.pack path) == "NONE" -> pure Nothing
      | otherwise -> pure (Just path)
    Nothing -> findExecutable "fzf"

isFZFInstalled :: Bool
isFZFInstalled =
  unsafePerformIO (isJust <$> fzfExecutable)
{-# NOINLINE isFZFInstalled #-}

-- | Fuzzy Selection options
data Options = Options
  { allowMultiSelect :: Bool
  }

-- | Default 'Options'
defaultOptions :: Options
defaultOptions =
  Options
    { allowMultiSelect = True
    }

-- | Convert options into command-line args for fzf
optsToArgs :: Options -> Bool -> [String]
optsToArgs opts useNumberings =
  defaultArgs <> case opts of
    Options {allowMultiSelect = True} -> ["-m"]
    _ -> []
  where
    defaultArgs =
      -- When using numberings, don't show or match on the first column of input.
      -- This allows us to prepend each line with a number, and use that number to determine
      -- which values from the input list were selected.
      Monoid.whenM
        useNumberings
        ["--with-nth", "2.."]
        <> [ -- Use only half the screen (it's nice to see what you were working on when searching)
             "--height=50%",
             -- But if 50% of the screen is too small, ensure show at least 10 results.
             "--min-height=10"
           ]

data FuzzySelections a where
  SelectFromChoices :: (a -> Text) -> [a] -> FuzzySelections a
  SelectFiles :: FuzzySelections Text

-- | Allows prompting the user to interactively fuzzy-select a result from a list of options, currently shells out to `fzf` under the hood.
-- If fzf is missing, or an error (other than ctrl-c) occurred, returns Nothing.
fuzzySelect :: forall a. Options -> FuzzySelections a -> IO (Maybe [a])
fuzzySelect opts selections =
  UnliftIO.handleAny handleException
    . handleError
    . restoreBuffering
    . runExceptT
    $ do
      fzfPath <-
        liftIO fzfExecutable >>= \case
          Nothing -> throwError "I couldn't find the `fzf` executable on your path, consider installing `fzf` to enable fuzzy searching."
          Just fzfPath -> pure fzfPath
      case selections of
        SelectFromChoices intoSearchText choices -> do
          let fzfArgs :: [String] =
                optsToArgs opts True
          let numberedChoices :: [(Int, a)] =
                zip [0 ..] choices
          let searchTexts :: [Text] =
                (\(n, ch) -> tShow (n) <> " " <> intoSearchText ch) <$> numberedChoices

          result <- lift $ fzfWithChoices fzfPath fzfArgs searchTexts
          -- Since we prefixed every search term with its number earlier, we know each result
          -- is prefixed with a number, we need to parse it and use it to select the matching
          -- value from our input list.
          pure $ case result of
            Left _ -> Nothing
            Right selections ->
              selections
                & mapMaybe (readMaybe @Int . Text.unpack . Text.takeWhile (/= ' '))
                & Set.fromList
                & ( \selectedNumbers ->
                      numberedChoices
                        & mapMaybe (\(n, a) -> if n `Set.member` selectedNumbers then Just a else Nothing)
                  )
                & Just
        SelectFiles -> do
          let fzfArgs :: [String] = optsToArgs opts False
          eitherToMaybe <$> (lift $ fzfFileSelector fzfPath fzfArgs)
  where
    fzfWithChoices :: FilePath -> [String] -> [Text] -> IO (Either SomeException [Text])
    fzfWithChoices fzfPath fzfArgs searchTexts = withHandles \((inputReadHandle, inputWriteHandle), (outputReadHandle, outputWriteHandle)) -> do
      let fzfProc :: Proc.CreateProcess =
            (Proc.proc fzfPath fzfArgs)
              { Proc.std_in = Proc.UseHandle inputReadHandle,
                Proc.std_out = Proc.UseHandle outputWriteHandle,
                Proc.delegate_ctlc = True
              }
      (_stdin, _stdout, _, procHandle) <- Proc.createProcess fzfProc
      liftIO . UnliftIO.tryAny $ do
        -- Dump the search terms into fzf's stdin
        traverse_ (Text.hPutStrLn inputWriteHandle) searchTexts
        UnliftIO.hClose inputWriteHandle
        void $ Proc.waitForProcess procHandle
        Text.lines <$> liftIO (Text.hGetContents outputReadHandle)
    fzfFileSelector :: FilePath -> [String] -> IO (Either SomeException [Text])
    fzfFileSelector fzfPath fzfArgs = withHandles \((inputReadHandle, inputWriteHandle), (outputReadHandle, outputWriteHandle)) -> do
      UnliftIO.hClose inputWriteHandle
      let fzfProc :: Proc.CreateProcess =
            (Proc.proc fzfPath fzfArgs)
              { Proc.std_in = Proc.UseHandle inputReadHandle,
                Proc.std_out = Proc.UseHandle outputWriteHandle,
                Proc.delegate_ctlc = True
              }
      (_stdin, _stdout, _, procHandle) <- Proc.createProcess fzfProc
      -- Generally no-buffering is helpful for highly interactive processes.
      hSetBuffering stdin NoBuffering
      liftIO . UnliftIO.tryAny $ do
        void $ Proc.waitForProcess procHandle
        Text.lines <$> liftIO (Text.hGetContents outputReadHandle)
    handleException :: SomeException -> IO (Maybe [a])
    handleException err = traceShowM err *> hPutStrLn stderr "Oops, something went wrong. No input selected." *> pure Nothing
    handleError :: IO (Either Text (Maybe [a])) -> IO (Maybe [a])
    handleError m =
      m >>= \case
        Left err -> Text.hPutStrLn stderr err *> pure Nothing
        Right as -> pure as
    restoreBuffering :: IO c -> IO c
    restoreBuffering action =
      bracket (hGetBuffering stdin) (hSetBuffering stdin) (const action)
    withHandles :: (((Handle, Handle), (Handle, Handle)) -> IO r) -> IO r
    withHandles action = do
      let acquire = do
            (inputReadHandle, inputWriteHandle) <- Proc.createPipe
            (outputReadHandle, outputWriteHandle) <- Proc.createPipe
            hSetBuffering inputWriteHandle UnliftIO.NoBuffering
            hSetBuffering inputReadHandle UnliftIO.NoBuffering
            hSetBuffering outputWriteHandle UnliftIO.NoBuffering
            hSetBuffering outputReadHandle UnliftIO.NoBuffering
            pure ((inputReadHandle, inputWriteHandle), (outputReadHandle, outputWriteHandle))
      let cleanup ((inputReadHandle, inputWriteHandle), (outputReadHandle, outputWriteHandle)) = do
            UnliftIO.hClose inputReadHandle
            UnliftIO.hClose inputWriteHandle
            UnliftIO.hClose outputReadHandle
            UnliftIO.hClose outputWriteHandle
      UnliftIO.bracket acquire cleanup action
