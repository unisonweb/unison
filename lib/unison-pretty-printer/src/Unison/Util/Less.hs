module Unison.Util.Less where

import Control.Exception.Extra (ignore)
import Data.Char qualified as Char
import Data.Text.IO qualified as Text
import GHC.IO (unsafePerformIO)
import System.Environment (lookupEnv)
import System.IO (hClose)
import System.Process
import Unison.Prelude
import UnliftIO qualified
import UnliftIO.Directory (findExecutable)

shouldUsePager :: Bool
shouldUsePager = unsafePerformIO $ do
  inEmacs <- isJust <$> lookupEnv "INSIDE_EMACS"
  disablePager <-
    lookupEnv "UNISON_DISABLE_PAGER" <&> \mayStr ->
      fromMaybe False (mayStr >>= parseBool)
  isTerminal <- UnliftIO.hIsTerminalDevice UnliftIO.stdin
  pure $
    if inEmacs || disablePager
      then False
      else isTerminal
  where
    parseBool =
      map Char.toLower >>> \case
        "true" -> Just True
        "false" -> Just False
        _ -> Nothing
{-# NOINLINE shouldUsePager #-}

less :: Text -> IO ()
less str = do
  if shouldUsePager
    then usePager
    else noPager
  where
    noPager :: IO ()
    noPager = Text.putStr str
    usePager :: IO ()
    usePager = do
      pager <-
        runMaybeT $
          msum
            [ shell <$> MaybeT (lookupEnv "UNISON_PAGER"),
              MaybeT (findExecutable "less") <&> \less -> proc less lessArgs,
              -- most windows machines have 'more'.
              MaybeT (findExecutable "more") <&> \more -> proc more []
            ]
      case pager of
        Nothing -> noPager
        Just process -> do
          (Just stdin, _stdout, _stderr, pid) <-
            createProcess process {std_in = CreatePipe}

          -- If pager exits before consuming all of stdin, `hPutStr` will crash.
          ignore $ Text.hPutStr stdin str

          -- If pager has already exited, hClose throws an exception.
          ignore $ hClose stdin

          -- Wait for pager to exit.
          void $ waitForProcess pid

    lessArgs :: [String]
    lessArgs =
      [ "--no-init", -- don't clear the screen on exit
        "--RAW-CONTROL-CHARS", -- pass through colors and stuff
        "--prompt=[less] Use space/arrow keys to navigate, or 'q' to return to ucm:",
        "--quit-if-one-screen" -- self-explanatory
      ]
