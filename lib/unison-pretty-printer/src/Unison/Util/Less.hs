module Unison.Util.Less where

import Control.Exception.Extra (ignore)
import System.Environment (lookupEnv)
import System.IO (hClose, hPutStr)
import System.Process
import Unison.Prelude
import qualified UnliftIO
import UnliftIO.Directory (findExecutable)

less :: String -> IO ()
less str = do
  isInteractive <-
    lookupEnv "INSIDE_EMACS" >>= \case
      Just _ -> pure False
      Nothing -> UnliftIO.hIsTerminalDevice UnliftIO.stdin
  if isInteractive
    then usePager
    else noPager
  where
    noPager :: IO ()
    noPager = putStr str
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
          ignore $ hPutStr stdin str

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
