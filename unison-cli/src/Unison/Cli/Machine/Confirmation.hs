module Unison.Cli.Machine.Confirmation
  ( confirmation,
  )
where

import System.Console.Haskeline
import Unison.Cli.Machine

promptBool :: String -> InputT IO (Maybe Bool)
promptBool prompt = handleCtrlC do
  getInputChar prompt >>= \case
    Nothing -> pure Nothing
    Just c -> case c of
      'y' -> pure (Just True)
      'n' -> pure (Just False)
      _ -> promptBool prompt

confirmation :: Machine IO () (Maybe Bool)
confirmation =
  Machine
    ( \() -> do
        putStrLn "Confirm: [y/n]"
        runInputT defaultSettings (promptBool "")
    )
    \() mbool -> do
      case mbool of
        Nothing -> pure (Return () Nothing)
        Just bool -> do
          pure (Return () (Just bool))
