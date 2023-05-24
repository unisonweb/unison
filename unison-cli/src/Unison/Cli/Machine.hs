module Unison.Cli.Machine where

import Control.Monad.Catch (MonadMask)
import System.Console.Haskeline
import Unison.Prelude

data Machine m state output = forall input.
  Machine
  { parseInput :: state -> m input,
    transition :: state -> input -> m (MachineResult state m output)
  }

data MachineResult state m output
  = Continue state
  | Return state output
  | forall s o. Call (o -> m (MachineResult state m output)) (Machine m s o) s

runMachine :: forall m s o. Monad m => Machine m s o -> s -> m o
runMachine m@(Machine parseInput transition) s = do
  i <- parseInput s
  transition s i >>= processTransition
  where
    processTransition :: MachineResult s m o -> m o
    processTransition = \case
      Continue s' -> runMachine m s'
      Return _s' o -> pure o
      Call func subm subs0 -> do
        subr <- runMachine subm subs0
        processTransition =<< func subr

parseLine :: forall m. (MonadIO m, MonadMask m) => m String -> InputT m (Maybe String)
parseLine getPrompt =
  handleCtrlC go
  where
    go :: InputT m (Maybe String)
    go = do
      promptString <- lift getPrompt
      getInputLine promptString

handleCtrlC :: (MonadIO m, MonadMask m) => InputT m a -> InputT m a
handleCtrlC act =
  handleInterrupt (pure Nothing) (withInterrupt (Just <$> act)) >>= \case
    Nothing -> handleCtrlC act
    Just a -> pure a

promptBool :: forall m. (MonadIO m, MonadMask m) => String -> InputT m (Maybe Bool)
promptBool prompt = handleCtrlC do
  getInputChar prompt >>= \case
    Nothing -> pure Nothing
    Just c -> case c of
      'y' -> pure (Just True)
      'n' -> pure (Just False)
      _ -> promptBool prompt

confirmationMachine :: Machine IO () (Maybe Bool)
confirmationMachine =
  Machine
    (\() -> runInputT defaultSettings (promptBool ""))
    \() mbool -> do
      case mbool of
        Nothing -> pure (Return () Nothing)
        Just bool -> do
          pure (Return () (Just bool))
