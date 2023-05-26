module Unison.Cli.Machine
  ( Machine (..),
    MachineResult (..),
    runMachine,
    handleCtrlC,
  )
where

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
  | forall s o. Call (o -> s -> m (MachineResult state m output)) (Machine m s o) s

runMachine :: forall m s o. Monad m => Machine m s o -> s -> m (o, s)
runMachine m@(Machine parseInput transition) s = do
  i <- parseInput s
  transition s i >>= processTransition
  where
    processTransition :: MachineResult s m o -> m (o, s)
    processTransition = \case
      Continue s' -> runMachine m s'
      Return s' o -> pure (o, s')
      Call func subm subs0 -> do
        (o, s) <- runMachine subm subs0
        processTransition =<< func o s

handleCtrlC :: (MonadIO m, MonadMask m) => InputT m a -> InputT m a
handleCtrlC act =
  handleInterrupt (pure Nothing) (withInterrupt (Just <$> act)) >>= \case
    Nothing -> handleCtrlC act
    Just a -> pure a
