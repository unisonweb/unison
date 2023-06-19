module Unison.Codebase.Editor.HandleInput.SquashNamespace (squashNamespace) where

import U.Codebase.Causal (Causal (..))
import U.Codebase.Causal.Squash qualified as Causal
import Unison.Cli.Monad
import Unison.Cli.Monad as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (discardHistory)
import Unison.Codebase.Branch.Type as V1Branch
import Unison.Codebase.Causal.Type as V1Causal
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Sqlite (unsafeIO)

squashNamespace :: Maybe Path' -> Cli ()
squashNamespace path' = do
  abs <- case path' of
    Just p -> Cli.resolvePath' p
    Nothing -> Cli.getCurrentPath
  !currentV1Branch <- Cli.getBranchAt abs
  time "V2 squash" $ Cli.runTransaction $ do
    causal <- Codebase.getShallowCausalAtPath (Path.unabsolute abs) Nothing
    Causal {causalHash, valueHash} <- Causal.squashCausal causal
    unsafeIO $ putStrLn $ "Squashed " <> show (causalHash, valueHash)
  time "V1 squash" $ do
    let !unwrapped = V1Branch._history $ discardHistory currentV1Branch
    liftIO . putStrLn $ "V1 Squashed causal: " <> show (V1Causal.currentHash unwrapped, V1Causal.valueHash unwrapped)
  time "V1 squash again" $ do
    let !unwrapped = V1Branch._history $ discardHistory currentV1Branch
    liftIO . putStrLn $ "V1 Squashed causal: " <> show (V1Causal.currentHash unwrapped, V1Causal.valueHash unwrapped)
  pure ()
