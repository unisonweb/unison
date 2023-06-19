module Unison.Codebase.Editor.HandleInput.SquashNamespace (squashNamespace) where

import U.Codebase.Causal (Causal (..))
import U.Codebase.Causal.Squash qualified as Causal
import Unison.Cli.Monad
import Unison.Cli.Monad as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path
import Unison.Codebase.Path qualified as Path
import Unison.Prelude

squashNamespace :: Maybe Path' -> Cli ()
squashNamespace path' = do
  p' <- case path' of
    Just p -> Cli.resolvePath' p
    Nothing -> Cli.getCurrentPath
  Causal {causalHash, valueHash} <- Cli.runTransaction $ do
    causal <- Codebase.getShallowCausalAtPath (Path.unabsolute p') Nothing
    Causal.squashCausal causal
  liftIO $ print $ "Squashed " <> show (causalHash, valueHash)
  pure ()
