module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin) where

import Control.Monad.Reader
import Unison.Auth.OAuth (authenticateCodeserver)
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Codebase.Editor.Output (Output (CredentialFailureMsg, Success))
import Unison.Share.Types
import qualified UnliftIO

authLogin :: UnliftIO.MonadUnliftIO m => Codeserver -> Action m i v ()
authLogin Codeserver {codeserverId, codeserverDescription} = do
  credsMan <- asks credentialManager
  (Action . lift . lift . lift $ authenticateCodeserver credsMan codeserverId codeserverDescription) >>= \case
    Left err -> respond (CredentialFailureMsg err)
    Right () -> respond Success
