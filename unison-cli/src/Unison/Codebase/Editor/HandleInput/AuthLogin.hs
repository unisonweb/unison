module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin, ensureAuthenticatedWithCodeserver) where

import Control.Monad.Reader
import Unison.Auth.CredentialManager (getCredentials)
import Unison.Auth.OAuth (authenticateCodeserver)
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Codebase.Editor.Output (Output (CredentialFailureMsg, Success))
import Unison.Share.Types
import qualified UnliftIO

-- | Checks if the user has valid auth for the given codeserver,
-- and runs through an authentication flow if not.
ensureAuthenticatedWithCodeserver :: UnliftIO.MonadUnliftIO m => CodeserverURI -> Action m i v ()
ensureAuthenticatedWithCodeserver codeserverURI = do
  credsMan <- asks credentialManager
  getCredentials credsMan (codeserverIdFromCodeserverURI codeserverURI) >>= \case
    Right _ -> pure ()
    Left _ -> authLogin codeserverURI

authLogin :: UnliftIO.MonadUnliftIO m => Codeserver -> Action m i v ()
authLogin Codeserver {codeserverId, codeserverDescription} = do
  credsMan <- asks credentialManager
  (Action . lift . lift . lift $ authenticateCodeserver credsMan codeserverId codeserverDescription) >>= \case
    Left err -> respond (CredentialFailureMsg err)
    Right () -> respond Success
