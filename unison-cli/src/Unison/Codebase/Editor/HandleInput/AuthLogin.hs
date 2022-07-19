module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin, ensureAuthenticatedWithCodeserver) where

import Control.Monad.Reader
import Unison.Auth.CredentialManager (getCredentials)
import Unison.Auth.OAuth (authenticateCodeserver)
import Unison.Codebase.Editor.Command
  ( Action,
    Env (credentialManager),
    respond,
  )
import Unison.Codebase.Editor.Output (Output (CredentialFailureMsg, Success))
import Unison.Share.Types

-- | Checks if the user has valid auth for the given codeserver,
-- and runs through an authentication flow if not.
ensureAuthenticatedWithCodeserver :: CodeserverURI -> Action i v ()
ensureAuthenticatedWithCodeserver codeserverURI = do
  credsMan <- asks credentialManager
  getCredentials credsMan (codeserverIdFromCodeserverURI codeserverURI) >>= \case
    Right _ -> pure ()
    Left _ -> authLogin codeserverURI

authLogin :: CodeserverURI -> Action i v ()
authLogin host = do
  credsMan <- asks credentialManager
  authenticateCodeserver credsMan host >>= \case
    Left err -> respond (CredentialFailureMsg err)
    Right () -> respond Success
