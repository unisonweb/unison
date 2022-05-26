module Unison.Codebase.Editor.HandleInput.AuthLogin (authLogin) where

import Control.Monad.Reader
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)
import Unison.Auth.OAuth
import Unison.Auth.Types (Host (..))
import Unison.Codebase.Editor.HandleInput.LoopState
import Unison.Codebase.Editor.Output (Output (CredentialFailureMsg, Success))
import Unison.Prelude
import qualified UnliftIO
import UnliftIO.Environment (lookupEnv)

defaultShareHost :: Host
defaultShareHost = unsafePerformIO $ do
  lookupEnv "UNISON_SHARE_HOST" <&> \case
    Nothing -> Host "share.unison-lang.org"
    Just shareHost -> Host (Text.pack shareHost)
{-# NOINLINE defaultShareHost #-}

authLogin :: UnliftIO.MonadUnliftIO m => Maybe Host -> Action m i v ()
authLogin mayHost = do
  let host = fromMaybe defaultShareHost mayHost
  credsMan <- asks credentialManager
  (Action . lift . lift . lift $ authenticateHost credsMan host) >>= \case
    Left err -> respond (CredentialFailureMsg err)
    Right () -> respond Success
