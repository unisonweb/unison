module Unison.Cloud.Codeserver (defaultCloudAPI) where

import Network.URI (parseURI)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Prelude
import Unison.Share.Types
import UnliftIO.Environment (lookupEnv)

-- | This is the URI where the cloud API is based.
defaultCloudAPI :: CodeserverURI
defaultCloudAPI = unsafePerformIO $ do
  lookupEnv "UNISON_CLOUD_HOST" <&> \case
    Nothing ->
      CodeserverURI
        { codeserverScheme = Https,
          codeserverUserInfo = "",
          codeserverRegName = "api.unison.cloud",
          codeserverPort = Nothing,
          codeserverPath = []
        }
    Just cloudHost ->
      fromMaybe (error $ "Cloud Host is not a valid URI: " <> cloudHost) $ do
        uri <- parseURI cloudHost
        codeserverFromURI uri
{-# NOINLINE defaultCloudAPI #-}
