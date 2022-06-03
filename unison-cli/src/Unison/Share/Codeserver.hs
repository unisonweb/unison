module Unison.Share.Codeserver where

import Network.URI (parseURI)
import System.IO.Unsafe (unsafePerformIO)
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import Unison.Prelude
import Unison.Share.Types
import qualified Unison.Share.Types as Share
import UnliftIO.Environment (lookupEnv)

-- | This is the URI where the share API is based.
defaultCodeserver :: CodeserverURI
defaultCodeserver = unsafePerformIO $ do
  lookupEnv "UNISON_SHARE_HOST" <&> \case
    -- TODO: swap to production share before release.
    Nothing ->
      CodeserverURI
        { codeserverScheme = Share.Https,
          codeserverUserInfo = "",
          codeserverRegName = "share-next.us-west-2.unison-lang.org",
          codeserverPort = Just 443,
          codeserverPath = ["api"]
        }
    Just shareHost ->
      fromMaybe (error $ "Share Host is not a valid URI: " <> shareHost) $ do
        uri <- parseURI shareHost
        codeserverFromURI uri
{-# NOINLINE defaultCodeserver #-}

resolveCodeserver :: RemoteRepo.ShareCodeserver -> CodeserverURI
resolveCodeserver = \case
  RemoteRepo.DefaultCodeserver -> defaultCodeserver
  RemoteRepo.CustomCodeserver cs -> cs
