{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Share.Types where

import Data.Aeson
import Data.Text
import qualified Data.Text as Text
import Network.URI
import Unison.Prelude

data CodeserverURI = CodeserverURI
  { codeserverScheme :: String,
    codeserverAuthority :: URIAuth,
    codeserverPath :: String
  }
  deriving stock (Show, Eq, Ord)

codeserverToURI :: CodeserverURI -> URI
codeserverToURI (CodeserverURI {..}) =
  URI
    { uriScheme = codeserverScheme,
      uriAuthority = Just codeserverAuthority,
      uriPath = codeserverPath,
      uriQuery = "",
      uriFragment = ""
    }

codeserverFromURI :: URI -> Maybe CodeserverURI
codeserverFromURI URI {..} = do
  uriAuth <- uriAuthority
  pure $
    CodeserverURI
      { codeserverScheme = uriScheme,
        codeserverAuthority = uriAuth,
        codeserverPath = uriPath
      }

-- | This is distinct from the codeserver URI in that we store credentials by a normalized ID, since it's
-- much easier to look up that way than from an arbitrary path.
-- We may wish to use explicitly named configurations in the future.
-- This currently uses the 'uriRegName' from a parsed URI as the identifier.
newtype CodeserverId = CodeserverId {codeserverId :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Gets the part of the CodeserverURI that we use for identifying that codeserver in
-- credentials files.
--
-- >>> import Data.Maybe (fromJust)
-- >>> import Network.URI (parseURI)
-- >>> codeserverIdFromURI (CodeserverURI . fromJust $ parseURI "http://localhost:5424/api")
-- >>> codeserverIdFromURI (CodeserverURI . fromJust $ parseURI "https://share.unison-lang.org/api")
-- Right "localhost"
-- Right "share.unison-lang.org"
codeserverIdFromURI :: URI -> Either Text CodeserverId
codeserverIdFromURI uri =
  case uriAuthority uri of
    Nothing -> Left $ "No URI Authority for URI " <> tShow uri
    Just ua -> pure (CodeserverId (Text.pack $ uriRegName ua))

codeserverIdFromCodeserverURI :: CodeserverURI -> CodeserverId
codeserverIdFromCodeserverURI = CodeserverId . Text.pack . uriRegName . codeserverAuthority
