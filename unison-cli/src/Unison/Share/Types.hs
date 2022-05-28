{-# LANGUAGE DeriveAnyClass #-}

module Unison.Share.Types where

import Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Configurator.Types as Configurator
import Data.Functor.Contravariant (contramap)
import Data.Text
import qualified Data.Text as Text
import Network.URI
import Unison.Prelude

newtype CodeserverURI = CodeserverURI URI
  deriving newtype (Show, Eq, Ord)

instance Configurator.Configured CodeserverURI where
  convert = \case
    Configurator.String txt ->
      CodeserverURI <$> parseURI (Text.unpack txt)
    _ -> Nothing

-- | This is distinct from the codeserver URI in that we store credentials by a normalized ID, since it's
-- much easier to look up that way than from an arbitrary path.
-- We may wish to use explicitly named configurations in the future.
-- This currently uses the 'uriRegName' from a parsed URI as the identifier.
newtype CodeserverId = CodeserverId {codeserverId :: Text}
  deriving newtype (Show, Eq, Ord)

instance ToJSON CodeserverId where
  toJSON (CodeserverId txt) =
    toJSON txt

instance ToJSONKey CodeserverId where
  toJSONKey = contramap codeserverId toJSONKey

instance FromJSONKey CodeserverId where
  fromJSONKey = Aeson.FromJSONKeyText CodeserverId

instance FromJSON CodeserverId where
  parseJSON =
    withText "CodeserverId" $ pure . CodeserverId

-- | Gets the part of the CodeserverURI that we use for identifying that codeserver in
-- credentials files.
--
-- >>> import Data.Maybe (fromJust)
-- >>> import Network.URI (parseURI)
-- >>> codeserverIdFromURI (CodeserverURI . fromJust $ parseURI "http://localhost:5424/api")
-- >>> codeserverIdFromURI (CodeserverURI . fromJust $ parseURI "https://api.share.unison-lang.org")
-- Right "localhost"
-- Right "api.share.unison-lang.org"
codeserverIdFromURI :: CodeserverURI -> Either Text CodeserverId
codeserverIdFromURI (CodeserverURI uri) =
  case uriAuthority uri of
    Nothing -> Left $ "No URI Authority for URI " <> tShow uri
    Just ua -> pure (CodeserverId (Text.pack $ uriRegName ua))
