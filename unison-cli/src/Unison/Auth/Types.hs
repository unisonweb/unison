{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.Auth.Types
  ( DiscoveryDoc (..),
    Tokens (..),
    Credentials (..),
    Code,
    AccessToken,
    RefreshToken,
    IDToken,
    OAuthState,
    PKCEVerifier,
    PKCEChallenge,
    ProfileName,
    Audience (..),
    CredentialFailure (..),
    getActiveTokens,
    setActiveTokens,
    emptyCredentials,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Either.Extra (maybeToEither)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import Network.URI
import qualified Network.URI as URI
import Unison.Prelude

data CredentialFailure
  = ReauthRequired Audience
  | CredentialParseFailure FilePath Text
  | InvalidDiscoveryDocument URI Text
  | InvalidJWT Text
  | RefreshFailure Text
  | InvalidTokenResponse URI Text
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

type Code = Text

type OAuthState = ByteString

type PKCEVerifier = ByteString

type PKCEChallenge = ByteString

type AccessToken = Text

type RefreshToken = Text

type IDToken = Text

type TokenType = Text

newtype Scopes = Scopes [Text]
  deriving stock (Show, Eq, Ord)

instance ToJSON Scopes where
  toJSON (Scopes scopes) = Aeson.String $ Text.unwords scopes

instance FromJSON Scopes where
  parseJSON = Aeson.withText "Scopes" $ \txt -> do
    pure . Scopes $ Text.words txt

data DiscoveryDoc = DiscoveryDoc
  { issuer :: URI,
    authorizationEndpoint :: URI,
    tokenEndpoint :: URI,
    userInfoEndpoint :: URI
  }

data Tokens = Tokens
  { accessToken :: AccessToken,
    idToken :: Maybe IDToken,
    refreshToken :: Maybe RefreshToken,
    tokenType :: TokenType,
    expiresIn :: NominalDiffTime,
    scopes :: Scopes
  }
  deriving (Eq, Show)

instance Aeson.FromJSON Tokens where
  parseJSON =
    Aeson.withObject "Tokens" $ \obj -> do
      accessToken <- obj .: "access_token"
      idToken <- obj .:? "id_token"
      refreshToken <- obj .: "refresh_token"
      tokenType <- obj .: "token_type"
      expiresIn <- obj .: "expires_in"
      scopes <- obj .: "scope"
      pure (Tokens {..})

instance Aeson.ToJSON Tokens where
  toJSON (Tokens accessToken idToken refreshToken tokenType expiresIn scopes) =
    Aeson.object
      [ "access_token" .= accessToken,
        "id_token" .= idToken,
        "refresh_token" .= refreshToken,
        "token_type" .= tokenType,
        "expires_in" .= expiresIn,
        "scope" .= scopes
      ]

newtype URIParam = URIParam URI

instance Aeson.FromJSON URIParam where
  parseJSON = Aeson.withText "URI" $ \txt ->
    maybe (fail "Invalid URI") (pure . URIParam) $ URI.parseURI (Text.unpack txt)

instance Aeson.FromJSON DiscoveryDoc where
  parseJSON = Aeson.withObject "Discovery Document" $ \obj -> do
    URIParam issuer <- obj .: "issuer"
    URIParam authorizationEndpoint <- obj .: "authorization_endpoint"
    URIParam tokenEndpoint <- obj .: "token_endpoint"
    URIParam userInfoEndpoint <- obj .: "userinfo_endpoint"
    pure (DiscoveryDoc {..})

-- | Selector for which remote the credentials are for.
-- Just Share for now, but it's possible people will eventually spin up their own
-- servers, or that we'll provide private clouds or enterprise instances for customers,
-- this allows us to store multiple auth credentials at once.
data Audience = Share
  deriving anyclass (Aeson.ToJSONKey, Aeson.FromJSONKey)
  deriving stock (Eq, Ord, Show)

instance Aeson.ToJSON Audience where
  toJSON Share = Aeson.String "share"

instance Aeson.FromJSON Audience where
  parseJSON = Aeson.withText "Audience" \case
    "share" -> pure Share
    _ -> fail "Unknown audience"

type ProfileName = Text

data Credentials = Credentials
  { credentials :: Map ProfileName (Map Audience Tokens),
    activeProfile :: ProfileName
  }
  deriving (Eq)

emptyCredentials :: Credentials
emptyCredentials = Credentials mempty mempty

getActiveTokens :: Audience -> Credentials -> Either CredentialFailure Tokens
getActiveTokens aud (Credentials {credentials, activeProfile}) =
  maybeToEither (ReauthRequired aud) $
    credentials ^? ix activeProfile . ix aud

setActiveTokens :: Audience -> Tokens -> Credentials -> Credentials
setActiveTokens aud tokens creds@(Credentials {credentials, activeProfile}) =
  let newCredMap =
        credentials
          & at activeProfile . non Map.empty . at aud .~ Just tokens
   in creds {credentials = newCredMap}

instance Aeson.ToJSON Credentials where
  toJSON (Credentials credMap activeProfile) =
    Aeson.object
      [ "credentials" .= credMap,
        "active_profile" .= activeProfile
      ]

instance Aeson.FromJSON Credentials where
  parseJSON = Aeson.withObject "Credentials" $ \obj -> do
    credentials <- obj .: "credentials"
    activeProfile <- obj .: "active_profile"
    pure Credentials {..}
