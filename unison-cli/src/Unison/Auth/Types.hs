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
    CredentialFailure (..),
    getActiveTokens,
    setActiveTokens,
    emptyCredentials,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import Network.URI
import qualified Network.URI as URI
import Unison.Prelude
import Unison.Share.Types (CodeserverId, CodeserverURI)

defaultProfileName :: ProfileName
defaultProfileName = "default"

data CredentialFailure
  = ReauthRequired CodeserverId
  | CredentialParseFailure FilePath Text
  | InvalidDiscoveryDocument URI Text
  | InvalidJWT Text
  | RefreshFailure Text
  | InvalidTokenResponse URI Text
  | InvalidHost CodeserverURI
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
  deriving (Show)

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

type ProfileName = Text

data Credentials = Credentials
  { credentials :: Map ProfileName (Map CodeserverId Tokens),
    activeProfile :: ProfileName
  }
  deriving (Eq)

emptyCredentials :: Credentials
emptyCredentials = Credentials mempty defaultProfileName

getActiveTokens :: CodeserverId -> Credentials -> Either CredentialFailure Tokens
getActiveTokens host (Credentials {credentials, activeProfile}) =
  maybeToEither (ReauthRequired host) $
    credentials ^? ix activeProfile . ix host

setActiveTokens :: CodeserverId -> Tokens -> Credentials -> Credentials
setActiveTokens host tokens creds@(Credentials {credentials, activeProfile}) =
  let newCredMap =
        credentials
          & at activeProfile . non Map.empty . at host .~ Just tokens
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
