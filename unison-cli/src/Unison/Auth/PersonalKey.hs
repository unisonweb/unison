-- Module for working with "PersonalKeys" in Unison Auth.
--
-- A Personal Key is just an Ed25519 EdDSA key pair.
-- We use the private key to make assertions on behalf of the user,
-- such as signing comments.
--
-- Then we can register the public key with our share user account.
-- to link the key to the user.

module Unison.Auth.PersonalKey
  ( PersonalPrivateKey,
    encodePrivateKey,
    PersonalPublicKey,
    generatePersonalKey,
  )
where

import Crypto.JOSE.JWK (JWK, KeyMaterialGenParam (OKPGenParam), OKPCrv (Ed25519), genJWK)
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Value)
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.Text.Encoding qualified as Text
import Unison.Prelude

-- | A JWK representing a personal key
newtype PersonalPrivateKey = PersonalPrivateKey {_personalPrivateKeyJWK :: JWK}
  deriving stock (Eq)
  deriving newtype (Aeson.FromJSON)

-- | Encode the private JWK.
--
-- I left off a ToJSON instance because I want to be explicit about when
-- we're encoding the private key.
encodePrivateKey :: PersonalPrivateKey -> Value
encodePrivateKey (PersonalPrivateKey jwk) = Aeson.toJSON jwk

_publicKey :: PersonalPrivateKey -> PersonalPublicKey
_publicKey (PersonalPrivateKey jwk) = case (jwk ^. JWK.asPublicKey) of
  Just public -> PersonalPublicKey public
  Nothing -> error "publicKey: Failed to extract public key from private key. This should never happen."

newtype PersonalPublicKey = PersonalPublicKey {_personalPublicKeyJWK :: JWK}
  deriving newtype (ToJSON)

-- Generate a single Ed25519 JWK
generatePersonalKey :: (MonadIO m) => m PersonalPrivateKey
generatePersonalKey = liftIO $ do
  genJWK @IO (OKPGenParam Ed25519)
    <&> JWK.jwkUse .~ Just JWK.Sig
    <&> JWK.jwkAlg .~ Just (JWK.JWSAlg JWS.EdDSA)
    <&> (\j -> j & JWK.jwkKid .~ Just (jwkThumbprint j))
    <&> PersonalPrivateKey
  where
    jwkThumbprint :: JWK.JWK -> Text
    jwkThumbprint jwk =
      jwk ^. JWK.thumbprint @JWK.SHA256
        & ByteArray.unpack
        & BS.pack
        & Base64URL.encodeUnpadded
        & Text.decodeUtf8
