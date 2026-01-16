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
    publicKey,
    generatePersonalKey,
    personalKeyThumbprint,
    signWithPersonalKey,
    verifyWithPersonalKey,
    PersonalKeySignature (..),
  )
where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Crypto.JOSE qualified as JOSE
import Crypto.JOSE.JWA.JWK qualified as JWA
import Crypto.JOSE.JWK (JWK, KeyMaterialGenParam (OKPGenParam), OKPCrv (Ed25519), genJWK)
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Crypto.Random
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Value)
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.Text.Encoding qualified as Text
import Unison.KeyThumbprint (KeyThumbprint (..))
import Unison.Prelude

-- | A JWK representing a personal key
newtype PersonalPrivateKey = PersonalPrivateKey {_personalPrivateKeyJWK :: JWK}
  deriving stock (Eq)
  deriving newtype (Aeson.FromJSON)

personalKeyThumbprint :: PersonalPrivateKey -> KeyThumbprint
personalKeyThumbprint (PersonalPrivateKey jwk) = jwkThumbprint jwk

jwkThumbprint :: JWK.JWK -> KeyThumbprint
jwkThumbprint jwk =
  jwk ^. JWK.thumbprint @JWK.SHA256
    & ByteArray.convert
    & Base64URL.encodeUnpadded
    & Text.decodeUtf8
    & KeyThumbprint

-- | Encode the private JWK.
--
-- I left off a ToJSON instance because I want to be explicit about when
-- we're encoding the private key.
encodePrivateKey :: PersonalPrivateKey -> Value
encodePrivateKey (PersonalPrivateKey jwk) = Aeson.toJSON jwk

publicKey :: PersonalPrivateKey -> PersonalPublicKey
publicKey (PersonalPrivateKey jwk) = case (jwk ^. JWK.asPublicKey) of
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
    <&> (\j -> j & JWK.jwkKid .~ Just (thumbprintToText $ jwkThumbprint j))
    <&> PersonalPrivateKey

newtype PersonalKeySignature = PersonalKeySignature {unPersonalKeySignature :: ByteString}
  deriving (Show, Eq)

-- | For some reason `sign` and `verify` require a single monad which implements both MonadRandom and MonadError,
-- but ExceptT doesn't implement MonadRandom :|
newtype SignM a = SignM {_unSignM :: ExceptT JOSE.Error IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError JOSE.Error)

runSignM :: (MonadIO m) => SignM a -> m (Either JOSE.Error a)
runSignM (SignM e) = liftIO $ runExceptT e

instance MonadRandom SignM where
  getRandomBytes n = SignM . liftIO $ getRandomBytes n

-- | Sign arbitrary bytes using a personal private key
--
-- >>> key <- generatePersonalKey
-- >>> let msg = "Hello, world!"
-- >>> signature <- fromRight (error "failed to sign") <$> signWithPersonalKey key msg
-- >>> verifyWithPersonalKey (publicKey key) msg signature
-- True
signWithPersonalKey :: (MonadIO m) => PersonalPrivateKey -> BS.ByteString -> m (Either JOSE.Error PersonalKeySignature)
signWithPersonalKey (PersonalPrivateKey jwk) bytes = runSignM $ do
  PersonalKeySignature <$> (JWA.sign @SignM JWS.EdDSA (jwk ^. JWS.jwkMaterial) bytes)

-- | Verify a signature made with a personal private key
verifyWithPersonalKey :: (MonadIO m) => PersonalPublicKey -> BS.ByteString -> PersonalKeySignature -> m Bool
verifyWithPersonalKey (PersonalPublicKey jwk) bytes (PersonalKeySignature signature) =
  (JWA.verify @JOSE.Error @SignM JWS.EdDSA (jwk ^. JWS.jwkMaterial) bytes signature)
    & runSignM
    <&> fromRight False
