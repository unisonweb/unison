{-# LANGUAGE RecordWildCards #-}

-- | Hash-related types in the Share API.
module Unison.Share.API.Hash
  ( -- * Hash types
    HashJWT (..),
    hashJWTHash,
    HashJWTClaims (..),
    DecodedHashJWT (..),
    decodeHashJWT,
    decodeHashJWTClaims,
    decodedHashJWTHash,
  )
where

import Control.Lens (folding, ix, (^?))
import qualified Crypto.JWT as Jose
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Servant.Auth.JWT as Servant.Auth
import Unison.Hash32 (Hash32)
import Unison.Hash32.Orphans.Aeson ()
import Unison.Prelude
import qualified Web.JWT as JWT

newtype HashJWT = HashJWT {unHashJWT :: Text}
  deriving newtype (Show, Eq, Ord, ToJSON, FromJSON)

-- | Grab the hash out of a hash JWT.
--
-- This decodes the whole JWT, then throws away the claims; use it if you really only need the hash!
hashJWTHash :: HashJWT -> Hash32
hashJWTHash =
  decodedHashJWTHash . decodeHashJWT

data HashJWTClaims = HashJWTClaims
  { hash :: Hash32,
    userId :: Maybe Text
  }
  deriving stock (Show, Eq, Ord)

-- | Adding a type tag to the jwt prevents users from using jwts we issue for other things
-- in this spot. All of our jwts should have a type parameter of some kind.
hashJWTType :: String
hashJWTType = "hj"

instance Servant.Auth.ToJWT HashJWTClaims where
  encodeJWT (HashJWTClaims h u) =
    Jose.emptyClaimsSet
      & Jose.addClaim "h" (toJSON h)
      & Jose.addClaim "u" (toJSON u)
      & Jose.addClaim "t" (toJSON hashJWTType)

instance Servant.Auth.FromJWT HashJWTClaims where
  decodeJWT claims = maybe (Left "Invalid HashJWTClaims") pure $ do
    hash <- claims ^? Jose.unregisteredClaims . ix "h" . folding fromJSON
    userId <- claims ^? Jose.unregisteredClaims . ix "u" . folding fromJSON
    case claims ^? Jose.unregisteredClaims . ix "t" . folding fromJSON of
      Just t | t == hashJWTType -> pure ()
      _ -> empty
    pure HashJWTClaims {..}

instance ToJSON HashJWTClaims where
  toJSON (HashJWTClaims hash userId) =
    object
      [ "h" .= hash,
        "u" .= userId
      ]

instance FromJSON HashJWTClaims where
  parseJSON = Aeson.withObject "HashJWTClaims" \obj -> do
    hash <- obj .: "h"
    userId <- obj .: "u"
    pure HashJWTClaims {..}

-- | A decoded hash JWT that retains the original encoded JWT.
data DecodedHashJWT = DecodedHashJWT
  { claims :: HashJWTClaims,
    hashJWT :: HashJWT
  }
  deriving (Eq, Ord, Show)

-- | Decode a hash JWT.
decodeHashJWT :: HashJWT -> DecodedHashJWT
decodeHashJWT hashJWT =
  DecodedHashJWT
    { claims = decodeHashJWTClaims hashJWT,
      hashJWT
    }

-- | Decode the claims out of a hash JWT.
decodeHashJWTClaims :: HashJWT -> HashJWTClaims
decodeHashJWTClaims (HashJWT text) =
  case JWT.decode text of
    Nothing -> error "bad JWT"
    Just jwt ->
      let object =
            jwt
              & JWT.claims
              & JWT.unregisteredClaims
              & JWT.unClaimsMap
              & Map.toList
              & HashMap.fromList
              & Aeson.Object
       in case Aeson.fromJSON object of
            Aeson.Error err -> error ("bad JWT: " ++ err)
            Aeson.Success claims -> claims

-- | Grab the hash out of a decoded hash JWT.
decodedHashJWTHash :: DecodedHashJWT -> Hash32
decodedHashJWTHash DecodedHashJWT {claims = HashJWTClaims {hash}} =
  hash
