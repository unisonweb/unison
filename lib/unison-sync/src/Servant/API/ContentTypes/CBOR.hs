module Servant.API.ContentTypes.CBOR where

import Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Network.HTTP.Media as M
import Servant.API
import Unison.Prelude

data CBOR

instance Accept CBOR where
  -- Not technically an accepted MIME type yet
  contentType _proxy = "application" M.// "cbor"

instance Serialise a => MimeRender CBOR a where
  mimeRender _proxy = Serialise.serialise

instance Serialise a => MimeUnrender CBOR a where
  mimeUnrender _proxy = mapLeft show . Serialise.deserialiseOrFail
