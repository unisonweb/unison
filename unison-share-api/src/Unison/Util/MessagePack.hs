{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Util.MessagePack where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Validate
import Data.Generics.Sum
import qualified Data.List as List
import Data.MessagePack
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as Text
import qualified Network.HTTP.Media.MediaType as MediaType
import Servant
import Unison.Prelude

-- | Message Pack Content Type
data MessagePackCT

instance Accept MessagePackCT where
  contentType _ct = "application" MediaType.// "msgpack"

instance MessagePack a => MimeRender MessagePackCT a where
  mimeRender _ct = MsgPack.pack

instance MessagePack a => MimeUnrender MessagePackCT a where
  mimeUnrender _ct =
    mapLeft (List.intercalate ", " . MsgPack.errorMessages) . MsgPack.unpackEither

atKey :: (MessagePack a, MonadReader Config m, MonadValidate DecodeError m) => Text -> MsgPack.Object -> m a
atKey key obj = do
  config <- ask
  v <- orFail (fromString $ "Expected key " <> Text.unpack key) $ lookupOf (_Ctor @"ObjectMap" . traversed) (MsgPack.ObjectStr key) obj
  fromObjectWith config v

orFail :: MonadValidate e m => e -> Maybe a -> m a
orFail msg Nothing = refute msg
orFail _msg (Just a) = pure a
