{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Types where

-- Types common to endpoints --

import Control.Error (fromMaybe)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as LZ
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import OpenAPI ( ToOpenAPISchema(..) )
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.ShortHash (ShortHash)
import Unison.Util.Pretty (Width)
import Unison.Util.AnnotatedText ( AnnotatedString )

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

instance ToJSON Name
deriving instance ToOpenAPISchema Name

instance ToJSON ShortHash
deriving instance ToOpenAPISchema ShortHash

instance ToJSON HQ.HashQualified
deriving instance ToOpenAPISchema HQ.HashQualified

instance ToJSON a => ToJSON (AnnotatedString a)
instance ToOpenAPISchema a => ToOpenAPISchema (AnnotatedString a)

instance ToOpenAPISchema r => ToOpenAPISchema (Seq r) where
  toSchema _ = toSchema (Proxy @[r])

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.fromStrict

mungeShow :: Show s => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.pack

defaultWidth :: Width
defaultWidth = 80

discard :: Applicative m => a -> m ()
discard = const $ pure ()

mayDefault :: Maybe Width -> Width
mayDefault = fromMaybe defaultWidth
