{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Types where

-- Types common to endpoints --

import Control.Error (fromMaybe)
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as LZ
import Data.Text (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.ShortHash (ShortHash)
import Unison.Util.Pretty (Width)

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

instance ToJSON Name

instance ToJSON ShortHash

instance ToJSON HQ.HashQualified

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
