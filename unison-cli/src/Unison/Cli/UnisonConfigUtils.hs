-- | @.unisonConfig@ file utilities
module Unison.Cli.UnisonConfigUtils
  ( defaultMetadataKey,
    gitUrlKey,
    remoteMappingKey,
  )
where

import Data.Sequence (Seq (..))
import qualified Data.Text as Text
import qualified Unison.Codebase.Path as Path
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude

configKey :: Text -> Path.Absolute -> Text
configKey k p =
  Text.intercalate "." . toList $
    k
      :<| fmap
        NameSegment.toText
        (Path.toSeq $ Path.unabsolute p)

defaultMetadataKey :: Path.Absolute -> Text
defaultMetadataKey = configKey "DefaultMetadata"

gitUrlKey :: Path.Absolute -> Text
gitUrlKey = configKey "GitUrl"

remoteMappingKey :: Path.Absolute -> Text
remoteMappingKey = configKey "RemoteMapping"
