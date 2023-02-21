-- | @.unisonConfig@ file utilities
module Unison.Cli.UnisonConfigUtils
  ( defaultMetadataKey,
    gitUrlKey,
    remoteMappingKey,
    resolveConfiguredUrl,
  )
where

import Control.Lens
import qualified Data.Foldable.Extra as Foldable
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text as Text
import qualified Text.Megaparsec as P
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output.PushPull (PushPull)
import Unison.Codebase.Editor.RemoteRepo (WriteRemotePath (..))
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import qualified Unison.Codebase.Editor.UriParser as UriParser
import Unison.Codebase.Path (Path' (..))
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

-- Takes a maybe (namespace address triple); returns it as-is if `Just`;
-- otherwise, tries to load a value from .unisonConfig, and complains
-- if needed.
resolveConfiguredUrl :: PushPull -> Path' -> Cli WriteRemotePath
resolveConfiguredUrl pushPull destPath' = do
  destPath <- Cli.resolvePath' destPath'
  whenNothingM (remoteMappingForPath pushPull destPath) do
    let gitUrlConfigKey = gitUrlKey destPath
    -- Fall back to deprecated GitUrl key
    Cli.getConfig gitUrlConfigKey >>= \case
      Just url ->
        (WriteRemotePathGit <$> P.parse UriParser.deprecatedWriteGitRemotePath (Text.unpack gitUrlConfigKey) url) & onLeft \err ->
          Cli.returnEarly (ConfiguredRemoteMappingParseError pushPull destPath url (show err))
      Nothing -> Cli.returnEarly (NoConfiguredRemoteMapping pushPull destPath)

-- | Tries to look up a remote mapping for a given path.
-- Will also resolve paths relative to any mapping which is configured for a parent of that
-- path.
--
-- E.g.
--
-- A config which maps:
--
-- .myshare.foo -> .me.public.foo
--
-- Will resolve the following local paths into share paths like so:
--
-- .myshare.foo -> .me.public.foo
-- .myshare.foo.bar -> .me.public.foo.bar
-- .myshare.foo.bar.baz -> .me.public.foo.bar.baz
-- .myshare -> <Nothing>
remoteMappingForPath :: PushPull -> Path.Absolute -> Cli (Maybe WriteRemotePath)
remoteMappingForPath pushPull dest = do
  pathPrefixes dest & Foldable.firstJustM \(prefix, suffix) -> do
    let remoteMappingConfigKey = remoteMappingKey prefix
    Cli.getConfig remoteMappingConfigKey >>= \case
      Just url -> do
        let parseResult = P.parse UriParser.writeRemotePath (Text.unpack remoteMappingConfigKey) url
         in case parseResult of
              Left err -> Cli.returnEarly (ConfiguredRemoteMappingParseError pushPull dest url (show err))
              Right wrp -> do
                let remote = wrp & RemoteRepo.remotePath_ %~ \p -> Path.resolve p suffix
                 in pure $ Just remote
      Nothing -> pure Nothing
  where
    -- Produces a list of path prefixes and suffixes, from longest prefix to shortest
    --
    -- E.g.
    --
    -- >>> pathPrefixes ("a" :< "b" :< Path.absoluteEmpty)
    -- fromList [(.a.b,),(.a,b),(.,a.b)]
    pathPrefixes :: Path.Absolute -> Seq (Path.Absolute, Path.Path)
    pathPrefixes p =
      Path.unabsolute p
        & Path.toSeq
        & \seq ->
          Seq.zip (Seq.inits seq) (Seq.tails seq)
            & Seq.reverse
            <&> bimap (Path.Absolute . Path.Path) (Path.Path)
