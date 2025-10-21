module Unison.Cli.DirectoryUtils
  ( makeMakeTempFilename,
  )
where

import Data.Text qualified as Text
import System.Directory (canonicalizePath, getTemporaryDirectory)
import System.FilePath ((</>))
import System.IO.Temp qualified as Temporary
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import Unison.Prelude

makeMakeTempFilename :: (MonadIO m) => m (Text.Builder -> Text)
makeMakeTempFilename =
  liftIO do
    tmpdir0 <- getTemporaryDirectory
    tmpdir1 <- canonicalizePath tmpdir0
    tmpdir2 <- Temporary.createTempDirectory tmpdir1 "unison"
    pure \filename -> Text.pack (tmpdir2 </> Text.unpack (Text.Builder.run filename))
