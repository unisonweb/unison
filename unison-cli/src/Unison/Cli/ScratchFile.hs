-- | Scratch files for the actions that must produce a branch that compiles (@update@, @delete@, @merge@, @pull@ and
-- @upgrade@). When one of them can't, it leaves the user on a temporary branch, with a scratch file of the definitions
-- that need fixing. That file gets its own name, like @merge-bob-into-alice-a0458b71.u@, rather than reusing
-- @scratch.u@, so that it is unmistakable which file goes with which action and branch.
module Unison.Cli.ScratchFile
  ( scratchFileName,
    writeActionScratchFile,
  )
where

import Control.Lens ((?=))
import Control.Monad.Reader (ask)
import Crypto.Hash qualified as Crypto
import Data.ByteArray.Encoding qualified as BE
import Data.Char (isAlphaNum, isAscii)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.FilePath (replaceFileName)
import Unison.Cli.Monad (Cli, Env (..))
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Prelude

-- | @scratchFileName action words parts@ is @\<action\>-\<words\>-\<shorthash\>.u@. The @words@ say what the action is
-- about, like the branches involved (they are made safe for a file name, and shortened). The short hash (8 characters)
-- is of the action, the @words@ and the @parts@: the arguments, and the hashes of the branches involved. The same
-- action on the same branches therefore always gets the same file name, and a different one gets a different file name.
scratchFileName :: Text -> [Text] -> [Text] -> Text
scratchFileName action words_ parts =
  Text.intercalate "-" (action : map safe words_ ++ [Text.take 8 shortHash]) <> ".u"
  where
    safe :: Text -> Text
    safe =
      Text.take 30 . Text.map \c -> if isAscii c && (isAlphaNum c || c `elem` ("._-" :: String)) then c else '-'

    shortHash :: Text
    shortHash =
      Text.intercalate "\0" (action : words_ ++ parts)
        & Text.encodeUtf8
        & Crypto.hashWith Crypto.SHA256
        & BE.convertToBase BE.Base16
        & (Text.decodeUtf8 :: ByteString -> Text)

-- | Write @contents@ to the scratch file for @action@ (see 'scratchFileName'), in the directory of the latest file (or
-- the current directory), and make it the latest file. Returns the file's path.
writeActionScratchFile :: Text -> [Text] -> [Text] -> Text -> Cli FilePath
writeActionScratchFile action words_ parts contents = do
  env <- ask
  path <-
    Cli.getLatestFile <&> \case
      Nothing -> Text.unpack name
      Just (latestFile, _) -> replaceFileName latestFile (Text.unpack name)
  #latestFile ?= (path, True)
  liftIO
    if env.unattended
      then writeUtf8 path contents
      else env.writeSource (Text.pack path) contents True
  pure path
  where
    name = scratchFileName action words_ parts
