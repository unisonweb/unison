{-# LANGUAGE ScopedTypeVariables #-}

module U.Util.Process (($^), ($$), ($|), ($?)) where

import Data.Text (Text)
import qualified Data.Text as Text
import UnliftIO (MonadIO (liftIO))
import qualified UnliftIO as UnliftIO
import qualified UnliftIO.Process as UnliftIO

-- mimic the Shellmet API for now; we change it later

-- | This operator runs shell comand with given options after printing the
-- command itself.
-- >>> "echo" $! ["Foo", "Bar"]
-- ⚙  echo Foo Bar
-- Foo Bar
infix 5 $$
($$) :: MonadIO m => FilePath -> [Text] -> m ()
cmd $$ args = do
  let cmdStr = UnliftIO.showCommandForUser cmd (map Text.unpack args)
  liftIO $ putStrLn $ "⚙  " ++ cmdStr
  cmd $^ args

-- | This operator runs shell command with given options but doesn't print the
-- command itself.
-- >>> "echo" $^ ["Foo", "Bar"]
-- Foo Bar
infix 5 $^

($^) :: MonadIO m => FilePath -> [Text] -> m ()
cmd $^ args = UnliftIO.callProcess cmd (map Text.unpack args)

-- | Run shell command with given options and return stripped stdout of the
-- executed command.
-- >>> "echo" $| ["Foo", "Bar"]
-- "Foo Bar"
infix 5 $|

($|) :: MonadIO m => FilePath -> [Text] -> m Text
cmd $| args = post <$> UnliftIO.readProcess cmd (map Text.unpack args) stdin
  where
    stdin = ""
    post = Text.strip . Text.pack

-- | Do some IO actions when process failed with 'IOError'.
-- >>> "exit" ["0"] $? putStrLn "Command failed"
-- ⚙  exit 0
-- >>> "exit" ["1"] $? putStrLn "Command failed"
-- ⚙  exit 1
-- Command failed
infixl 4 $?

($?) :: IO a -> IO a -> IO a
action $? handler = action `UnliftIO.catch` \(_ :: IOError) -> handler
{-# INLINE ($?) #-}
