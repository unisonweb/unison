-- | Resolving the active surface syntax ("dialect") for the current UCM session.
--
-- Resolution order (first match wins):
--
--   1. the @UNISON_SYNTAX@ environment variable (a quick toggle for experiments/demos);
--   2. the @syntax.dialect@ config value stored in the codebase (the durable per-codebase setting);
--   3. otherwise the default Haskell-like dialect.
--
-- The config value is read fresh on each call (it is a single SQLite read inside the transaction we open anyway) so
-- that @config.set syntax.dialect ...@ takes effect mid-session without a restart. An unknown name from either source
-- silently falls back to the default.
module Unison.Cli.Dialect
  ( getActiveDialect,
    getActivePrintDialect,
  )
where

import Data.Text qualified as Text
import System.Environment (lookupEnv)
import U.Codebase.Config qualified as Config
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Prelude
import Unison.Syntax.Dialect (Dialect, PrintDialect)
import Unison.Syntax.Dialect qualified as Dialect

-- | Resolve the active 'Dialect' (see module header for resolution order).
getActiveDialect :: Cli Dialect
getActiveDialect = do
  mOverride <- liftIO (lookupEnv "UNISON_SYNTAX")
  mName <- case mOverride of
    Just s -> pure (Just (Text.pack s))
    Nothing -> Cli.runTransaction (Queries.getConfigValue Config.SyntaxDialectKey)
  pure (fromMaybe Dialect.defaultDialect (mName >>= Dialect.dialectByName))

-- | Resolve the print half of the active 'Dialect'.
getActivePrintDialect :: Cli PrintDialect
getActivePrintDialect = Dialect.printDialect <$> getActiveDialect
