module Unison.Codebase.SqliteCodebase.Migrations.Helpers where

import qualified Unison.Sqlite as Sqlite

abortMigration :: String -> Sqlite.Transaction a
abortMigration msg = do
  error $
    unlines
      [ "⚠️ " <> msg,
        "",
        "An unrecoverable error occurred, the migration has been aborted.",
        "Please report this bug to https://github.com/unisonweb/unison/issues and include your migration output.",
        "Downgrading to the previous UCM version will allow you to continue using your codebase while we investigate your issue."
      ]
