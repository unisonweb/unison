module Unison.Sqlite
  ( -- * Connection management
    Connection,
    withConnection,

    -- * Monadic query interface
    DB,
    Transaction,
    runDB,
    runTransaction,

    -- * Executing queries
    Sql,

    -- *** With parameters
    execute,
    executeMany,

    -- *** Without parameters
    execute_,

    -- ** With results

    -- *** With parameters
    queryList,
    queryListOne,
    queryMaybe,
    queryMaybeOne,
    queryOne,
    queryOneOne,

    -- **** With checks
    queryOneCheck,

    -- *** Without parameters
    queryList_,
    queryListOne_,
    queryMaybe_,
    queryMaybeOne_,
    queryOne_,
    queryOneOne_,

    -- **** With checks
    queryOneCheck_,

    -- * Data version
    DataVersion (..),
    getDataVersion,

    -- * Journal mode
    JournalMode (..),
    trySetJournalMode,

    -- ** Low-level
    withSavepoint,
    rollbackTo,
    withStatement,

    -- * Exceptions
    SqliteException (..),
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
  )
where

import Unison.Sqlite.Connection
  ( Connection,
    ExpectedAtMostOneRowException (..),
    ExpectedExactlyOneRowException (..),
    SqliteException (..),
    withConnection,
  )
import Unison.Sqlite.DB
import Unison.Sqlite.DataVersion
import Unison.Sqlite.JournalMode
import Unison.Sqlite.Sql
import Unison.Sqlite.Transaction (Transaction)
