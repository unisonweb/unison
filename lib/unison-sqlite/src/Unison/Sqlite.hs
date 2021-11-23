module Unison.Sqlite
  ( -- * Connection management
    Connection,
    withConnection,

    -- * Type class query interface
    DB,
    runDB,

    -- * Executing queries
    Sql,

    -- ** Without results

    -- *** With parameters
    execute,
    executeMany,

    -- *** Without parameters
    execute_,

    -- ** With results
    -- $query-naming-convention

    -- *** With parameters
    queryList,
    queryListOne,
    queryMaybe,
    queryMaybeOne,
    queryOne,
    queryOneOne,

    -- **** With checks
    queryListCheck,
    queryListOneCheck,
    queryMaybeCheck,
    queryMaybeOneCheck,
    queryOneCheck,
    queryOneOneCheck,

    -- *** Without parameters
    queryList_,
    queryListOne_,
    queryMaybe_,
    queryMaybeOne_,
    queryOne_,
    queryOneOne_,

    -- **** With checks
    queryListCheck_,
    queryListOneCheck_,
    queryMaybeCheck_,
    queryMaybeOneCheck_,
    queryOneCheck_,
    queryOneOneCheck_,

    -- * Data version
    DataVersion (..),
    getDataVersion,

    -- * Journal mode
    JournalMode (..),
    trySetJournalMode,

    -- ** Low-level
    withSavepoint,
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

-- $query-naming-convention
--
-- Queries that return results have many different variants.
--
-- Every function name begins with the string @__query__@.
--
--   1. /Row count/. The caller may expect /exactly one/, /zero or one/, or /zero or more/ rows, in which case the
--      function name includes the string @__List__@, @__Maybe__@, or @__One__@, respectively. Example: @query__List__@.
--
--   2. /Row width/. The caller may expect the returned rows may contain /exactly one/ or /more than one/ column. In the
--      former case, the function name includes the string @__One__@. Example: @queryOne__One__@.
--
--   3. /Result checks/. The caller may want to perform additional validation on the returned rows, in which case the
--      function name includes the string @__Check__@. Example: @queryMaybeOne__Check__@.
--
--   4. /Parameter count/. The query may contain /zero/ or /one or more/ parameters. In the former case, the function
--      name includes the string @__\___@. Example: @queryList__\___@.
--
-- All together, the full anatomy of a query function is:
--
-- @
-- query(List|Maybe|One)[One][Check][_]
-- @
