module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema5To6 (migrateSchema5To6) where

import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import System.FilePath ((</>))
import U.Codebase.HashTags (CausalHash (CausalHash))
import qualified U.Codebase.Reflog as Reflog
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase (CodebasePath)
import qualified Unison.Hash as Hash
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import UnliftIO (catchIO)

-- | The 5 to 6 migration adds the reflog as a table in the DB
migrateSchema5To6 :: CodebasePath -> Sqlite.Transaction ()
migrateSchema5To6 codebasePath = do
  Q.expectSchemaVersion 5
  Q.addReflogTable
  migrateCurrentReflog codebasePath
  Q.setSchemaVersion 6

migrateCurrentReflog :: CodebasePath -> Sqlite.Transaction ()
migrateCurrentReflog codebasePath = do
  now <- Sqlite.unsafeIO $ getCurrentTime
  oldEntries <- Sqlite.unsafeIO $ oldReflogEntries codebasePath now
  for_ oldEntries \oldEntry -> do
    -- There's no guarantee these causals actually exist in the DB,
    -- so we check first to avoid triggering a bad foreign key constraint.
    haveFrom <- isJust <$> Q.loadCausalByCausalHash (Reflog.fromRootCausalHash oldEntry)
    haveTo <- isJust <$> Q.loadCausalByCausalHash (Reflog.toRootCausalHash oldEntry)
    when (haveFrom && haveTo) $ Ops.appendReflog oldEntry

oldReflogEntries :: CodebasePath -> UTCTime -> IO [Reflog.Entry CausalHash Text]
oldReflogEntries codebasePath now =
  ( do
      contents <- readUtf8 reflogPath
      let lines = Text.lines contents
      let entries = mapMaybe parseEntry (zip [0 ..] $ reverse lines)
      pure entries
  )
    `catchIO` const (pure [])
  where
    reflogPath :: FilePath
    reflogPath = codebasePath </> "reflog"

    parseEntry :: (Integer, Text) -> Maybe (Reflog.Entry CausalHash Text)
    parseEntry (n, txt) =
      -- We offset existing entries by a number of seconds corresponding to their position in
      -- the current file; we can't reclaim timestamps for old reflog entries, but this at
      -- least puts them in the correct order chronologically.
      let offsetTime = addUTCTime (fromInteger @NominalDiffTime n) now
       in case Text.words txt of
            (Hash.fromBase32Hex -> Just old) : (Hash.fromBase32Hex -> Just new) : (Text.unwords -> reason) ->
              Just $
                Reflog.Entry
                  { time = offsetTime,
                    fromRootCausalHash = CausalHash old,
                    toRootCausalHash = CausalHash new,
                    reason
                  }
            _ -> Nothing
