{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema5To6 (migrateSchema5To6) where

import Data.Bitraversable
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import System.FilePath ((</>))
import U.Codebase.HashTags (CausalHash (CausalHash))
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (CodebasePath)
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite
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
  oldEntries <- Sqlite.unsafeIO $ oldReflogEntries reflogPath now
  for_ oldEntries \oldEntry -> do
    -- There's no guarantee these causals actually exist in the DB,
    -- so we check first to avoid triggering a bad foreign key constraint.
    haveFrom <- isJust <$> Q.loadCausalByCausalHash (Reflog.fromRootCausalHash oldEntry)
    haveTo <- isJust <$> Q.loadCausalByCausalHash (Reflog.toRootCausalHash oldEntry)
    when (haveFrom && haveTo) $ appendReflog oldEntry
  Sqlite.unsafeIO . putStrLn $ "I migrated old reflog entries from " <> reflogPath <> " into the codebase; you may delete that file now if you like."
  where
    reflogPath :: FilePath
    reflogPath = codebasePath </> "reflog"

    appendReflog :: Reflog.Entry CausalHash Text -> Sqlite.Transaction ()
    appendReflog entry = do
      dbEntry <- (bitraverse Q.saveCausalHash pure) entry
      Sqlite.execute
        [Sqlite.sql|
          INSERT INTO reflog (time, from_root_causal_id, to_root_causal_id, reason)
          VALUES (@dbEntry, @, @, @)
        |]

oldReflogEntries :: CodebasePath -> UTCTime -> IO [Reflog.Entry CausalHash Text]
oldReflogEntries reflogPath now =
  ( do
      contents <- readUtf8 reflogPath
      let lines = Text.lines contents
      let entries = mapMaybe parseEntry (zip [0 ..] $ reverse lines)
      pure entries
  )
    `catchIO` const (pure [])
  where
    parseEntry :: (Integer, Text) -> Maybe (Reflog.Entry CausalHash Text)
    parseEntry (n, txt) =
      -- We offset existing entries by a number of seconds corresponding to their position in
      -- the current file; we can't reclaim timestamps for old reflog entries, but this at
      -- least puts them in the correct order chronologically.
      let offsetTime = addUTCTime (negate $ fromInteger @NominalDiffTime n) now
       in case Text.words txt of
            (Hash.fromBase32HexText -> Just old) : (Hash.fromBase32HexText -> Just new) : (Text.unwords -> reason) ->
              Just $
                Reflog.Entry
                  { time = offsetTime,
                    fromRootCausalHash = CausalHash old,
                    toRootCausalHash = CausalHash new,
                    reason
                  }
            _ -> Nothing
