{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.SqliteCodebase.Migrations.DefinitionHashCheck where

import Data.Bitraversable
import U.Codebase.HashTags
import U.Codebase.Sqlite.Decode (decodeTermFormat)
import U.Codebase.Sqlite.HashHandle (HashHandle (..))
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Term.Format qualified as S.Term
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Hash32 qualified as Hash32
import Unison.Prelude
import Unison.Sqlite qualified as Sqlite

definitionHashCheck :: Sqlite.Transaction ()
definitionHashCheck = do
  termOIDs <- Sqlite.queryListCol [Sqlite.sql|SELECT id FROM object WHERE type_id = 0|]
  results <- for termOIDs \oid -> do
    componentHash <- Q.expectPrimaryHashByObjectId $ oid
    S.Term.Term lic <- fromMaybe (error $ "Failed to load term object: " <> show oid) <$> Q.loadTermObject oid decodeTermFormat
    lic' <- bitraverse Q.expectText (fmap Hash32.fromHash . Q.expectPrimaryHashByObjectId) lic
    let success = verifyTermFormatHash v2HashHandle (ComponentHash componentHash) (S.Term.Term lic')
    when (not success) $ do
      Sqlite.unsafeIO . print $ "Definition hash check failed for " <> show (oid, componentHash)
    pure success
  if (and results)
    then Sqlite.unsafeIO $ putStrLn "Definition hash check passed"
    else Sqlite.unsafeIO $ putStrLn "Definition hash check failed"
