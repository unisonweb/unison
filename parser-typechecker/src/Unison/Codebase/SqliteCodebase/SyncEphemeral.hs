{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.SqliteCodebase.SyncEphemeral where

import Control.Monad.Except (ExceptT, throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.Extra (ifM)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import qualified Data.Either.Extra as Either
import qualified Data.Map as Map
import Data.Set (Set)
import U.Codebase.HashTags (CausalHash (CausalHash))
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Sync22 as Sync22
import qualified U.Codebase.Sync as Sync
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch (Branch), Branch0)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Codebase.SqliteCodebase as SqliteCodebase
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Hash (Hash)
import Unison.Prelude (MonadIO)
import qualified Data.Validation as Validation

data Dependencies = Dependencies
  { definitions :: Set Hash,
    branches :: Set Hash
  }

data Error
  = Sync22Error Sync22.Error
  | SrcMissingSchema [(Q.SchemaType, Q.SchemaName)]
  | DestMissingSchema [(Q.SchemaType, Q.SchemaName)]
  | DisappearingBranch CausalHash
  deriving (Show)

-- does destPath need to be a codebase?
syncToDirectory ::
  forall m.
  MonadIO m =>
  CodebasePath ->
  CodebasePath ->
  SyncMode ->
  Branch m ->
  Sync.Progress m Sync22.Entity ->
  m ()
syncToDirectory srcPath destPath _mode newRoot progress = do
  result <- runExceptT do
    syncEnv@(Sync22.Env srcConn _ _) <-
      Sync22.Env
        <$> SqliteCodebase.unsafeGetConnection srcPath
        <*> SqliteCodebase.unsafeGetConnection destPath
        <*> pure (16 * 1024 * 1024)
    (closeSrc, src) <-
      lift (SqliteCodebase.sqliteCodebase srcPath)
        >>= Except.liftEither . Either.mapLeft SrcMissingSchema
    (closeDest, dest) <-
      lift (SqliteCodebase.sqliteCodebase destPath)
        >>= Except.liftEither . Either.mapLeft DestMissingSchema
    -- we want to use sync22 wherever possible
    -- so for each branch, we'll check if it exists in the destination branch
    -- or if it exists in the source branch, then we can sync22 it
    -- oh god but we have to figure out the dbid
    -- if it doesn't exist in the dest or source branch,
    -- then just use putBranch to the dest
    let branchDeps :: forall m. Applicative m => Branch0 m -> [(Branch.Hash, m (Branch m))]
        branchDeps =
          map (\b -> (Branch.headHash b, pure b))
            . Map.elems
            . Branch._children
        causalDeps :: forall m. Applicative m => Branch m -> [(Branch.Hash, m (Branch m))]
        causalDeps (Branch c) = case c of
          Causal.One _h b -> branchDeps b
          Causal.Cons _h b tail -> processTails [tail] b
          Causal.Merge _h b tails -> processTails (Map.toList tails) b
          where
            processTails tails b =
              let tails' = fmap (\(ht, mt) -> (ht, fmap Branch mt)) tails
                  deps = branchDeps b
               in tails' ++ deps
    let se :: forall m a. Functor m => (ExceptT Sync22.Error m a -> ExceptT Error m a)
        se = withExceptT Sync22Error
    let r :: forall m a. (ReaderT Sync22.Env m a -> m a)
        r = flip runReaderT syncEnv
        processBranches ::
          forall m v a.
          MonadIO m =>
          Sync.Sync (ReaderT Sync22.Env (ExceptT Sync22.Error m)) Sync22.Entity ->
          Sync.Progress (ReaderT Sync22.Env (ExceptT Sync22.Error m)) Sync22.Entity ->
          Codebase m v a ->
          Codebase m v a ->
          [(Branch.Hash, m (Branch m))] ->
          ExceptT Error m ()
        processBranches _ _ _ _ [] = pure ()
        processBranches sync progress src dest ((h, mb) : rest) = do
          ifM @(ExceptT Error m)
            (lift $ Codebase.branchExists dest h)
            (processBranches sync progress src dest rest)
            ( ifM
                (lift $ Codebase.branchExists src h)
                ( let h2 = CausalHash . Cv.hash1to2 $ Causal.unRawHash h
                   in lift (flip runReaderT srcConn (Q.loadCausalHashIdByCausalHash h2))
                        >>= \case
                          Nothing -> throwError $ DisappearingBranch h2
                          Just chId -> se . r $ Sync.sync sync progress [Sync22.C chId]
                )
                ( lift mb >>= \b -> do
                    let deps = causalDeps b
                    if (null deps)
                      then lift $ Codebase.putBranch dest b
                      else processBranches @m sync progress src dest (deps ++ (h, mb) : rest)
                )
            )
    sync <- se . r $ Sync22.sync22
    let progress' = Sync.transformProgress (lift . lift) progress
    processBranches sync progress' src dest [(Branch.headHash newRoot, pure newRoot)]
    lift closeSrc
    lift closeDest
  pure $ Validation.valueOr (error . show) result