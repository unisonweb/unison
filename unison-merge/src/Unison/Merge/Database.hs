module Unison.Merge.Database
  ( MergeDatabase (..),
    makeMergeDatabase,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import U.Codebase.Branch (CausalBranch)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reference (Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin qualified as Builtins
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as V1
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Parser.Ann qualified as V1 (Ann)
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol qualified as V1 (Symbol)
import Unison.Term qualified as V1 (Term)
import Unison.Util.Cache qualified as Cache

------------------------------------------------------------------------------------------------------------------------
-- Merge database

-- A mini record-of-functions that contains just the (possibly backed by a cache) database queries used in merge.
data MergeDatabase = MergeDatabase
  { loadCausal :: CausalHash -> Transaction (CausalBranch Transaction),
    loadDeclNumConstructors :: TypeReferenceId -> Transaction Int,
    loadDeclType :: TypeReference -> Transaction ConstructorType,
    loadV1Branch :: CausalHash -> Transaction (V1.Branch Transaction),
    loadV1Decl :: TypeReferenceId -> Transaction (V1.Decl V1.Symbol V1.Ann),
    loadV1Term :: TermReferenceId -> Transaction (V1.Term V1.Symbol V1.Ann)
  }

makeMergeDatabase :: MonadIO m => Codebase IO V1.Symbol V1.Ann -> m MergeDatabase
makeMergeDatabase codebase = liftIO do
  -- Create a bunch of cached database lookup functions
  loadCausal <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache Operations.expectCausalBranchByCausalHash)
  loadDeclNumConstructors <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache Operations.expectDeclNumConstructors)
  let loadV1Branch = Codebase.expectBranchForHash codebase
  loadV1Decl <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache (Codebase.unsafeGetTypeDeclaration codebase))
  -- Since loading a decl type loads the decl and projects out the decl type, just reuse the loadDecl cache
  let loadDeclType ref =
        case ref of
          ReferenceBuiltin name ->
            Map.lookup ref Builtins.builtinConstructorType
              & maybe (error ("Unknown builtin: " ++ Text.unpack name)) pure
          ReferenceDerived refId -> V1.Decl.constructorType <$> loadV1Decl refId
  loadV1Term <- do
    cache <- Cache.semispaceCache 1024
    pure (cacheTransaction cache (Codebase.unsafeGetTerm codebase))
  pure MergeDatabase {loadCausal, loadDeclNumConstructors, loadDeclType, loadV1Branch, loadV1Decl, loadV1Term}

-----------------------------------------------------------------------------------------------------------------------
-- Utilities for caching transaction calls
--
-- These ought to be in a more general-puprose location, but defining here for now

cacheTransaction :: forall k v. Cache.Cache k v -> (k -> Transaction v) -> (k -> Transaction v)
cacheTransaction cache f k =
  unTransactionWithMonadIO (Cache.apply cache (TransactionWithMonadIO . f) k)

newtype TransactionWithMonadIO a
  = TransactionWithMonadIO (Transaction a)
  deriving newtype (Applicative, Functor, Monad)

unTransactionWithMonadIO :: TransactionWithMonadIO a -> Transaction a
unTransactionWithMonadIO (TransactionWithMonadIO m) = m

instance MonadIO TransactionWithMonadIO where
  liftIO :: forall a. IO a -> TransactionWithMonadIO a
  liftIO = coerce @(IO a -> Transaction a) Sqlite.unsafeIO
