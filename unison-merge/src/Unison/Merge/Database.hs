module Unison.Merge.Database
  ( MergeDatabase (..),
    referent2to1,
    makeMergeDatabase,
  )
where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import U.Codebase.Branch (CausalBranch)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reference (Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Builtin qualified as Builtins
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations (expectDeclComponent)
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType (ConstructorType)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.DataDeclaration qualified as V1.Decl
import Unison.Hash (Hash)
import Unison.Parser.Ann qualified as V1 (Ann)
import Unison.Prelude
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol qualified as V1 (Symbol)
import Unison.Term qualified as V1 (Term)
import Unison.Type qualified as V1 (Type)
import Unison.Util.Cache qualified as Cache

------------------------------------------------------------------------------------------------------------------------
-- Merge database

-- A mini record-of-functions that contains just the (possibly backed by a cache) database queries used in merge.
data MergeDatabase = MergeDatabase
  { loadCausal :: CausalHash -> Transaction (CausalBranch Transaction),
    loadDeclNumConstructors :: TypeReferenceId -> Transaction Int,
    loadDeclType :: TypeReference -> Transaction ConstructorType,
    loadV1Decl :: TypeReferenceId -> Transaction (V1.Decl V1.Symbol V1.Ann),
    loadV1DeclComponent :: Hash -> Transaction [V1.Decl V1.Symbol V1.Ann],
    loadV1Term :: TermReferenceId -> Transaction (V1.Term V1.Symbol V1.Ann),
    loadV1TermComponent :: Hash -> Transaction [(V1.Term V1.Symbol V1.Ann, V1.Type V1.Symbol V1.Ann)]
  }

makeMergeDatabase :: (MonadIO m) => Codebase IO V1.Symbol V1.Ann -> m MergeDatabase
makeMergeDatabase codebase = liftIO do
  -- Create a bunch of cached database lookup functions
  loadCausal <- do
    cache <- Cache.semispaceCache 1024
    pure (Sqlite.cacheTransaction cache Operations.expectCausalBranchByCausalHash)
  loadDeclNumConstructors <- do
    cache <- Cache.semispaceCache 1024
    pure (Sqlite.cacheTransaction cache Operations.expectDeclNumConstructors)
  loadV1Decl <- do
    cache <- Cache.semispaceCache 1024
    pure (Sqlite.cacheTransaction cache (Codebase.unsafeGetTypeDeclaration codebase))
  -- Since loading a decl type loads the decl and projects out the decl type, just reuse the loadDecl cache
  let loadDeclType ref =
        case ref of
          ReferenceBuiltin name ->
            Map.lookup ref Builtins.builtinConstructorType
              & maybe (error ("Unknown builtin: " ++ Text.unpack name)) pure
          ReferenceDerived refId -> V1.Decl.constructorType <$> loadV1Decl refId
  loadV1Term <- do
    cache <- Cache.semispaceCache 1024
    pure (Sqlite.cacheTransaction cache (Codebase.unsafeGetTerm codebase))
  let loadV1TermComponent = Codebase.unsafeGetTermComponent codebase
  let loadV1DeclComponent = Operations.expectDeclComponent
  pure
    MergeDatabase
      { loadCausal,
        loadDeclNumConstructors,
        loadDeclType,
        loadV1Decl,
        loadV1DeclComponent,
        loadV1Term,
        loadV1TermComponent
      }

-- Convert a v2 referent (missing decl type) to a v1 referent.
referent2to1 :: MergeDatabase -> Referent -> Transaction V1.Referent
referent2to1 MergeDatabase {loadDeclType} = \case
  Referent.Con typeRef conId -> do
    declTy <- loadDeclType typeRef
    pure (V1.Referent.Con (ConstructorReference typeRef conId) declTy)
  Referent.Ref termRef -> pure (V1.Referent.Ref termRef)
