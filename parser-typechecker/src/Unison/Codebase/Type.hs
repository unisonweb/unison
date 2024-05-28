{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Type
  ( Codebase (..),
    CodebasePath,
    LocalOrRemote (..),
  )
where

import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reference qualified as V2
import Unison.Codebase.Branch (Branch)
import Unison.CodebasePath (CodebasePath)
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import Unison.Prelude
import Unison.Reference (Reference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.Sqlite qualified as Sqlite
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.WatchKind qualified as WK

-- | Abstract interface to a user's codebase.
data Codebase m v a = Codebase
  { -- | Get a user-defined term from the codebase.
    --
    -- Note that it is possible to call 'putTerm', then 'getTerm', and receive @Nothing@, per the semantics of
    -- 'putTerm'.
    getTerm :: Reference.Id -> Sqlite.Transaction (Maybe (Term v a)),
    -- | Get the type of a user-defined term.
    --
    -- Note that it is possible to call 'putTerm', then 'getTypeOfTermImpl', and receive @Nothing@, per the semantics of
    -- 'putTerm'.
    getTypeOfTermImpl :: Reference.Id -> Sqlite.Transaction (Maybe (Type v a)),
    -- | Get a type declaration.
    --
    -- Note that it is possible to call 'putTypeDeclaration', then 'getTypeDeclaration', and receive @Nothing@, per the
    -- semantics of 'putTypeDeclaration'.
    getTypeDeclaration :: Reference.Id -> Sqlite.Transaction (Maybe (Decl v a)),
    -- | Get the type of a given decl.
    getDeclType :: V2.Reference -> Sqlite.Transaction CT.ConstructorType,
    -- | Enqueue the put of a user-defined term (with its type) into the codebase, if it doesn't already exist. The
    -- implementation may choose to delay the put until all of the term's (and its type's) references are stored as
    -- well.
    putTerm :: Reference.Id -> Term v a -> Type v a -> Sqlite.Transaction (),
    putTermComponent :: Hash -> [(Term v a, Type v a)] -> Sqlite.Transaction (),
    -- | Enqueue the put of a type declaration into the codebase, if it doesn't already exist. The implementation may
    -- choose to delay the put until all of the type declaration's references are stored as well.
    putTypeDeclaration :: Reference.Id -> Decl v a -> Sqlite.Transaction (),
    putTypeDeclarationComponent :: Hash -> [Decl v a] -> Sqlite.Transaction (),
    -- getTermComponent :: Hash -> m (Maybe [Term v a]),
    getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term v a, Type v a)]),
    -- | Get the root branch.
    getRootBranch :: m (Branch m),
    -- | Like 'putBranch', but also adjusts the root branch pointer afterwards.
    putRootBranch ::
      Text -> -- Reason for the change, will be recorded in the reflog
      Branch m ->
      m (),
    getBranchForHash :: CausalHash -> m (Maybe (Branch m)),
    -- | Put a branch into the codebase, which includes its children, its patches, and the branch itself, if they don't
    -- already exist.
    --
    -- The terms and type declarations that a branch references must already exist in the codebase.
    putBranch :: Branch m -> m (),
    -- | @getWatch k r@ returns watch result @t@ that was previously put by @putWatch k r t@.
    getWatch :: WK.WatchKind -> Reference.Id -> Sqlite.Transaction (Maybe (Term v a)),
    -- | Get the set of user-defined terms-or-constructors that have the given type.
    termsOfTypeImpl :: Reference -> Sqlite.Transaction (Set Referent.Id),
    -- | Get the set of user-defined terms-or-constructors mention the given type anywhere in their signature.
    termsMentioningTypeImpl :: Reference -> Sqlite.Transaction (Set Referent.Id),
    -- | Return the subset of the given set that has the given type.
    filterTermsByReferenceIdHavingTypeImpl :: TypeReference -> Set Reference.Id -> Sqlite.Transaction (Set Reference.Id),
    -- | Return the subset of the given set that has the given type.
    filterTermsByReferentIdHavingTypeImpl :: TypeReference -> Set Referent.Id -> Sqlite.Transaction (Set Referent.Id),
    -- | Get the set of user-defined terms-or-constructors whose hash matches the given prefix.
    termReferentsByPrefix :: ShortHash -> Sqlite.Transaction (Set Referent.Id),
    -- | Acquire a new connection to the same underlying database file this codebase object connects to.
    withConnection :: forall x. (Sqlite.Connection -> m x) -> m x,
    -- | Acquire a new connection to the same underlying database file this codebase object connects to.
    withConnectionIO :: forall x. (Sqlite.Connection -> IO x) -> IO x
  }

-- | Whether a codebase is local or remote.
data LocalOrRemote
  = Local
  | Remote
  deriving (Show, Eq, Ord)
