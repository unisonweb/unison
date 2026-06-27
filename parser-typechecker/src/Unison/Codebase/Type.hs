module Unison.Codebase.Type
  ( Codebase (..),
    CodebasePath,
    LocalOrRemote (..),
  )
where

import U.Codebase.HashTags (BranchHash, CausalHash)
import Unison.Codebase.Branch (Branch)
import Unison.CodebasePath (CodebasePath)
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (Decl)
import Unison.DeclCoherencyCheck (IncoherentDeclReasons)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Hash (Hash)
import Unison.OpaqueDeclaration (OpaqueDeclaration)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.ShortHash (ShortHash)
import Unison.Sqlite qualified as Sqlite
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.TypeAlias (TypeAlias)
import Unison.TypeEntry (TypeEntry)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView)
import Unison.WatchKind qualified as WK

-- | Abstract interface to a user's codebase.
data Codebase m v a = Codebase
  { -- | Get a user-defined term from the codebase.
    --
    -- Note that it is possible to call 'putTerm', then 'getTerm', and receive @Nothing@, per the semantics of
    -- 'putTerm'.
    getTerm :: TermReferenceId -> Sqlite.Transaction (Maybe (Term v a)),
    -- | Get the type of a user-defined term.
    --
    -- Note that it is possible to call 'putTerm', then 'getTypeOfTermImpl', and receive @Nothing@, per the semantics of
    -- 'putTerm'.
    getTypeOfTermImpl :: TermReferenceId -> Sqlite.Transaction (Maybe (Type v a)),
    -- | Get a type declaration.
    --
    -- Note that it is possible to call 'putTypeDeclaration', then 'getTypeDeclaration', and receive @Nothing@, per the
    -- semantics of 'putTypeDeclaration'.
    getTypeDeclaration :: TypeReferenceId -> Sqlite.Transaction (Maybe (Decl v a)),
    getTypeDeclarationComponent :: Hash -> Sqlite.Transaction (Maybe [Decl v a]),
    -- | Get a type alias entry. Returns @Nothing@ if no such alias exists or
    -- if the reference points to a regular type declaration.
    getTypeAlias :: TypeReferenceId -> Sqlite.Transaction (Maybe (TypeAlias v a)),
    -- | Get either a data\/effect declaration or a type alias for the given
    -- type reference. Aliases share the namespace's types slot with decls;
    -- this is the single dispatch point that distinguishes them. Returns
    -- @Nothing@ if the reference points at neither.
    getTypeEntry :: TypeReferenceId -> Sqlite.Transaction (Maybe (TypeEntry v a)),
    -- | Check whether a type-position reference resolves to an alias.
    isTypeAlias :: TypeReference -> Sqlite.Transaction Bool,
    -- | Get an opaque declaration. Returns @Nothing@ if no such opaque
    -- declaration exists, or if the reference points to a regular type
    -- declaration or alias.
    --
    -- TODO(opaque): the returned 'OpaqueDeclaration' currently has an empty
    -- @body@; body items will be fetched via the membership table once it
    -- lands.
    getOpaqueDeclaration :: TypeReferenceId -> Sqlite.Transaction (Maybe (OpaqueDeclaration v a)),
    -- | Check whether a type-position reference resolves to an opaque
    -- declaration.
    isOpaqueDeclaration :: TypeReference -> Sqlite.Transaction Bool,
    -- | Get the type of a given decl.
    getDeclType :: TypeReference -> Sqlite.Transaction CT.ConstructorType,
    expectDeclNumConstructors :: TypeReferenceId -> Sqlite.Transaction Int,
    -- | Enqueue the put of a user-defined term (with its type) into the codebase, if it doesn't already exist. The
    -- implementation may choose to delay the put until all of the term's (and its type's) references are stored as
    -- well.
    putTerm :: TermReferenceId -> Term v a -> Type v a -> Sqlite.Transaction (),
    putTermComponent :: Hash -> [(Term v a, Type v a)] -> Sqlite.Transaction (),
    -- | Enqueue the put of a type declaration into the codebase, if it doesn't already exist. The implementation may
    -- choose to delay the put until all of the type declaration's references are stored as well.
    putTypeDeclaration :: TypeReferenceId -> Decl v a -> Sqlite.Transaction (),
    putTypeDeclarationComponent :: Hash -> [Decl v a] -> Sqlite.Transaction (),
    -- | Save a type alias. Its body's dependencies must already be in the
    -- codebase; aliases cannot be enqueued for deferred persistence the way
    -- recursive decl components can, because aliases are non-recursive.
    putTypeAlias :: TypeReferenceId -> TypeAlias v a -> Sqlite.Transaction (),
    -- | Save an opaque type declaration. The RHS's dependencies must already
    -- be in the codebase; opaques cannot be enqueued for deferred persistence
    -- because their RHS is non-recursive.
    --
    -- TODO(opaque): body items are not saved by this call; they are persisted
    -- as ordinary terms by the surrounding @addDefsToCodebase@ flow, and the
    -- membership row that links each body term to this parent is written
    -- separately (plan §2.2).
    putOpaqueDeclaration :: TypeReferenceId -> OpaqueDeclaration v a -> Sqlite.Transaction (),
    -- getTermComponent :: Hash -> m (Maybe [Term v a]),
    getTermComponentWithTypes :: Hash -> Sqlite.Transaction (Maybe [(Term v a, Type v a)]),
    getBranchForHash :: CausalHash -> m (Maybe (Branch m)),
    -- | Like `getBranchForHash`, but in Transaction... this should entirely replace `getBranchForHash` (some day)
    getBranchForHashTx :: CausalHash -> Sqlite.Transaction (Maybe (Branch Sqlite.Transaction)),
    getBranchDeclNumConstructors :: BranchHash -> Set TypeReference -> Sqlite.Transaction (Map TypeReferenceId Int),
    -- | Get a partial decl name lookup for a branch.
    getBranchPartialDeclNameLookup :: BranchHash -> UnconflictedLocalDefnsView -> Sqlite.Transaction PartialDeclNameLookup,
    -- | Get a decl name lookup for a branch (or an error, if there's an incoherent decl)
    getBranchDeclNameLookup ::
      BranchHash ->
      UnconflictedLocalDefnsView ->
      Sqlite.Transaction (Either IncoherentDeclReasons DeclNameLookup),
    -- | Put a branch into the codebase, which includes its children, its patches, and the branch itself, if they don't
    -- already exist.
    --
    -- The terms and type declarations that a branch references must already exist in the codebase.
    putBranch :: Branch m -> m (),
    putBranchTx :: Branch Sqlite.Transaction -> Sqlite.Transaction (),
    -- | @getWatch k r@ returns watch result @t@ that was previously put by @putWatch k r t@.
    getWatch :: WK.WatchKind -> TermReferenceId -> Sqlite.Transaction (Maybe (Term v a)),
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
    withConnectionIO :: forall x. (Sqlite.Connection -> IO x) -> IO x,
    -- | This optimization allows us to pre-fetch a branch from SQLite into the branch cache when we know we'll need it
    -- soon, but not immediately. E.g. the user has switched a branch, but hasn't run any commands on it yet.
    --
    -- This combinator returns immediately, but warms the cache in the background with the desired branch.
    preloadBranch :: CausalHash -> m ()
  }

-- | Whether a codebase is local or remote.
data LocalOrRemote
  = Local
  | Remote
  deriving (Show, Eq, Ord)
