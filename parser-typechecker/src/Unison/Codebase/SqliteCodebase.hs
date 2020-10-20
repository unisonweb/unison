module Unison.Codebase.SqliteCodebase where

-- initCodebase :: Branch.Cache IO -> FilePath -> IO (Codebase IO Symbol Ann)

import qualified Unison.Codebase as Codebase1
import qualified Unison.Reference as Reference
import Unison.Term (Term)
import Unison.Symbol (Symbol)
import Unison.Parser (Ann)
import Unison.Type (Type)
import Unison.DataDeclaration (Decl)
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Data.Set (Set)
import Unison.Reference (Reference)
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.UnisonFile as UF
import qualified Unison.Codebase.Reflog as Reflog
import Data.Text (Text)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
-- import qualified Database.SQLite.Simple as Sqlite

sqliteCodebase :: FilePath -> IO (Codebase1.Codebase IO Symbol Ann)
sqliteCodebase _root = do
  -- c :: Sqlite.Connection <- error "todo"
  let
    getTerm            :: Reference.Id -> IO (Maybe (Term Symbol Ann))
    getTypeOfTermImpl  :: Reference.Id -> IO (Maybe (Type Symbol Ann))
    getTypeDeclaration :: Reference.Id -> IO (Maybe (Decl Symbol Ann))
    putTerm            :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> IO ()
    putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> IO ()
    getRootBranch      :: IO (Either Codebase1.GetRootBranchError (Branch IO))
    putRootBranch      :: Branch IO -> IO ()
    rootBranchUpdates  :: IO (IO (), IO (Set Branch.Hash))
    getBranchForHash   :: Branch.Hash -> IO (Maybe (Branch IO))
    dependentsImpl     :: Reference -> IO (Set Reference.Id)
    syncFromDirectory  :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
    syncToDirectory    :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
    watches            :: UF.WatchKind -> IO [Reference.Id]
    getWatch           :: UF.WatchKind -> Reference.Id -> IO (Maybe (Term Symbol Ann))
    putWatch           :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> IO ()
    getReflog          :: IO [Reflog.Entry]
    appendReflog       :: Text -> Branch IO -> Branch IO -> IO ()
    termsOfTypeImpl    :: Reference -> IO (Set Referent.Id)
    termsMentioningTypeImpl :: Reference -> IO (Set Referent.Id)
    hashLength         :: IO Int
    termReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
    typeReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
    termReferentsByPrefix :: ShortHash -> IO (Set Referent.Id)
    branchHashLength   :: IO Int
    branchHashesByPrefix :: ShortBranchHash -> IO (Set Branch.Hash)

    getTerm (Reference.Id _r _i _n) = error "todo"
    getTypeOfTermImpl = error "todo"
    getTypeDeclaration = error "todo"
    putTerm = error "todo"
    putTypeDeclaration = error "todo"
    getRootBranch = error "todo"
    putRootBranch = error "todo"
    rootBranchUpdates = error "todo"
    getBranchForHash = error "todo"
    dependentsImpl = error "todo"
    syncFromDirectory = error "todo"
    syncToDirectory = error "todo"
    watches = error "todo"
    getWatch = error "todo"
    putWatch = error "todo"
    getReflog = error "todo"
    appendReflog = error "todo"
    termsOfTypeImpl = error "todo"
    termsMentioningTypeImpl = error "todo"
    hashLength = error "todo"
    termReferencesByPrefix = error "todo"
    typeReferencesByPrefix = error "todo"
    termReferentsByPrefix = error "todo"
    branchHashLength = error "todo"
    branchHashesByPrefix = error "todo"
  pure $ Codebase1.Codebase
    getTerm
    getTypeOfTermImpl
    getTypeDeclaration
    putTerm
    putTypeDeclaration
    getRootBranch
    putRootBranch
    rootBranchUpdates
    getBranchForHash
    dependentsImpl
    syncFromDirectory
    syncToDirectory
    watches
    getWatch
    putWatch
    getReflog
    appendReflog
    termsOfTypeImpl
    termsMentioningTypeImpl
    hashLength
    termReferencesByPrefix
    typeReferencesByPrefix
    termReferentsByPrefix
    branchHashLength
    branchHashesByPrefix
