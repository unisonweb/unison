{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.SqliteCodebase where

-- initCodebase :: Branch.Cache IO -> FilePath -> IO (Codebase IO Symbol Ann)

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Short as SBS
import Data.Set (Set)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as Sqlite
import System.FilePath ((</>))
import qualified U.Codebase.Decl as V2.Decl
import qualified U.Codebase.Reference as V2
import qualified U.Codebase.Reference as V2.Reference
import qualified U.Codebase.Referent as V2
import qualified U.Codebase.Sqlite.Symbol as V2
import qualified U.Codebase.Term as V2.Term
import qualified U.Core.ABT as V2.ABT
import qualified U.Util.Hash as V2
import qualified U.Util.Hash as V2.Hash
import qualified Unison.ABT as V1.ABT
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase1
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Reflog as Reflog
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import Unison.Hash (Hash)
import qualified Unison.Hash as V1
import Unison.Parser (Ann)
import qualified Unison.Parser as Ann
import qualified Unison.Pattern as P
import Unison.Prelude (MaybeT (runMaybeT))
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Reference as V1
import qualified Unison.Reference as V1.Reference
import qualified Unison.Referent as Referent
import qualified Unison.Referent as V1
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import qualified Unison.Symbol as V1
import Unison.Term (Term)
import qualified Unison.Term as V1.Term
import Unison.Type (Type)
import qualified Unison.Type as V1.Type
import qualified Unison.UnisonFile as UF
import qualified Unison.Var as Var

sqliteCodebase :: CodebasePath -> IO (IO (), Codebase1.Codebase IO Symbol Ann)
sqliteCodebase root = do
  conn :: Sqlite.Connection <- Sqlite.open $ root </> "v2" </> "unison.sqlite3"
  let getTypeOfTermImpl :: Reference.Id -> IO (Maybe (Type Symbol Ann))
      getTypeDeclaration :: Reference.Id -> IO (Maybe (Decl Symbol Ann))
      putTerm :: Reference.Id -> Term Symbol Ann -> Type Symbol Ann -> IO ()
      putTypeDeclaration :: Reference.Id -> Decl Symbol Ann -> IO ()
      getRootBranch :: IO (Either Codebase1.GetRootBranchError (Branch IO))
      putRootBranch :: Branch IO -> IO ()
      rootBranchUpdates :: IO (IO (), IO (Set Branch.Hash))
      getBranchForHash :: Branch.Hash -> IO (Maybe (Branch IO))
      dependentsImpl :: Reference -> IO (Set Reference.Id)
      syncFromDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
      syncToDirectory :: Codebase1.CodebasePath -> SyncMode -> Branch IO -> IO ()
      watches :: UF.WatchKind -> IO [Reference.Id]
      getWatch :: UF.WatchKind -> Reference.Id -> IO (Maybe (Term Symbol Ann))
      putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> IO ()
      getReflog :: IO [Reflog.Entry]
      appendReflog :: Text -> Branch IO -> Branch IO -> IO ()
      termsOfTypeImpl :: Reference -> IO (Set Referent.Id)
      termsMentioningTypeImpl :: Reference -> IO (Set Referent.Id)
      hashLength :: IO Int
      termReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
      typeReferencesByPrefix :: ShortHash -> IO (Set Reference.Id)
      termReferentsByPrefix :: ShortHash -> IO (Set Referent.Id)
      branchHashLength :: IO Int
      branchHashesByPrefix :: ShortBranchHash -> IO (Set Branch.Hash)

      getTerm :: Reference.Id -> IO (Maybe (Term Symbol Ann))
      getTerm (Reference.Id _h _i _n) = error "todo"
      -- runDB . fmap (term2to1 h) $ Ops.loadTermByReference (C.Reference.Id h i)
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
  let finalizer = Sqlite.close conn
  pure $
    ( finalizer,
      Codebase1.Codebase
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
    )

-- x :: DB m => MaybeT m (Term Symbol) -> MaybeT m (Term Symbol Ann)
-- x = error "not implemented"

term2to1 :: forall m. Monad m => Hash -> (Hash -> m V1.Reference.Size) -> (V2.Reference -> m CT.ConstructorType) -> V2.Term.Term V2.Symbol -> m (V1.Term.Term V1.Symbol Ann)
term2to1 h lookupSize lookupCT tm =
  V1.ABT.transformM (termF2to1 h lookupSize lookupCT)
    . V1.ABT.vmap symbol2to1
    . V1.ABT.amap (const Ann.External)
    $ abt2to1 tm

symbol2to1 :: V2.Symbol -> V1.Symbol
symbol2to1 (V2.Symbol w t) = V1.Symbol w (Var.User t)

abt2to1 :: Functor f => V2.ABT.Term f v a -> V1.ABT.Term f v a
abt2to1 (V2.ABT.Term fv a out) = V1.ABT.Term fv a (go out)
  where
    go = \case
      V2.ABT.Cycle body -> V1.ABT.Cycle (abt2to1 body)
      V2.ABT.Abs v body -> V1.ABT.Abs v (abt2to1 body)
      V2.ABT.Var v -> V1.ABT.Var v
      V2.ABT.Tm tm -> V1.ABT.Tm (abt2to1 <$> tm)

abt1to2 :: Functor f => V1.ABT.Term f v a -> V2.ABT.Term f v a
abt1to2 (V1.ABT.Term fv a out) = V2.ABT.Term fv a (go out)
  where
    go = \case
      V1.ABT.Cycle body -> V2.ABT.Cycle (abt1to2 body)
      V1.ABT.Abs v body -> V2.ABT.Abs v (abt1to2 body)
      V1.ABT.Var v -> V2.ABT.Var v
      V1.ABT.Tm tm -> V2.ABT.Tm (abt1to2 <$> tm)

rreference2to1 :: Applicative m => Hash -> (Hash -> m V1.Reference.Size) -> V2.Reference' Text (Maybe V2.Hash) -> m V1.Reference
rreference2to1 h lookupSize = \case
  V2.ReferenceBuiltin t -> pure $ V1.Reference.Builtin t
  V2.ReferenceDerived i -> V1.Reference.DerivedId <$> rreferenceid2to1 h lookupSize i

rreferenceid2to1 :: Functor m => Hash -> (Hash -> m V1.Reference.Size) -> V2.Reference.Id' (Maybe V2.Hash) -> m V1.Reference.Id
rreferenceid2to1 h lookupSize (V2.Reference.Id oh i) =
  V1.Reference.Id h' i <$> lookupSize h'
  where
    h' = maybe h hash2to1 oh

reference2to1 :: Applicative m => (Hash -> m V1.Reference.Size) -> V2.Reference -> m V1.Reference
reference2to1 lookupSize = \case
  V2.ReferenceBuiltin t -> pure $ V1.Reference.Builtin t
  V2.ReferenceDerived i -> V1.Reference.DerivedId <$> referenceid2to1 lookupSize i

referenceid2to1 :: Functor m => (Hash -> m V1.Reference.Size) -> V2.Reference.Id -> m V1.Reference.Id
referenceid2to1 lookupSize (V2.Reference.Id h i) =
  V1.Reference.Id sh i <$> lookupSize sh
  where
    sh = hash2to1 h

rreferent2to1 :: Applicative m => Hash -> (Hash -> m V1.Reference.Size) -> (V2.Reference -> m CT.ConstructorType) -> V2.ReferentH -> m V1.Referent
rreferent2to1 h lookupSize lookupCT = \case
  V2.Ref r -> V1.Ref <$> rreference2to1 h lookupSize r
  V2.Con r i -> V1.Con <$> reference2to1 lookupSize r <*> pure (fromIntegral i) <*> lookupCT r

hash2to1 :: V2.Hash.Hash -> Hash
hash2to1 (V2.Hash.Hash sbs) = V1.Hash (SBS.fromShort sbs)

ttype2to1 :: Monad m => (Hash -> m V1.Reference.Size) -> V2.Term.Type V2.Symbol -> m (V1.Type.Type V1.Symbol Ann)
ttype2to1 = undefined

dtype2to1 :: Monad m => Hash -> (Hash -> m V1.Reference.Size) -> V2.Decl.Type V2.Symbol -> m (V1.Type.Type V1.Symbol Ann)
dtype2to1 = undefined

termF2to1 :: forall m a. Monad m => Hash -> (Hash -> m V1.Reference.Size) -> (V2.Reference -> m CT.ConstructorType) -> V2.Term.F V2.Symbol a -> m (V1.Term.F V1.Symbol Ann Ann a)
termF2to1 h lookupSize lookupCT = go
  where
    go :: V2.Term.F V2.Symbol a -> m (V1.Term.F V1.Symbol Ann Ann a)
    go = \case
      V2.Term.Int i -> pure $ V1.Term.Int i
      V2.Term.Nat n -> pure $ V1.Term.Nat n
      V2.Term.Float d -> pure $ V1.Term.Float d
      V2.Term.Boolean b -> pure $ V1.Term.Boolean b
      V2.Term.Text t -> pure $ V1.Term.Text t
      V2.Term.Char c -> pure $ V1.Term.Char c
      V2.Term.Ref r -> V1.Term.Ref <$> rreference2to1 h lookupSize r
      V2.Term.Constructor r i ->
        V1.Term.Constructor <$> reference2to1 lookupSize r <*> pure (fromIntegral i)
      V2.Term.Request r i ->
        V1.Term.Request <$> reference2to1 lookupSize r <*> pure (fromIntegral i)
      V2.Term.Handle a a4 -> pure $ V1.Term.Handle a a4
      V2.Term.App a a4 -> pure $ V1.Term.App a a4
      V2.Term.Ann a t2 -> V1.Term.Ann a <$> ttype2to1 lookupSize t2
      V2.Term.Sequence sa -> pure $ V1.Term.Sequence sa
      V2.Term.If a a4 a5 -> pure $ V1.Term.If a a4 a5
      V2.Term.And a a4 -> pure $ V1.Term.And a a4
      V2.Term.Or a a4 -> pure $ V1.Term.Or a a4
      V2.Term.Lam a -> pure $ V1.Term.Lam a
      V2.Term.LetRec as a -> pure $ V1.Term.LetRec False as a
      V2.Term.Let a a4 -> pure $ V1.Term.Let False a a4
      V2.Term.Match a cases -> V1.Term.Match a <$> traverse goCase cases
      V2.Term.TermLink rr -> V1.Term.TermLink <$> rreferent2to1 h lookupSize lookupCT rr
      V2.Term.TypeLink r -> V1.Term.TypeLink <$> reference2to1 lookupSize r
    goCase = \case
      V2.Term.MatchCase pat cond body ->
        V1.Term.MatchCase <$> (goPat pat) <*> pure cond <*> pure body
    goPat = \case
      V2.Term.PUnbound -> pure $ P.Unbound a
      V2.Term.PVar -> pure $ P.Var a
      V2.Term.PBoolean b -> pure $ P.Boolean a b
      V2.Term.PInt i -> pure $ P.Int a i
      V2.Term.PNat n -> pure $ P.Nat a n
      V2.Term.PFloat d -> pure $ P.Float a d
      V2.Term.PText t -> pure $ P.Text a t
      V2.Term.PChar c -> pure $ P.Char a c
      V2.Term.PConstructor r i ps ->
        P.Constructor a <$> reference2to1 lookupSize r <*> pure i <*> (traverse goPat ps)
      V2.Term.PAs p -> P.As a <$> goPat p
      V2.Term.PEffectPure p -> P.EffectPure a <$> goPat p
      V2.Term.PEffectBind r i ps p -> P.EffectBind a <$> reference2to1 lookupSize r <*> pure i <*> traverse goPat ps <*> goPat p
      V2.Term.PSequenceLiteral ps -> P.SequenceLiteral a <$> traverse goPat ps
      V2.Term.PSequenceOp p1 op p2 -> P.SequenceOp a <$> goPat p1 <*> pure (goOp op) <*> goPat p2
    goOp = \case
      V2.Term.PCons -> P.Cons
      V2.Term.PSnoc -> P.Snoc
      V2.Term.PConcat -> P.Concat
    a = Ann.External

runDB :: Connection -> MaybeT (ReaderT Connection IO) a -> IO (Maybe a)
runDB conn action = flip runReaderT conn $ runMaybeT action
