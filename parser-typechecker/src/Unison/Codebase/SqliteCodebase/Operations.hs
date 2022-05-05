{-# LANGUAGE RecordWildCards #-}

-- | This module contains sqlite-specific operations on high-level "parser-typechecker" types all in the Transaction
-- monad.
--
-- The Codebase record-of-functions wraps this functionality, and runs each transaction to IO, so that the operations'
-- are unified with non-sqlite operations in the Codebase interface, like 'appendReflog'.
module Unison.Codebase.SqliteCodebase.Operations where

import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Bitraversable (bitraverse)
import Data.Either.Extra ()
import qualified Data.List as List
import Data.List.NonEmpty.Extra (NonEmpty ((:|)), maximum1)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import U.Codebase.HashTags (CausalHash (unCausalHash))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.NamedRef as S
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Util.Hash as H2
import qualified Unison.Builtin as Builtins
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal.Type as Causal
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as Decl
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Names (Names (Names))
import qualified Unison.Names as Names
import Unison.Names.Scoped (ScopedNames (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.ShortHash as ShortHash
import Unison.Sqlite (Transaction)
import qualified Unison.Sqlite as Sqlite
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Relation as Rel
import qualified Unison.Util.Set as Set
import qualified Unison.WatchKind as UF
import UnliftIO.STM

------------------------------------------------------------------------------------------------------------------------
-- Buffer entry

-- 1) buffer up the component
-- 2) in the event that the component is complete, then what?
--  * can write component provided all of its dependency components are complete.
--    if dependency not complete,
--    register yourself to be written when that dependency is complete

-- an entry for a single hash
data BufferEntry a = BufferEntry
  { -- First, you are waiting for the cycle to fill up with all elements
    -- Then, you check: are all dependencies of the cycle in the db?
    --   If yes: write yourself to database and trigger check of dependents
    --   If no: just wait, do nothing
    beComponentTargetSize :: Maybe Word64,
    beComponent :: Map Reference.Pos a,
    beMissingDependencies :: Set Hash,
    beWaitingDependents :: Set Hash
  }
  deriving (Eq, Show)

prettyBufferEntry :: Show a => Hash -> BufferEntry a -> String
prettyBufferEntry (h :: Hash) BufferEntry {..} =
  "BufferEntry " ++ show h ++ "\n"
    ++ "  { beComponentTargetSize = "
    ++ show beComponentTargetSize
    ++ "\n"
    ++ "  , beComponent = "
    ++ if Map.size beComponent < 2
      then show $ Map.toList beComponent
      else
        mkString (Map.toList beComponent) (Just "\n      [ ") "      , " (Just "]\n")
          ++ "  , beMissingDependencies ="
          ++ if Set.size beMissingDependencies < 2
            then show $ Set.toList beMissingDependencies
            else
              mkString (Set.toList beMissingDependencies) (Just "\n      [ ") "      , " (Just "]\n")
                ++ "  , beWaitingDependents ="
                ++ if Set.size beWaitingDependents < 2
                  then show $ Set.toList beWaitingDependents
                  else
                    mkString (Set.toList beWaitingDependents) (Just "\n      [ ") "      , " (Just "]\n")
                      ++ "  }"
  where
    mkString :: (Foldable f, Show a) => f a -> Maybe String -> String -> Maybe String -> String
    mkString as start middle end =
      fromMaybe "" start ++ List.intercalate middle (show <$> toList as) ++ fromMaybe "" end

type TermBufferEntry = BufferEntry (Term Symbol Ann, Type Symbol Ann)

type DeclBufferEntry = BufferEntry (Decl Symbol Ann)

getBuffer :: TVar (Map Hash (BufferEntry a)) -> Hash -> IO (BufferEntry a)
getBuffer tv h = do
  (Map.lookup h <$> readTVarIO tv) <&> \case
    Just e -> e
    Nothing -> BufferEntry Nothing Map.empty Set.empty Set.empty

putBuffer :: TVar (Map Hash (BufferEntry a)) -> Hash -> BufferEntry a -> IO ()
putBuffer tv h e =
  atomically $ modifyTVar' tv (Map.insert h e)

removeBuffer :: TVar (Map Hash (BufferEntry a)) -> Hash -> IO ()
removeBuffer tv h =
  atomically $ modifyTVar' tv (Map.delete h)

addBufferDependent :: Hash -> TVar (Map Hash (BufferEntry a)) -> Hash -> IO ()
addBufferDependent dependent tv dependency = do
  be <- getBuffer tv dependency
  putBuffer tv dependency be {beWaitingDependents = Set.insert dependent $ beWaitingDependents be}

tryFlushBuffer ::
  forall a.
  (Show a) =>
  TVar (Map Hash (BufferEntry a)) ->
  (H2.Hash -> [a] -> Transaction ()) ->
  (Hash -> Transaction ()) ->
  Hash ->
  Transaction ()
tryFlushBuffer buf saveComponent tryWaiting h@(Cv.hash1to2 -> h2) =
  -- skip if it has already been flushed
  unlessM (Ops.objectExistsForHash h2) do
    BufferEntry size comp (Set.delete h -> missing) waiting <- Sqlite.unsafeIO (getBuffer buf h)
    case size of
      Just size -> do
        missing' <- filterM (fmap not . Ops.objectExistsForHash . Cv.hash1to2) (toList missing)
        if null missing' && size == fromIntegral (length comp)
          then do
            saveComponent h2 (toList comp)
            Sqlite.unsafeIO (removeBuffer buf h)
            traverse_ tryWaiting waiting
          else Sqlite.unsafeIO do
            putBuffer buf h $
              BufferEntry (Just size) comp (Set.fromList missing') waiting
      Nothing ->
        -- it's never even been added, so there's nothing to do.
        pure ()

------------------------------------------------------------------------------------------------------------------------
-- Operations

getTerm ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference.Id ->
  Transaction (Maybe (Term Symbol Ann))
getTerm doGetDeclType (Reference.Id h1@(Cv.hash1to2 -> h2) i) =
  runMaybeT do
    term2 <- Ops.loadTermByReference (C.Reference.Id h2 i)
    lift (Cv.term2to1 h1 doGetDeclType term2)

getDeclType :: C.Reference.Reference -> Transaction CT.ConstructorType
getDeclType = \case
  C.Reference.ReferenceBuiltin t ->
    let err =
          error $
            "I don't know about the builtin type ##"
              ++ show t
              ++ ", but I've been asked for it's ConstructorType."
     in pure . fromMaybe err $
          Map.lookup (Reference.Builtin t) Builtins.builtinConstructorType
  C.Reference.ReferenceDerived i -> expectDeclTypeById i

expectDeclTypeById :: C.Reference.Id -> Transaction CT.ConstructorType
expectDeclTypeById = fmap Cv.decltype2to1 . Ops.expectDeclTypeById

getTypeOfTermImpl :: Reference.Id -> Transaction (Maybe (Type Symbol Ann))
getTypeOfTermImpl (Reference.Id (Cv.hash1to2 -> h2) i) =
  runMaybeT do
    type2 <- Ops.loadTypeOfTermByTermReference (C.Reference.Id h2 i)
    pure (Cv.ttype2to1 type2)

getTermComponentWithTypes ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Hash ->
  Transaction (Maybe [(Term Symbol Ann, Type Symbol Ann)])
getTermComponentWithTypes doGetDeclType h1@(Cv.hash1to2 -> h2) =
  runMaybeT do
    tms <- Ops.loadTermComponent h2
    for tms (bitraverse (lift . Cv.term2to1 h1 doGetDeclType) (pure . Cv.ttype2to1))

getTypeDeclaration :: Reference.Id -> Transaction (Maybe (Decl Symbol Ann))
getTypeDeclaration (Reference.Id h1@(Cv.hash1to2 -> h2) i) =
  runMaybeT do
    decl2 <- Ops.loadDeclByReference (C.Reference.Id h2 i)
    pure (Cv.decl2to1 h1 decl2)

getDeclComponent :: Hash -> Transaction (Maybe [Decl Symbol Ann])
getDeclComponent h1@(Cv.hash1to2 -> h2) =
  runMaybeT do
    decl2 <- Ops.loadDeclComponent h2
    pure (map (Cv.decl2to1 h1) decl2)

getCycleLength :: Hash -> Transaction (Maybe Reference.CycleSize)
getCycleLength (Cv.hash1to2 -> h2) =
  Ops.getCycleLen h2

putTerm ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Term Symbol Ann ->
  Type Symbol Ann ->
  Transaction ()
putTerm termBuffer declBuffer (Reference.Id h@(Cv.hash1to2 -> h2) i) tm tp =
  unlessM (Ops.objectExistsForHash h2) do
    BufferEntry size comp missing waiting <- Sqlite.unsafeIO (getBuffer termBuffer h)
    let termDependencies = Set.toList $ Term.termDependencies tm
    -- update the component target size if we encounter any higher self-references
    let size' = max size (Just $ biggestSelfReference + 1)
          where
            biggestSelfReference =
              maximum1 $
                i :| [i' | Reference.Derived h' i' <- termDependencies, h == h']
    let comp' = Map.insert i (tm, tp) comp
    -- for the component element that's been passed in, add its dependencies to missing'
    missingTerms' <-
      filterM
        (fmap not . Ops.objectExistsForHash . Cv.hash1to2)
        [h | Reference.Derived h _i <- termDependencies]
    missingTypes' <-
      filterM (fmap not . Ops.objectExistsForHash . Cv.hash1to2) $
        [h | Reference.Derived h _i <- Set.toList $ Term.typeDependencies tm]
          ++ [h | Reference.Derived h _i <- Set.toList $ Type.dependencies tp]
    let missing' = missing <> Set.fromList (missingTerms' <> missingTypes')
    Sqlite.unsafeIO do
      -- notify each of the dependencies that h depends on them.
      traverse_ (addBufferDependent h termBuffer) missingTerms'
      traverse_ (addBufferDependent h declBuffer) missingTypes'
      putBuffer termBuffer h (BufferEntry size' comp' missing' waiting)
    tryFlushTermBuffer termBuffer h

tryFlushTermBuffer :: TVar (Map Hash TermBufferEntry) -> Hash -> Transaction ()
tryFlushTermBuffer termBuffer =
  let loop h =
        tryFlushBuffer
          termBuffer
          ( \h2 component -> do
              oId <-
                Ops.saveTermComponent h2 $
                  fmap (bimap (Cv.term1to2 h) Cv.ttype1to2) component
              addTermComponentTypeIndex oId (fmap snd component)
          )
          loop
          h
   in loop

addTermComponentTypeIndex :: ObjectId -> [Type Symbol Ann] -> Transaction ()
addTermComponentTypeIndex oId types = for_ (types `zip` [0 ..]) \(tp, i) -> do
  let self = C.Referent.RefId (C.Reference.Id oId i)
      typeForIndexing = Hashing.typeToReference tp
      typeMentionsForIndexing = Hashing.typeToReferenceMentions tp
  Ops.addTypeToIndexForTerm self (Cv.reference1to2 typeForIndexing)
  Ops.addTypeMentionsToIndexForTerm self (Set.map Cv.reference1to2 typeMentionsForIndexing)

addDeclComponentTypeIndex :: ObjectId -> [[Type Symbol Ann]] -> Transaction ()
addDeclComponentTypeIndex oId ctorss =
  for_ (ctorss `zip` [0 ..]) \(ctors, i) ->
    for_ (ctors `zip` [0 ..]) \(tp, j) -> do
      let self = C.Referent.ConId (C.Reference.Id oId i) j
          typeForIndexing = Hashing.typeToReference tp
          typeMentionsForIndexing = Hashing.typeToReferenceMentions tp
      Ops.addTypeToIndexForTerm self (Cv.reference1to2 typeForIndexing)
      Ops.addTypeMentionsToIndexForTerm self (Set.map Cv.reference1to2 typeMentionsForIndexing)

putTypeDeclaration ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Decl Symbol Ann ->
  Transaction ()
putTypeDeclaration termBuffer declBuffer (Reference.Id h@(Cv.hash1to2 -> h2) i) decl =
  unlessM (Ops.objectExistsForHash h2) do
    BufferEntry size comp missing waiting <- Sqlite.unsafeIO (getBuffer declBuffer h)
    let declDependencies = Set.toList $ Decl.declDependencies decl
    let size' = max size (Just $ biggestSelfReference + 1)
          where
            biggestSelfReference =
              maximum1 $
                i :| [i' | Reference.Derived h' i' <- declDependencies, h == h']
    let comp' = Map.insert i decl comp
    moreMissing <-
      filterM (fmap not . Ops.objectExistsForHash . Cv.hash1to2) $
        [h | Reference.Derived h _i <- declDependencies]
    let missing' = missing <> Set.fromList moreMissing
    Sqlite.unsafeIO do
      traverse_ (addBufferDependent h declBuffer) moreMissing
      putBuffer declBuffer h (BufferEntry size' comp' missing' waiting)
    tryFlushDeclBuffer termBuffer declBuffer h

tryFlushDeclBuffer ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Hash ->
  Transaction ()
tryFlushDeclBuffer termBuffer declBuffer =
  let loop h =
        tryFlushBuffer
          declBuffer
          ( \h2 component -> do
              oId <- Ops.saveDeclComponent h2 $ fmap (Cv.decl1to2 h) component
              addDeclComponentTypeIndex oId $
                fmap (map snd . Decl.constructors . Decl.asDataDecl) component
          )
          (\h -> tryFlushTermBuffer termBuffer h >> loop h)
          h
   in loop

getRootBranch ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  TVar (Maybe (Sqlite.DataVersion, Branch Transaction)) ->
  Transaction (Branch Transaction)
getRootBranch doGetDeclType rootBranchCache =
  Sqlite.unsafeIO (readTVarIO rootBranchCache) >>= \case
    Nothing -> forceReload
    Just (v, b) -> do
      -- check to see if root namespace hash has been externally modified
      -- and reload it if necessary
      v' <- Sqlite.getDataVersion
      if v == v'
        then pure b
        else do
          newRootHash <- Ops.expectRootCausalHash
          if Branch.headHash b == Cv.branchHash2to1 newRootHash
            then pure b
            else do
              traceM $ "database was externally modified (" ++ show v ++ " -> " ++ show v' ++ ")"
              forceReload
  where
    forceReload :: Transaction (Branch Transaction)
    forceReload = do
      causal2 <- Ops.expectRootCausal
      branch1 <- Cv.causalbranch2to1 doGetDeclType causal2
      ver <- Sqlite.getDataVersion
      Sqlite.unsafeIO (atomically (writeTVar rootBranchCache (Just (ver, branch1))))
      pure branch1

getRootBranchExists :: Transaction Bool
getRootBranchExists =
  isJust <$> Ops.loadRootCausalHash

putRootBranch :: TVar (Maybe (Sqlite.DataVersion, Branch Transaction)) -> Branch Transaction -> Transaction ()
putRootBranch rootBranchCache branch1 = do
  -- todo: check to see if root namespace hash has been externally modified
  -- and do something (merge?) it if necessary. But for now, we just overwrite it.
  void (Ops.saveRootBranch (Cv.causalbranch1to2 branch1))
  Sqlite.unsafeIO (atomically $ modifyTVar' rootBranchCache (fmap . second $ const branch1))

-- if this blows up on cromulent hashes, then switch from `hashToHashId`
-- to one that returns Maybe.
getBranchForHash ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Branch.Hash ->
  Transaction (Maybe (Branch Transaction))
getBranchForHash doGetDeclType h = do
  Ops.loadCausalBranchByCausalHash (Cv.branchHash1to2 h) >>= \case
    Nothing -> pure Nothing
    Just causal2 -> do
      branch1 <- Cv.causalbranch2to1 doGetDeclType causal2
      pure (Just branch1)

putBranch :: Branch Transaction -> Transaction ()
putBranch =
  void . Ops.saveBranch . Cv.causalbranch1to2

isCausalHash :: Branch.Hash -> Transaction Bool
isCausalHash (Causal.RawHash h) =
  Q.loadHashIdByHash (Cv.hash1to2 h) >>= \case
    Nothing -> pure False
    Just hId -> Q.isCausalHash hId

getPatch :: Branch.EditHash -> Transaction (Maybe Patch)
getPatch h =
  runMaybeT do
    patchId <- MaybeT (Q.loadPatchObjectIdForPrimaryHash (Cv.patchHash1to2 h))
    patch <- lift (Ops.expectPatch patchId)
    pure (Cv.patch2to1 patch)

putPatch :: Branch.EditHash -> Patch -> Transaction ()
putPatch h p =
  void $ Ops.savePatch (Cv.patchHash1to2 h) (Cv.patch1to2 p)

patchExists :: Branch.EditHash -> Transaction Bool
patchExists h = fmap isJust $ Q.loadPatchObjectIdForPrimaryHash (Cv.patchHash1to2 h)

dependentsImpl :: Reference -> Transaction (Set Reference.Id)
dependentsImpl r =
  Set.map Cv.referenceid2to1
    <$> Ops.dependents (Cv.reference1to2 r)

dependentsOfComponentImpl :: Hash -> Transaction (Set Reference.Id)
dependentsOfComponentImpl h =
  Set.map Cv.referenceid2to1
    <$> Ops.dependentsOfComponent (Cv.hash1to2 h)

watches :: UF.WatchKind -> Transaction [Reference.Id]
watches w =
  Ops.listWatches (Cv.watchKind1to2 w)
    <&> fmap Cv.referenceid2to1

getWatch ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  UF.WatchKind ->
  Reference.Id ->
  Transaction (Maybe (Term Symbol Ann))
getWatch doGetDeclType k r@(Reference.Id h _i) =
  if elem k standardWatchKinds
    then runMaybeT do
      watch <- Ops.loadWatch (Cv.watchKind1to2 k) (Cv.referenceid1to2 r)
      lift (Cv.term2to1 h doGetDeclType watch)
    else pure Nothing

putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> Transaction ()
putWatch k r@(Reference.Id h _i) tm =
  when (elem k standardWatchKinds) do
    Ops.saveWatch
      (Cv.watchKind1to2 k)
      (Cv.referenceid1to2 r)
      (Cv.term1to2 h tm)

standardWatchKinds :: [UF.WatchKind]
standardWatchKinds = [UF.RegularWatch, UF.TestWatch]

clearWatches :: Transaction ()
clearWatches = Ops.clearWatches

termsOfTypeImpl ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference ->
  Transaction (Set Referent.Id)
termsOfTypeImpl doGetDeclType r =
  Ops.termsHavingType (Cv.reference1to2 r)
    >>= Set.traverse (Cv.referentid2to1 doGetDeclType)

termsMentioningTypeImpl ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Reference ->
  Transaction (Set Referent.Id)
termsMentioningTypeImpl doGetDeclType r =
  Ops.termsMentioningType (Cv.reference1to2 r)
    >>= Set.traverse (Cv.referentid2to1 doGetDeclType)

hashLength :: Transaction Int
hashLength = pure 10

branchHashLength :: Transaction Int
branchHashLength = pure 10

defnReferencesByPrefix :: OT.ObjectType -> ShortHash -> Transaction (Set Reference.Id)
defnReferencesByPrefix _ (ShortHash.Builtin _) = pure mempty
defnReferencesByPrefix ot (ShortHash.ShortHash prefix (fmap Cv.shortHashSuffix1to2 -> cycle) _cid) = do
  refs <- do
    Ops.componentReferencesByPrefix ot prefix cycle
      >>= traverse (C.Reference.idH Q.expectPrimaryHashByObjectId)
      >>= pure . Set.fromList
  pure $ Set.map Cv.referenceid2to1 refs

termReferencesByPrefix :: ShortHash -> Transaction (Set Reference.Id)
termReferencesByPrefix = defnReferencesByPrefix OT.TermComponent

declReferencesByPrefix :: ShortHash -> Transaction (Set Reference.Id)
declReferencesByPrefix = defnReferencesByPrefix OT.DeclComponent

referentsByPrefix ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  ShortHash ->
  Transaction (Set Referent.Id)
referentsByPrefix _doGetDeclType SH.Builtin {} = pure mempty
referentsByPrefix doGetDeclType (SH.ShortHash prefix (fmap Cv.shortHashSuffix1to2 -> cycle) cid) = do
  termReferents <-
    Ops.termReferentsByPrefix prefix cycle
      >>= traverse (Cv.referentid2to1 doGetDeclType)
  declReferents' <- Ops.declReferentsByPrefix prefix cycle (read . Text.unpack <$> cid)
  let declReferents =
        [ Referent.ConId (ConstructorReference (Reference.Id (Cv.hash2to1 h) pos) (fromIntegral cid)) (Cv.decltype2to1 ct)
          | (h, pos, ct, cids) <- declReferents',
            cid <- cids
        ]
  pure . Set.fromList $ termReferents <> declReferents

branchHashesByPrefix :: ShortBranchHash -> Transaction (Set Branch.Hash)
branchHashesByPrefix sh = do
  -- given that a Branch is shallow, it's really `CausalHash` that you'd
  -- refer to to specify a full namespace w/ history.
  -- but do we want to be able to refer to a namespace without its history?
  cs <- Ops.causalHashesByPrefix (Cv.sbh1to2 sh)
  pure $ Set.map (Causal.RawHash . Cv.hash2to1 . unCausalHash) cs

sqlLca :: Branch.Hash -> Branch.Hash -> Transaction (Maybe Branch.Hash)
sqlLca h1 h2 = do
  h3 <- Ops.lca (Cv.causalHash1to2 h1) (Cv.causalHash1to2 h2)
  pure (Cv.causalHash2to1 <$> h3)

-- well one or the other. :zany_face: the thinking being that they wouldn't hash-collide
termExists, declExists :: Hash -> Transaction Bool
termExists = fmap isJust . Q.loadObjectIdForPrimaryHash . Cv.hash1to2
declExists = termExists

before :: Branch.Hash -> Branch.Hash -> Transaction (Maybe Bool)
before h1 h2 =
  Ops.before (Cv.causalHash1to2 h1) (Cv.causalHash1to2 h2)

-- | Construct a 'ScopedNames' which can produce names which are relative to the provided
-- Path.
namesAtPath ::
  Path ->
  Transaction ScopedNames
namesAtPath path = do
  (termNames, typeNames) <- Ops.rootBranchNames
  let allTerms :: [(Name, Referent.Referent)]
      allTerms =
        termNames <&> \(S.NamedRef {reversedSegments, ref = (ref, ct)}) ->
          let v1ref = runIdentity $ Cv.referent2to1 (const . pure . Cv.constructorType2to1 . fromMaybe (error "Required constructor type for constructor but it was null") $ ct) ref
           in (Name.fromReverseSegments (coerce reversedSegments), v1ref)
  let allTypes :: [(Name, Reference.Reference)]
      allTypes =
        typeNames <&> \(S.NamedRef {reversedSegments, ref}) ->
          (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
  let rootTerms = Rel.fromListDomainAsc allTerms
  let rootTypes = Rel.fromListDomainAsc allTypes
  let absoluteRootNames = Names {terms = rootTerms, types = rootTypes}
  let (relativeScopedNames, absoluteExternalNames) =
        case path of
          Path.Empty -> (absoluteRootNames, mempty)
          p ->
            let reversedPathSegments = reverse . Path.toList $ p
                (relativeTerms, externalTerms) = foldMap (partitionByPathPrefix reversedPathSegments) allTerms
                (relativeTypes, externalTypes) = foldMap (partitionByPathPrefix reversedPathSegments) allTypes
             in ( Names {terms = Rel.fromListDomainAsc relativeTerms, types = Rel.fromListDomainAsc relativeTypes},
                  Names {terms = Rel.fromListDomainAsc externalTerms, types = Rel.fromListDomainAsc externalTypes}
                )
  pure $
    ScopedNames
      { absoluteExternalNames,
        relativeScopedNames,
        absoluteRootNames
      }
  where
    -- If the given prefix matches the given name, the prefix is stripped and it's collected
    -- on the left, otherwise it's left as-is and collected on the right.
    -- >>> partitionByPathPrefix ["b", "a"] ("a.b.c", ())
    -- ([(c,())],[])
    --
    -- >>> partitionByPathPrefix ["y", "x"] ("a.b.c", ())
    -- ([],[(a.b.c,())])
    partitionByPathPrefix :: [NameSegment] -> (Name, r) -> ([(Name, r)], [(Name, r)])
    partitionByPathPrefix reversedPathSegments (n, ref) =
      case Name.stripReversedPrefix n reversedPathSegments of
        Nothing -> (mempty, [(n, ref)])
        Just stripped -> ([(Name.makeRelative stripped, ref)], mempty)

saveRootNamesIndex :: Names -> Transaction ()
saveRootNamesIndex Names {Names.terms, Names.types} = do
  let termNames :: [(S.NamedRef (C.Referent.Referent, Maybe C.Referent.ConstructorType))]
      termNames = Rel.toList terms <&> \(name, ref) -> S.NamedRef {reversedSegments = nameSegments name, ref = splitReferent ref}
  let typeNames :: [(S.NamedRef C.Reference.Reference)]
      typeNames =
        Rel.toList types
          <&> ( \(name, ref) ->
                  S.NamedRef {reversedSegments = nameSegments name, ref = Cv.reference1to2 ref}
              )
  Ops.rebuildNameIndex termNames typeNames
  where
    nameSegments :: Name -> NonEmpty Text
    nameSegments = coerce @(NonEmpty NameSegment) @(NonEmpty Text) . Name.reverseSegments
    splitReferent :: Referent.Referent -> (C.Referent.Referent, Maybe C.Referent.ConstructorType)
    splitReferent referent = case referent of
      Referent.Ref {} -> (Cv.referent1to2 referent, Nothing)
      Referent.Con _ref ct -> (Cv.referent1to2 referent, Just (Cv.constructorType1to2 ct))
