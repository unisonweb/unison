{-# LANGUAGE RecordWildCards #-}

-- | This module contains sqlite-specific operations on high-level "parser-typechecker" types all in the Transaction
-- monad.
--
-- The Codebase record-of-functions wraps this functionality, and runs each transaction to IO, so that the operations'
-- are unified with non-sqlite operations in the Codebase interface, like 'appendReflog'.
module Unison.Codebase.SqliteCodebase.Operations where

import Control.Lens (ifor)
import Data.Bitraversable (bitraverse)
import Data.Either.Extra ()
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEList
import Data.List.NonEmpty.Extra (NonEmpty ((:|)), maximum1)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified U.Codebase.Branch as V2Branch
import qualified U.Codebase.Branch.Diff as BranchDiff
import qualified U.Codebase.Causal as V2Causal
import U.Codebase.HashTags (BranchHash, CausalHash (unCausalHash))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.Sqlite.DbId (ObjectId)
import qualified U.Codebase.Sqlite.NamedRef as S
import qualified U.Codebase.Sqlite.ObjectType as OT
import U.Codebase.Sqlite.Operations (NamesByPath (..))
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sqlite.V2.Decl (saveDeclComponent)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import U.Codebase.Sqlite.V2.Term (saveTermComponent)
import qualified U.Util.Cache as Cache
import qualified U.Util.Hash as H2
import qualified Unison.Builtin as Builtins
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Names as V1Branch
import qualified Unison.Codebase.Causal.Type as Causal
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.SqliteCodebase.Branch.Cache (BranchCache)
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
import qualified Unison.Sqlite.Transaction as Sqlite
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
tryFlushBuffer buf saveComponent tryWaiting h =
  -- skip if it has already been flushed
  unlessM (Ops.objectExistsForHash h) do
    BufferEntry size comp (Set.delete h -> missing) waiting <- Sqlite.unsafeIO (getBuffer buf h)
    case size of
      Just size -> do
        missing' <- filterM (fmap not . Ops.objectExistsForHash) (toList missing)
        if null missing' && size == fromIntegral (length comp)
          then do
            saveComponent h (toList comp)
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
getTerm doGetDeclType (Reference.Id h i) =
  runMaybeT do
    term2 <- Ops.loadTermByReference (C.Reference.Id h i)
    lift (Cv.term2to1 h doGetDeclType term2)

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
getTypeOfTermImpl (Reference.Id h i) =
  runMaybeT do
    type2 <- Ops.loadTypeOfTermByTermReference (C.Reference.Id h i)
    pure (Cv.ttype2to1 type2)

getTermComponentWithTypes ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Hash ->
  Transaction (Maybe [(Term Symbol Ann, Type Symbol Ann)])
getTermComponentWithTypes doGetDeclType h =
  runMaybeT do
    tms <- Ops.loadTermComponent h
    for tms (bitraverse (lift . Cv.term2to1 h doGetDeclType) (pure . Cv.ttype2to1))

getTypeDeclaration :: Reference.Id -> Transaction (Maybe (Decl Symbol Ann))
getTypeDeclaration (Reference.Id h i) =
  runMaybeT do
    decl2 <- Ops.loadDeclByReference (C.Reference.Id h i)
    pure (Cv.decl2to1 h decl2)

getDeclComponent :: Hash -> Transaction (Maybe [Decl Symbol Ann])
getDeclComponent h =
  runMaybeT do
    decl2 <- Ops.loadDeclComponent h
    pure (map (Cv.decl2to1 h) decl2)

putTermComponent ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  -- | The hash of the term component.
  Hash ->
  [(Term Symbol Ann, Type Symbol Ann)] ->
  Transaction ()
putTermComponent termBuffer declBuffer h component =
  unlessM (Ops.objectExistsForHash h) do
    for_ (Reference.componentFor h component) \(ref, (tm, tp)) -> do
      putTerm_ termBuffer declBuffer ref tm tp
      tryFlushTermBuffer termBuffer h

putTerm ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Term Symbol Ann ->
  Type Symbol Ann ->
  Transaction ()
putTerm termBuffer declBuffer ref@(Reference.Id h _) tm tp =
  unlessM (Ops.objectExistsForHash h) do
    putTerm_ termBuffer declBuffer ref tm tp
    tryFlushTermBuffer termBuffer h

putTerm_ ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Term Symbol Ann ->
  Type Symbol Ann ->
  Transaction ()
putTerm_ termBuffer declBuffer (Reference.Id h i) tm tp = do
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
      (fmap not . Ops.objectExistsForHash)
      [h | Reference.Derived h _i <- termDependencies]
  missingTypes' <-
    filterM (fmap not . Ops.objectExistsForHash) $
      [h | Reference.Derived h _i <- Set.toList $ Term.typeDependencies tm]
        ++ [h | Reference.Derived h _i <- Set.toList $ Type.dependencies tp]
  let missing' = missing <> Set.fromList (missingTerms' <> missingTypes')
  Sqlite.unsafeIO do
    -- notify each of the dependencies that h depends on them.
    traverse_ (addBufferDependent h termBuffer) missingTerms'
    traverse_ (addBufferDependent h declBuffer) missingTypes'
    putBuffer termBuffer h (BufferEntry size' comp' missing' waiting)

tryFlushTermBuffer :: TVar (Map Hash TermBufferEntry) -> Hash -> Transaction ()
tryFlushTermBuffer termBuffer =
  let loop h =
        tryFlushBuffer
          termBuffer
          (\h2 component -> void $ saveTermComponent Nothing h2 (Cv.termComponent1to2 h component))
          loop
          h
   in loop

addDeclComponentTypeIndex :: ObjectId -> [[Type Symbol Ann]] -> Transaction ()
addDeclComponentTypeIndex oId ctorss =
  for_ (ctorss `zip` [0 ..]) \(ctors, i) ->
    for_ (ctors `zip` [0 ..]) \(tp, j) -> do
      let self = C.Referent.ConId (C.Reference.Id oId i) j
          typeForIndexing = Hashing.typeToReference tp
          typeMentionsForIndexing = Hashing.typeToReferenceMentions tp
      Ops.addTypeToIndexForTerm self (Cv.reference1to2 typeForIndexing)
      Ops.addTypeMentionsToIndexForTerm self (Set.map Cv.reference1to2 typeMentionsForIndexing)

putTypeDeclarationComponent ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Hash ->
  [Decl Symbol Ann] ->
  Transaction ()
putTypeDeclarationComponent termBuffer declBuffer h decls =
  unlessM (Ops.objectExistsForHash h) do
    for_ (Reference.componentFor h decls) \(ref, decl) ->
      putTypeDeclaration_ declBuffer ref decl
    tryFlushDeclBuffer termBuffer declBuffer h

putTypeDeclaration ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Decl Symbol Ann ->
  Transaction ()
putTypeDeclaration termBuffer declBuffer ref@(Reference.Id h _) decl = do
  unlessM (Ops.objectExistsForHash h) do
    putTypeDeclaration_ declBuffer ref decl
    tryFlushDeclBuffer termBuffer declBuffer h

putTypeDeclaration_ ::
  TVar (Map Hash DeclBufferEntry) ->
  Reference.Id ->
  Decl Symbol Ann ->
  Transaction ()
putTypeDeclaration_ declBuffer (Reference.Id h i) decl = do
  BufferEntry size comp missing waiting <- Sqlite.unsafeIO (getBuffer declBuffer h)
  let declDependencies = Set.toList $ Decl.declDependencies decl
  let size' = max size (Just $ biggestSelfReference + 1)
        where
          biggestSelfReference =
            maximum1 $
              i :| [i' | Reference.Derived h' i' <- declDependencies, h == h']
  let comp' = Map.insert i decl comp
  moreMissing <-
    filterM (fmap not . Ops.objectExistsForHash) $
      [h | Reference.Derived h _i <- declDependencies]
  let missing' = missing <> Set.fromList moreMissing
  Sqlite.unsafeIO do
    traverse_ (addBufferDependent h declBuffer) moreMissing
    putBuffer declBuffer h (BufferEntry size' comp' missing' waiting)

tryFlushDeclBuffer ::
  TVar (Map Hash TermBufferEntry) ->
  TVar (Map Hash DeclBufferEntry) ->
  Hash ->
  Transaction ()
tryFlushDeclBuffer termBuffer declBuffer =
  let loop h =
        tryFlushBuffer
          declBuffer
          ( \h2 component ->
              void $
                saveDeclComponent
                  Nothing
                  h2
                  (fmap (Cv.decl1to2 h) component)
          )
          (\h -> tryFlushTermBuffer termBuffer h >> loop h)
          h
   in loop

uncachedLoadRootBranch ::
  BranchCache Sqlite.Transaction ->
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  Transaction (Branch Transaction)
uncachedLoadRootBranch branchCache getDeclType = do
  causal2 <- Ops.expectRootCausal
  Cv.causalbranch2to1 branchCache getDeclType causal2

-- | Get whether the root branch exists.
getRootBranchExists :: Transaction Bool
getRootBranchExists =
  isJust <$> Ops.loadRootCausalHash

putRootBranch :: Branch Transaction -> Transaction ()
putRootBranch branch1 = do
  -- todo: check to see if root namespace hash has been externally modified
  -- and do something (merge?) it if necessary. But for now, we just overwrite it.
  void (Ops.saveRootBranch v2HashHandle (Cv.causalbranch1to2 branch1))

-- if this blows up on cromulent hashes, then switch from `hashToHashId`
-- to one that returns Maybe.
getBranchForHash ::
  -- | A 'getDeclType'-like lookup, possibly backed by a cache.
  BranchCache Sqlite.Transaction ->
  (C.Reference.Reference -> Transaction CT.ConstructorType) ->
  Branch.CausalHash ->
  Transaction (Maybe (Branch Transaction))
getBranchForHash branchCache doGetDeclType h = do
  Ops.loadCausalBranchByCausalHash (Cv.causalHash1to2 h) >>= \case
    Nothing -> pure Nothing
    Just causal2 -> do
      branch1 <- Cv.causalbranch2to1 branchCache doGetDeclType causal2
      pure (Just branch1)

putBranch :: Branch Transaction -> Transaction ()
putBranch =
  void . Ops.saveBranch v2HashHandle . Cv.causalbranch1to2

-- | Check whether the given branch exists in the codebase.
branchExists :: Branch.CausalHash -> Transaction Bool
branchExists (Causal.CausalHash h) =
  Q.loadHashIdByHash h >>= \case
    Nothing -> pure False
    Just hId -> Q.isCausalHash hId

getPatch :: Branch.EditHash -> Transaction (Maybe Patch)
getPatch h =
  runMaybeT do
    patchId <- MaybeT (Q.loadPatchObjectIdForPrimaryHash (Cv.patchHash1to2 h))
    patch <- lift (Ops.expectPatch patchId)
    pure (Cv.patch2to1 patch)

-- | Put a patch into the codebase.
--
-- Note that 'putBranch' may also put patches.
putPatch :: Branch.EditHash -> Patch -> Transaction ()
putPatch h p =
  void $ Ops.savePatch v2HashHandle (Cv.patchHash1to2 h) (Cv.patch1to2 p)

-- | Check whether the given patch exists in the codebase.
patchExists :: Branch.EditHash -> Transaction Bool
patchExists h = fmap isJust $ Q.loadPatchObjectIdForPrimaryHash (Cv.patchHash1to2 h)

dependentsImpl :: Q.DependentsSelector -> Reference -> Transaction (Set Reference.Id)
dependentsImpl selector r =
  Set.map Cv.referenceid2to1
    <$> Ops.dependents selector (Cv.reference1to2 r)

dependentsOfComponentImpl :: Hash -> Transaction (Set Reference.Id)
dependentsOfComponentImpl h =
  Set.map Cv.referenceid2to1 <$> Ops.dependentsOfComponent h

-- | @watches k@ returns all of the references @r@ that were previously put by a @putWatch k r t@. @t@ can be
-- retrieved by @getWatch k r@.
watches :: UF.WatchKind -> Transaction [Reference.Id]
watches w =
  Ops.listWatches (Cv.watchKind1to2 w) <&> fmap Cv.referenceid2to1

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

-- | @putWatch k r t@ puts a watch of kind @k@, with hash-of-expression @r@ and decompiled result @t@ into the
-- codebase.
--
-- For example, in the watch expression below, @k@ is 'WK.Regular', @r@ is the hash of @x@, and @t@ is @7@.
--
-- @
-- > x = 3 + 4
--   ⧩
--   7
-- @
putWatch :: UF.WatchKind -> Reference.Id -> Term Symbol Ann -> Transaction ()
putWatch k r@(Reference.Id h _i) tm =
  when (elem k standardWatchKinds) do
    Ops.saveWatch
      (Cv.watchKind1to2 k)
      (Cv.referenceid1to2 r)
      (Cv.term1to2 h tm)

standardWatchKinds :: [UF.WatchKind]
standardWatchKinds = [UF.RegularWatch, UF.TestWatch]

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

-- | The number of base32 characters needed to distinguish any two references in the codebase.
hashLength :: Transaction Int
hashLength = pure 10

-- | The number of base32 characters needed to distinguish any two branch in the codebase.
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

-- | Get the set of type declarations whose hash matches the given prefix.
typeReferencesByPrefix :: ShortHash -> Transaction (Set Reference.Id)
typeReferencesByPrefix = defnReferencesByPrefix OT.DeclComponent

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
  cid' <- case cid of
    Nothing -> pure Nothing
    Just c ->
      case readMaybe (Text.unpack c) of
        Nothing -> error $ reportBug "994787297" "cid of ShortHash must be an integer but got: " <> show cid
        Just cInt -> pure $ Just cInt
  declReferents' <- Ops.declReferentsByPrefix prefix cycle cid'
  let declReferents =
        [ Referent.ConId (ConstructorReference (Reference.Id h pos) (fromIntegral cid)) (Cv.decltype2to1 ct)
          | (h, pos, ct, cids) <- declReferents',
            cid <- cids
        ]
  pure . Set.fromList $ termReferents <> declReferents

-- | Get the set of branches whose hash matches the given prefix.
causalHashesByPrefix :: ShortCausalHash -> Transaction (Set Branch.CausalHash)
causalHashesByPrefix sh = do
  -- given that a Branch is shallow, it's really `CausalHash` that you'd
  -- refer to to specify a full namespace w/ history.
  -- but do we want to be able to refer to a namespace without its history?
  cs <- Ops.causalHashesByPrefix (Cv.sch1to2 sh)
  pure $ Set.map (Causal.CausalHash . unCausalHash) cs

-- returns `Nothing` to not implemented, fallback to in-memory
--    also `Nothing` if no LCA
-- The result is undefined if the two hashes are not in the codebase.
-- Use `Codebase.lca` which wraps this in a nice API.
sqlLca :: Branch.CausalHash -> Branch.CausalHash -> Transaction (Maybe Branch.CausalHash)
sqlLca h1 h2 = do
  h3 <- Ops.lca (Cv.causalHash1to2 h1) (Cv.causalHash1to2 h2)
  pure (Cv.causalHash2to1 <$> h3)

-- well one or the other. :zany_face: the thinking being that they wouldn't hash-collide
termExists, declExists :: Hash -> Transaction Bool
termExists = fmap isJust . Q.loadObjectIdForPrimaryHash
declExists = termExists

-- `before b1 b2` is undefined if `b2` not in the codebase
before :: Branch.CausalHash -> Branch.CausalHash -> Transaction Bool
before h1 h2 =
  fromJust <$> Ops.before (Cv.causalHash1to2 h1) (Cv.causalHash1to2 h2)

-- | Construct a 'ScopedNames' which can produce names which are relative to the provided
-- Path.
--
-- NOTE: this method requires an up-to-date name lookup index, which is
-- currently not kept up-to-date automatically (because it's slow to do so).
namesAtPath ::
  Path ->
  Transaction ScopedNames
namesAtPath path = do
  let namespace = if path == Path.empty then Nothing else Just $ tShow path
  NamesByPath {termNamesInPath, termNamesExternalToPath, typeNamesInPath, typeNamesExternalToPath} <- Ops.rootNamesByPath namespace
  let termsInPath = convertTerms termNamesInPath
  let typesInPath = convertTypes typeNamesInPath
  let termsOutsidePath = convertTerms termNamesExternalToPath
  let typesOutsidePath = convertTypes typeNamesExternalToPath
  let allTerms :: [(Name, Referent.Referent)]
      allTerms = termsInPath <> termsOutsidePath
  let allTypes :: [(Name, Reference.Reference)]
      allTypes = typesInPath <> typesOutsidePath
  let rootTerms = Rel.fromList allTerms
  let rootTypes = Rel.fromList allTypes
  let absoluteRootNames = Names.makeAbsolute $ Names {terms = rootTerms, types = rootTypes}
  let absoluteExternalNames = Names.makeAbsolute $ Names {terms = Rel.fromList termsOutsidePath, types = Rel.fromList typesOutsidePath}
  let relativeScopedNames =
        case path of
          Path.Empty -> (Names.makeRelative $ absoluteRootNames)
          p ->
            let reversedPathSegments = reverse . Path.toList $ p
                relativeTerms = stripPathPrefix reversedPathSegments <$> termsInPath
                relativeTypes = stripPathPrefix reversedPathSegments <$> typesInPath
             in (Names {terms = Rel.fromList relativeTerms, types = Rel.fromList relativeTypes})
  pure $
    ScopedNames
      { absoluteExternalNames,
        relativeScopedNames,
        absoluteRootNames
      }
  where
    convertTypes names =
      names <&> \(S.NamedRef {reversedSegments, ref}) ->
        (Name.fromReverseSegments (coerce reversedSegments), Cv.reference2to1 ref)
    convertTerms names =
      names <&> \(S.NamedRef {reversedSegments, ref = (ref, ct)}) ->
        let v1ref = runIdentity $ Cv.referent2to1 (const . pure . Cv.constructorType2to1 . fromMaybe (error "Required constructor type for constructor but it was null") $ ct) ref
         in (Name.fromReverseSegments (coerce reversedSegments), v1ref)

    -- If the given prefix matches the given name, the prefix is stripped and it's collected
    -- on the left, otherwise it's left as-is and collected on the right.
    -- >>> stripPathPrefix ["b", "a"] ("a.b.c", ())
    -- ([(c,())])
    stripPathPrefix :: [NameSegment] -> (Name, r) -> (Name, r)
    stripPathPrefix reversedPathSegments (n, ref) =
      case Name.stripReversedPrefix n reversedPathSegments of
        Nothing -> error $ "Expected name to be in namespace" <> show (n, reverse reversedPathSegments)
        Just stripped -> (Name.makeRelative stripped, ref)

-- | Update the root namespace names index which is used by the share server for serving api
-- requests.
updateNameLookupIndex ::
  (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) ->
  Path ->
  -- | "from" branch, if 'Nothing' use the empty branch
  Maybe BranchHash ->
  -- | "to" branch
  BranchHash ->
  Sqlite.Transaction ()
updateNameLookupIndex getDeclType pathPrefix mayFromBranchHash toBranchHash = do
  fromBranch <- case mayFromBranchHash of
    Nothing -> pure V2Branch.empty
    Just fromBH -> Ops.expectBranchByBranchHash fromBH
  toBranch <- Ops.expectBranchByBranchHash toBranchHash
  treeDiff <- BranchDiff.diffBranches fromBranch toBranch
  let namePrefix = case pathPrefix of
        Path.Empty -> Nothing
        (p Path.:< ps) -> Just $ Name.fromSegments (p :| Path.toList ps)
  let BranchDiff.NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals} = BranchDiff.nameChanges namePrefix treeDiff
  termNameAddsWithCT <- do
    for termNameAdds \(name, ref) -> do
      refWithCT <- addReferentCT ref
      pure $ toNamedRef (name, refWithCT)
  Ops.updateNameIndex (termNameAddsWithCT, toNamedRef <$> termNameRemovals) (toNamedRef <$> typeNameAdds, toNamedRef <$> typeNameRemovals)
  where
    toNamedRef :: (Name, ref) -> S.NamedRef ref
    toNamedRef (name, ref) = S.NamedRef {reversedSegments = coerce $ Name.reverseSegments name, ref = ref}
    addReferentCT :: C.Referent.Referent -> Transaction (C.Referent.Referent, Maybe C.Referent.ConstructorType)
    addReferentCT referent = case referent of
      C.Referent.Ref {} -> pure (referent, Nothing)
      C.Referent.Con ref _conId -> do
        ct <- getDeclType ref
        pure (referent, Just $ Cv.constructorType1to2 ct)

-- | Compute the root namespace names index which is used by the share server for serving api
-- requests. Using 'updateNameLookupIndex' is preferred whenever possible, since it's
-- considerably faster. This can be used to reset the index if it ever gets out of sync due to
-- a bug.
--
-- This version can be used if you've already got the root Branch pre-loaded, otherwise
-- it's faster to use 'initializeNameLookupIndexFromV2Root'
initializeNameLookupIndexFromV1Branch :: Branch Transaction -> Sqlite.Transaction ()
initializeNameLookupIndexFromV1Branch root = do
  Q.dropNameLookupTables
  saveRootNamesIndexV1 (V1Branch.toNames . Branch.head $ root)
  where
    saveRootNamesIndexV1 :: Names -> Transaction ()
    saveRootNamesIndexV1 Names {Names.terms, Names.types} = do
      let termNames :: [(S.NamedRef (C.Referent.Referent, Maybe C.Referent.ConstructorType))]
          termNames = Rel.toList terms <&> \(name, ref) -> S.NamedRef {reversedSegments = nameSegments name, ref = splitReferent ref}
      let typeNames :: [(S.NamedRef C.Reference.Reference)]
          typeNames =
            Rel.toList types
              <&> ( \(name, ref) ->
                      S.NamedRef {reversedSegments = nameSegments name, ref = Cv.reference1to2 ref}
                  )
      Ops.updateNameIndex (termNames, []) (typeNames, [])
      where
        nameSegments :: Name -> NonEmpty Text
        nameSegments = coerce @(NonEmpty NameSegment) @(NonEmpty Text) . Name.reverseSegments
        splitReferent :: Referent.Referent -> (C.Referent.Referent, Maybe C.Referent.ConstructorType)
        splitReferent referent = case referent of
          Referent.Ref {} -> (Cv.referent1to2 referent, Nothing)
          Referent.Con _ref ct -> (Cv.referent1to2 referent, Just (Cv.constructorType1to2 ct))

-- | Compute the root namespace names index which is used by the share server for serving api
-- requests. Using 'updateNameLookupIndex' is preferred whenever possible, since it's
-- considerably faster. This can be used to reset the index if it ever gets out of sync due to
-- a bug.
--
-- This version should be used if you don't already have the root Branch pre-loaded,
-- If you do, use 'initializeNameLookupIndexFromV1Branch' instead.
initializeNameLookupIndexFromV2Root :: (C.Reference.Reference -> Sqlite.Transaction CT.ConstructorType) -> Sqlite.Transaction ()
initializeNameLookupIndexFromV2Root getDeclType = do
  Q.dropNameLookupTables
  rootHash <- Ops.expectRootCausalHash
  causalBranch <- Ops.expectCausalBranchByCausalHash rootHash
  (termNameMap, typeNameMap) <- nameMapsFromV2Branch [] causalBranch
  let expandedTermNames = Map.toList termNameMap >>= (\(name, refs) -> (name,) <$> Set.toList refs)
  termNameList <- do
    for expandedTermNames \(name, ref) -> do
      refWithCT <- addReferentCT ref
      pure S.NamedRef {S.reversedSegments = coerce name, S.ref = refWithCT}
  let typeNameList = do
        (name, refs) <- Map.toList typeNameMap
        ref <- Set.toList refs
        pure $ S.NamedRef {S.reversedSegments = coerce name, S.ref = ref}
  Ops.updateNameIndex (termNameList, []) (typeNameList, [])
  where
    addReferentCT :: C.Referent.Referent -> Transaction (C.Referent.Referent, Maybe C.Referent.ConstructorType)
    addReferentCT referent = case referent of
      C.Referent.Ref {} -> pure (referent, Nothing)
      C.Referent.Con ref _conId -> do
        ct <- getDeclType ref
        pure (referent, Just $ Cv.constructorType1to2 ct)

    -- Traverse a v2 branch
    -- Collects two maps, one with all term names and one with all type names.
    -- Note that unlike the `Name` type in `unison-core1`, this list of name segments is
    -- in reverse order, e.g. `["map", "List", "base"]`
    nameMapsFromV2Branch :: Monad m => [V2Branch.NameSegment] -> V2Branch.CausalBranch m -> m (Map (NonEmpty V2Branch.NameSegment) (Set C.Referent.Referent), Map (NonEmpty V2Branch.NameSegment) (Set C.Reference.Reference))
    nameMapsFromV2Branch reversedNamePrefix cb = do
      b <- V2Causal.value cb
      let (shallowTermNames, shallowTypeNames) = (Map.keysSet <$> V2Branch.terms b, Map.keysSet <$> V2Branch.types b)
      (prefixedChildTerms, prefixedChildTypes) <-
        fold <$> (ifor (V2Branch.children b) $ \nameSegment cb -> (nameMapsFromV2Branch (nameSegment : reversedNamePrefix) cb))
      pure (Map.mapKeys (NEList.:| reversedNamePrefix) shallowTermNames <> prefixedChildTerms, Map.mapKeys (NEList.:| reversedNamePrefix) shallowTypeNames <> prefixedChildTypes)

-- | Given a transaction, return a transaction that first checks a semispace cache of the given size.
--
-- The transaction should probably be read-only, as we (of course) don't hit SQLite on a cache hit.
makeCachedTransaction :: (Ord a, MonadIO m) => Word -> (a -> Sqlite.Transaction b) -> m (a -> Sqlite.Transaction b)
makeCachedTransaction size action = do
  cache <- Cache.semispaceCache size
  pure \x -> do
    conn <- Sqlite.unsafeGetConnection
    Sqlite.unsafeIO (Cache.apply cache (\x -> Sqlite.unsafeUnTransaction (action x) conn) x)

-- | Like 'makeCachedTransaction', but for when the transaction returns a Maybe; only cache the Justs.
makeMaybeCachedTransaction ::
  (Ord a, MonadIO m) =>
  Word ->
  (a -> Sqlite.Transaction (Maybe b)) ->
  m (a -> Sqlite.Transaction (Maybe b))
makeMaybeCachedTransaction size action = do
  cache <- Cache.semispaceCache size
  pure \x -> do
    conn <- Sqlite.unsafeGetConnection
    Sqlite.unsafeIO (Cache.applyDefined cache (\x -> Sqlite.unsafeUnTransaction (action x) conn) x)
