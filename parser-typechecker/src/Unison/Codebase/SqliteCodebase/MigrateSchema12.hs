{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.SqliteCodebase.MigrateSchema12 where

import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import U.Codebase.Sqlite.Connection (Connection)
import U.Codebase.Sqlite.DbId (CausalHashId, HashId, ObjectId)
import U.Codebase.Sqlite.ObjectType (ObjectType)
import qualified U.Codebase.Sqlite.ObjectType as OT
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S.Reference
import U.Codebase.Sync (Sync (Sync))
import qualified U.Codebase.Sync as Sync
import qualified U.Codebase.WatchKind as WK
import Unison.Prelude (ByteString, Map, MonadIO)
import Unison.Reference (Pos)
import Unison.Referent (ConstructorId)
import Data.Set (Set)
import qualified U.Codebase.Sqlite.Operations as Ops

-- lookupCtor :: ConstructorMapping -> ObjectId -> Pos -> ConstructorId -> Maybe (Pos, ConstructorId)
-- lookupCtor (ConstructorMapping cm) oid pos cid =
--   Map.lookup oid cm >>= (Vector.!? fromIntegral pos) >>= (Vector.!? cid)

-- lookupTermRef :: TermLookup -> S.Reference -> Maybe S.Reference
-- lookupTermRef _tl (ReferenceBuiltin t) = Just (ReferenceBuiltin t)
-- lookupTermRef tl (ReferenceDerived id) = ReferenceDerived <$> lookupTermRefId tl id

-- lookupTermRefId :: TermLookup -> S.Reference.Id -> Maybe S.Reference.Id
-- lookupTermRefId tl (Id oid pos) = Id oid <$> lookupTermPos tl oid pos

-- lookupTermPos :: TermLookup -> ObjectId -> Pos -> Maybe Pos
-- lookupTermPos (TermLookup tl) oid pos = Map.lookup oid tl >>= (Vector.!? fromIntegral pos)

-- newtype ConstructorMapping = ConstructorMapping (Map ObjectId (Vector (Vector (Pos, ConstructorId))))
-- newtype TermLookup = TermLookup (Map ObjectId (Vector Pos))

type TypeIdentifier = (ObjectId, Pos)
type Old a = a
type New a = a
data MigrationState = MigrationState
  -- Mapping between old cycle-position -> new cycle-position for a given Decl object.
  { -- declLookup :: Map (Old ObjectId) (Map (Old Pos) (New Pos)),
    declLookup :: Map (Old Reference.Id) (New Reference.Id),
    -- Mapping between contructor indexes for the type identified by (ObjectId, Pos)
    ctorLookup :: Map (Old TypeIdentifier) (Map (Old ConstructorId) (New ConstructorId)),
    ctorLookup' :: Map (Old Referent.Id) (New Referent.Id),
    -- This provides the info needed for rewriting a term.  You'll access it with a function :: Old
    termLookup :: Map (Old ObjectId) (New ObjectId, Map (Old Pos) (New Pos)),
    objLookup :: Map (Old ObjectId) (New ObjectId),


    --
    componentPositionMapping :: Map ObjectId (Map (Old Pos) (New Pos)),
    constructorIDMapping :: Map ObjectId (Map (Old ConstructorId) (New ConstructorId)),
    completed :: Set ObjectId
  } deriving Generic

  -- declLookup :: Map ObjectId (Map Pos (Pos, Map ConstructorId ConstructorId)),

{-
* Load entire codebase as a list
* Pick a term from the codebase
* Look up the references inside the term
* If any haven't been processed, add them to the "to process" stack, push the term you were working on back onto that stack
* Rebuild & rehash the term, store that
* For any data constructor terms inside,
  * Store a map from old ConstructorId to new, based on the old and new reference hashes
* After rebuilding a cycle, map old Pos to new
-}

-- Q: can we plan to hold the whole mapping in memory? ✅
-- Q: a) update database in-place? or b) write to separate database and then overwrite? leaning (b).
-- note: we do need to rebuild namespaces, although we don't need to rehash them.

-- cycle position index `Pos`
-- constructor index `ConstructorId`

{-
data Maybe a = (Just Bar | Nothing X)

-- changes due to missing size from ref(Y)
data X = MkX Y

-- know old hash and old cycle positions
data Y = MkY Int
-}

data Entity
  = O ObjectId
  | C CausalHashId
  | W WK.WatchKind S.Reference.IdH
  deriving (Eq, Ord, Show)

data Env = Env {db :: Connection}

--  -> m (TrySyncResult h)
migrationSync ::
  (MonadIO m, MonadState MigrationState m, MonadReader Env m) =>
  Sync m Entity
migrationSync = Sync \case
  -- To sync an object,
  --   * If we have already synced it, we are done.
  --   * Otherwise, read the object from the database and switch on its object type.
  --   * See next steps below v
  --
  -- To sync a decl component object,
  --   * If we have not already synced all dependencies, push syncing them onto the front of the work queue.
  --   * Otherwise, ???
  --
  -- To sync a term component object,
  --   * If we have not already synced all dependencies, push syncing them onto the front of the work queue.
  --   * Otherwise, ???
  --
  -- To sync a namespace object,
  --   * Deserialize it and compute its dependencies (terms, types, patches, children).
  --   * If we have not already synced all of its dependencies, push syncing them onto the front of the work queue.
  --   * To sync a 'BranchFull',
  --     * We need to make a new 'BranchFull' in memory, then insert it into the database under a new object id.
  --       * Wait, we need to preserve the ordering of the types/terms, either by not changing them (but the orderings of the
  --         reference ids used in keys is definitely not preserved by this migration), or by permuting the local id vectors,
  --         but we may be at a level too low or high for us to care?
  --     * Its 'LocalBranch' must have all references changed in-place per the (old (object id, pos) => new (object id, pos)) mapping.
  --     * The local IDs within the body _likely_ don't need to change. (why likely?)
  --     * Its 'BranchLocalIds' must be translated from the old codebase object IDs to the new object IDs,
  --       we can use our MigrationState to look these up, since they must have already been migrated.
  --   * To sync a 'BranchDiff',
  --     * These don't exist in schema v1; we can error if we encounter one.
  --
  -- To sync a patch object
  --   * Rewrite all old hashes in the patch to the new hashes.
  --
  -- To sync a watch expression
  --   * ???
  --
  -- To sync a Causal
  --- * If we haven't yet synced its parents, push them onto the work queue
  --- * If we haven't yet synced the causal's value (namespace), push it onto the work queue.
  --- * Rehash the Causal's valueHash AND CausalHash, and add the new causal, its hashes, and hash objects to the codebase under a fresh object ID
  O objId -> do
    let alreadySynced :: m Bool
        alreadySynced = undefined
    alreadySynced >>= \case
      False -> do
        (hId, objType, bytes) <- runSrc $ Q.loadObjectWithHashIdAndTypeById oId
        migrateObject objType hId bytes
      True -> pure Sync.PreviouslyDone
  -- result <- runValidateT @(Set Entity) @m @ObjectId case objType of
  -- To sync a causal,
  --   1. ???
  --   2. Synced
  C causalHashID -> _
  -- To sync a watch result,
  --   1. ???
  --   2. Synced
  W watchKind idH -> _

-- data ObjectType
--   = TermComponent -- 0
--   | DeclComponent -- 1
--   | Namespace -- 2
--   | Patch -- 3

migrateObject :: ObjectType -> HashId -> ByteString -> m _
migrateObject objType hash bytes = case objType of
  OT.TermComponent -> migrateTermComponent hash bytes
  OT.DeclComponent -> migrateDeclComponent hash bytes
  OT.Namespace -> migrateNamespace hash bytes
  OT.Patch -> migratePatch hash bytes

migratePatch :: HashId -> ByteString -> m _
migratePatch = error "not implemented"

migrateNamespace :: HashId -> ByteString -> m _
migrateNamespace = error "not implemented"

migrateTermComponent :: HashId -> ByteString -> m _
migrateTermComponent = error "not implemented"

migrateDeclComponent :: EDB m => Codebase m v a -> HashId -> m _
migrateDeclComponent Codebase{..} hashId = do
  hash <- Ops.loadHashByHashId hashId
  declComponent :: [Decl v a] <- Codebase.getDeclComponent (Cv.hash2to1 hash) >>= \case
    Nothing -> error "handle this" -- not non-fatal! 
    Just dc -> pure dc
  
  -- type Decl = Either EffectDeclaration DataDeclaration
  let componentIDMap :: Map Reference.Id (Decl v a)
      componentIDMap = Map.fromList $ Reference.componentFor hash declComponent

  let unhashed :: Map Reference.Id (v, Decl v a)
      unhashed = DD.unhashComponent componentIDMap
--  data DataDeclaration v a = DataDeclaration {
--   modifier :: Modifier,
--   annotation :: a,
--   bound :: [v],
--   constructors' :: [(a, v, Type v a)]
-- } deriving (Eq, Show, Functor)
 
  let allTypes :: [Type v a]
      allTypes = (\(_, _, typ) -> typ) . concatMap (constructors' . snd) . Map.values $ unhashed
  let allContainedReferences :: [Reference.Id]
      allContainedReferences = foldMap (ABT.find findReferenceIds) (constructors)
  


-- get references:
--
--   references :: Term f v a -> [Reference.Id]
--
-- are all those references keys in our skymap?
--   yes => migrate term
--   no => returh those references (as Entity, though) as more work to do 

-- how to turn Reference.Id into Entity?
--   need its ObjectId,

-- Term f v a -> ValidateT (Seq Reference.Id) m (Term f v a)
-- 
recordRefsInType :: MonadState MigrationState m => Type v a -> WriterT [Reference.Id] m (Type v a)
recordRefsInType = _

findReferenceIds :: Type v a -> FindAction Reference.Id
findReferenceIds = Term.out >>> \case
  Tm (Ref (Reference.DerivedId r)) -> Found r
  x -> Continue




-- data DataDeclaration v a = DataDeclaration {
--   modifier :: Modifier,
--   annotation :: a,
--   bound :: [v],
--   constructors' :: [(a, v, Type v a)]
-- } deriving (Eq, Show, Functor)


-- compute correspondence between `v`s in `fst <$> named` compared to `fst <$> new_references` to get a Reference.Id -> Reference.Id mapping
-- mitchell tapped out before understanding the following line
-- compute correspondence between constructors names & constructor indices in corresponding decls
-- submit/mappend these two correspondences to sky mapping

-- Swap the Reference positions according to our map of already computed swaps
-- Hydrate into the parser-typechecker version, get the new hash
-- reserialize it into the sqlite format
-- Compare the old and new sqlite versions to add those ConstructorID/Pos mappings to our context.

-- unrelated Q:
--   do we kinda have circular dependency issues here?
--   parser-typechecker depends on codebase2, but we are talking about doing things at the parser-typechecker level in this migration
--   answer: no
  
  -- unhashComponent
  -- :: forall v a. Var v => Map Reference.Id (Decl v a) -> Map Reference.Id (v, Decl v a)

  let dataDecls = fmap (either toDataDecl id) declComponent
  -- DD.unhashComponent
  
  
  -- [OldDecl] ==map==> [NewDecl] ==number==> [(NewDecl, Int)] ==sort==> [(NewDecl, Int)] ==> permutation is map snd of that


-- type List a = Nil | Cons (List a)

-- unique type Thunk = Thunk (Int ->{MakeThunk} Int)
-- ability MakeThunk where go : (Int -> Int) -> Thunk

-- What mitchell thinks unhashComponent is doing:
--
--  Take a recursive type like
--
--     Fix \myself -> Alternatives [Nil, Cons a myself]
--
--  And write it with variables in place of recursive mentions like
--
--     (Var 1, Alternatives [Nil, Cons a (Var 1)]  

-- can derive `original` from Hash + [OldDecl]
-- original :: Map Reference.Id (Decl v a)

-- named, rewritten_dependencies :: Map (Reference.Id {old}) (v, Decl v a {old pos in references})
-- named = Decl.unhashComponent original

-- Mapping from the sky: (Reference.Id -> Reference.Id)

-- rewritten_dependencies = replace_dependency_pos's skymap named

-- new_references :: Map v (Reference.Id {new}, DataDeclaration v a)
-- new_references = Unison.Hashing.V2.Convert.hashDecls $ Map.toList $ Foldable.toList rewritten_dependencies

  
  


  -- let DeclFormat locallyIndexedComponent = case runGetS S.getDeclFormat declFormatBytes of
  --   Left err -> error "something went wrong"
  --   Right declFormat -> declFormat

  -- Operations.hs converts from S level to C level
  -- SqliteCodebase.hs converts from C level to 
  

-- | migrate sqlite codebase from version 1 to 2, return False and rollback on failure
migrateSchema12 :: Applicative m => Connection -> m Bool
migrateSchema12 db = do
  -- todo: drop and recreate corrected type/mentions index schema
  -- do we want to garbage collect at this time? ✅
  -- or just convert everything without going in dependency order? ✅
  error "todo: go through "
  -- todo: double-hash all the types and produce an constructor mapping
  -- object ids will stay the same
  -- todo: rehash all the terms using the new constructor mapping
  -- and adding the type to the term
  -- do we want to diff namespaces at this time? ❌
  -- do we want to look at supporting multiple simultaneous representations of objects at this time?
  pure "todo: migrate12"
  pure True

-- -- remember that the component order might be different
-- rehashDeclComponent :: [Decl v a] -> (Hash, ConstructorMappings)
-- rehashDeclComponent decls = fmap decls <&> \case
--
--     --
--     error "todo: rehashDeclComponent"

-- rewriteDeclComponent :: DeclFormat.LocallyIndexedComponent -> (Hash, DeclFormat.LocallyIndexedComponent, ConstructorMappings)
-- rewriteDeclComponent =
--     --
--     error "todo: rehashDeclComponent"

-- rehashDeclComponent :: [Decl v a] -> (Hash, DeclFormat.LocallyIndexedComponent, ConstructorMappings)

-- rehashTermComponent :: ConstructorMappings -> TermFormat.LocallyIndexedComponent -> (Hash, TermFormat.LocallyIndexedComponent)
-- rehashTermComponent = error "todo: rehashTermComponent"

-- -- getConstructor :: ConstructorMappings -> ObjectId -> Pos -> ConstructorId
-- -- getConstructor cm
