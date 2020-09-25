{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
module U.Codebase.Convert.SyncV1V2 where

import qualified U.Util.Hashable as H
import Database.SQLite.Simple.FromField (FromField)
import U.Util.Hash (Hash)
import qualified Unison.Codebase.V1.Branch.Raw as V1
import Data.Text (Text)
import Database.SQLite.Simple.ToField (ToField)
import qualified Unison.Codebase.V1.Reference as V1.Reference

newtype V1 a = V1 {runV1 :: a} deriving (Eq, Ord, Show)

newtype V2 a = V2 {runV2 :: a}
  deriving (Eq, Ord, Show, Functor)
  deriving (FromField, H.Accumulate, H.Hashable) via a

data V1EntityRef
  = Decl1 (V1 Hash) -- this will refer to the whole component
  | Term1 (V1 Hash) -- ditto
  | Patch1 (V1 V1.EditHash)
  | Branch1 (V1 V1.BranchHash)
  deriving (Eq, Ord, Show)

v1EntityRefToHash :: V1EntityRef -> V1 Hash
v1EntityRefToHash = \case
  Decl1 h -> h
  Term1 h -> h
  Patch1 (V1 (V1.EditHash h)) -> V1 h
  Branch1 (V1 (V1.BranchHash h)) -> V1 h

newtype Base32HexPrefix = Base32HexPrefix Text
  deriving (Show) via Text
  deriving (ToField) via Text
  deriving (FromField) via Text

-- newtype PatchHash h = PatchHash h
-- newtype NamespaceHash h = NamespaceHash h
newtype CausalHash h = CausalHash h

-- -- |things that appear in a deserialized RawBranch
-- type V2EntityRef =
--   V2EntityRefH
--     Hash
--     (PatchHash Hash)
--     (NamespaceHash Hash)
--     (CausalHash Hash)

-- -- -- |things that appear in a serialized RawBranch
-- -- type V2EntityRefS =
-- --     V2EntityRefH
-- --       Db.ObjectId
-- --       (PatchHash Db.ObjectId)
-- --       (NamespaceHash Db.NamespaceHashId)
-- --       (CausalHash Db.CausalHashId)

-- -- data V2EntityRefH hr hp hn hc
-- --   = Decl2 V2.ReferenceId
-- --   | Term2 Reference.Id
-- --   | Patch2 PatchHash
-- --   | NamespaceHash2 NamespaceHash
-- --   | CausalHash2 CausalHash

-- initializeV2DB :: MonadIO m => m ()
-- initializeV2DB = error "todo"

data FatalError
  = NoRootBranch
  | MissingBranch (V1 Hash)
  | MissingPatch (V1 Hash)
  | MissingTerm V1.Reference.Id
  | MissingTermHash (V1 Hash)
  | MissingTypeOfTerm V1.Reference.Id
  | MissingDecl V1.Reference.Id
  | MissingDeclHash (V1 Hash)
  | InvalidBranch (V1 Hash)
  | InvalidPatch (V1 Hash)
  | InvalidTerm V1.Reference.Id
  | InvalidTypeOfTerm V1.Reference.Id
  | InvalidDecl V1.Reference.Id

-- type Type = Type.Type Symbol Ann

-- type Term = Term.Term Symbol Ann

-- type Decl = DD.Decl Symbol Ann

-- type Patch = Patch.Patch V1.Reference

-- -- the H stands for "for hashing"
-- -- the S stands for "for serialization"
-- type Term2ComponentH = [Term2 Hash]

-- type Term2ComponentS = [Term2 Db.ObjectId]

-- type Decl2ComponentH = [Decl2 (Maybe Hash)]

-- type Decl2S = Decl2 Db.ObjectId
-- type Decl2ComponentS = [Decl2S]


-- -- these have maybes in them to indicate a self-component reference
-- type Term2 h = V2.Term h

-- type Decl2 h = DD.DeclR (V2.Reference h) Symbol ()

-- -- for indexing
-- type Decl2I = DD.DeclR (V2.Reference Db.ObjectId) Symbol ()

-- type Term2S = Term2 Db.ObjectId

-- type Type2S = V2.Type Db.ObjectId

-- -- what about referent types in the index?

-- -- type CtorType2S = Type.TypeH Db.ObjectId Symbol Ann
-- -- type Term2S = Term.TermH (Maybe Db.ObjectId) Symbol Ann
-- type Patch2S = Patch.Patch (V2.Reference Db.ObjectId)

-- --type Term2S = ABT.Term (Term.F' (Maybe TermId) DeclId (Type.TypeH DeclId Symbol ()) Void ()) Symbol ()
-- --alternative representation if embedded
-- --type Term2S = ABT.Term (Term.F' (Maybe TermId) DeclId TypeId Void ()) Symbol ()

-- fmtV :: S.Format Symbol
-- fmtV = S.V1.formatSymbol

-- getV :: S.Get Symbol
-- getV = S.get S.V1.formatSymbol

-- putV :: S.Put Symbol
-- putV = S.put fmtV

-- fmtA :: S.Format Ann
-- fmtA = V1.formatAnn

-- getA :: S.Get Ann
-- getA = S.get fmtA

-- putA :: S.Put Ann
-- putA = S.put fmtA

-- -- todo: this just converts a whole codebase, which we need to do locally \
-- --       but we also want some code that imports just a particular sub-branch.
-- syncV1V2 :: forall m. MonadIO m => Connection -> CodebasePath -> m (Either FatalError ())
-- syncV1V2 c rootDir = liftIO $ SQLite.withTransaction c . runExceptT . flip runReaderT c $ do
--   v1RootHash <- getV1RootBranchHash rootDir >>= maybe (throwError NoRootBranch) pure
--   -- starting from the root namespace, convert all entities you can find
--   convertEntities [Branch1 v1RootHash]
--   v2RootHash <- v2CausalHashForV1BranchHash v1RootHash
--   setV2Root v2RootHash
--   error "todo: compressEntities and vacuum db" v2RootHash

--   -- Incorporating diff construction into the conversion is tough because
--   -- a) I was thinking we'd represent an older version as a diff against the
--   --    newer version, but the newer version hasn't been fully constructed
--   --    until the older versions have been converted and hashed.
--   -- b) If we just store all the old versions uncompressed, it might be too big.
--   --    (But no bigger than the v1 db.) But if that is okay, we can compress and
--   --    vacuum them afterwards.

--   pure ()
--   where
--     setV2Root = error "todo: setV2Root"
--     v2CausalHashForV1BranchHash = error "todo: v2CausalHashForV1BranchHash"
--     convertEntities ::
--       forall m.
--       DB m =>
--       MonadError FatalError m =>
--       [V1EntityRef] ->
--       m ()
--     convertEntities [] = pure ()
--     convertEntities all@(h : rest) = do
--       termDirComponents <- componentMapForDir (V1.termsDir rootDir)
--       declsDirComponents <- componentMapForDir (V1.typesDir rootDir)
--       case h of
--         Term1 h ->
--           -- if this hash is already associated to an object
--           ifM (existsObjectWithHash (runV1 h)) (convertEntities rest) $ do
--             -- load a cycle from disk
--             e <- loadTerm1 rootDir termDirComponents h
--             matchTerm1Dependencies h e >>= \case
--               Left missing -> convertEntities (missing ++ all)
--               Right lookup -> do
--                 convertTerm1 lookup h e
--                 convertEntities rest
--         Decl1 h ->
--           ifM (existsObjectWithHash (runV1 h)) (convertEntities rest) $ do
--             d <- loadDecl1 rootDir declsDirComponents h
--             matchDecl1Dependencies h d >>= \case
--               Left missing -> convertEntities (missing ++ all)
--               Right lookup -> do
--                 convertDecl1 (error "todo: lookup") h d
--                 convertEntities rest
--         Patch1 h ->
--           ifM (existsObjectWithHash (runV1 h)) (convertEntities rest) $ do
--             p <- loadPatch1 rootDir h
--             matchPatch1Dependencies ("patch " ++ show h) p >>= \case
--               Left missing -> convertEntities (missing ++ all)
--               Right lookup -> do
--                 -- hashId <- Db.saveHashByteString (runV1 h)
--                 -- savePatch hashId (Patch.hmap (lookup . V1) p)
--                 error "todo"
--                 convertEntities rest
--         Branch1 (V1.unRawHash -> h) ->
--           ifM (existsObjectWithHash h) (convertEntities rest) $ do
--             cb <- loadCausalBranch1 rootDir (V1 h)
--             matchCausalBranch1Dependencies ("branch " ++ show h) cb >>= \case
--               Left missing -> convertEntities (missing ++ all)
--               Right (lookupObject, lookupCausal) -> do
--                 convertCausalBranch1 lookupObject lookupCausal cb
--                 convertEntities rest

-- -- | load a causal branch raw thingo
-- loadCausalBranch1 ::
--   MonadIO m =>
--   MonadError FatalError m =>
--   CodebasePath ->
--   V1 Hash ->
--   m (V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw)
-- loadCausalBranch1 rootDir h = do
--   let file = V1.branchPath rootDir (V1.RawHash (runV1 h))
--   ifM
--     (doesFileExist file)
--     ( S.getFromFile' (S.V1.getCausal0 S.V1.getRawBranch) file >>= \case
--         Left err -> throwError $ InvalidBranch h
--         Right c0 -> pure c0
--     )
--     (throwError $ MissingBranch h)

-- primaryHashByHash1 :: DB m => V2.ObjectType -> Hash -> m (Maybe Hash)
-- primaryHashByHash1 t h =
--   Db.query sql (t, Base32Hex.fromHash h) <&> \case
--     [Only h] -> Just (Base32Hex.toHash h)
--     [] -> Nothing
--     hs ->
--       error $
--         "hash2ForHash1 " ++ show t ++ " "
--           ++ take 10 (show h)
--           ++ " = "
--           ++ (show . map (take 10 . show)) hs
--   where
--     sql =
--       [here|
--     SELECT v2hash.base32
--     FROM hash AS v2hash
--     INNER JOIN object ON object.primary_hash_id = v2hash.id
--     INNER JOIN hash_object ON object.id = hash_object.object_id
--     INNER JOIN hash AS v1hash ON hash_object.hash_id = v1hash.id
--     WHERE object.type_id = ? AND v1hash.base32 = ?
--   |]

-- loadBranch1 ::
--   forall m.
--   MonadIO m =>
--   MonadError FatalError m =>
--   m V1.Branch.Raw
-- loadBranch1 = error "todo: loadBranch1"

-- --      ifM (not <$> doesFileExist (V1.branchPath root h))
-- --        (throwError $ MissingBranch h)
-- --        (do
-- --          branch1 <- loadBranch1
-- --          )

-- newtype MdValuesR r = MdValues (Set r)

-- deriving via
--   (Set r)
--   instance
--     Hashable r => Hashable (MdValuesR r)

-- -- this is the version we'll hash
-- type RawBranch =
--   RawBranchH
--     (V2.Referent Hash Hash) -- terms
--     (V2.Reference Hash) -- types
--     (V2.Reference Hash) -- metadata
--     (V2 Hash) -- patches
--     (V2 Hash) -- children

-- -- this is the version that closely corresponds to the db schema
-- type RawBranch2S =
--   RawBranchH
--     (V2.Referent Db.ObjectId Db.ObjectId) -- terms
--     (V2.Reference Db.ObjectId) -- types
--     (V2.Reference Db.ObjectId) -- metadata
--     Db.ObjectId -- patches
--     Db.CausalHashId -- children

-- data RawBranchH termRef typeRef mdRef pRef cRef = RawBranch
--   { terms :: Map (NameSegment, termRef) (Set mdRef),
--     types :: Map (NameSegment, typeRef) (Set mdRef),
--     patches :: Map NameSegment pRef,
--     children :: Map NameSegment cRef
--   }

-- type RawCausal = RawCausalH Db.CausalHashId Db.NamespaceHashId

-- data RawCausalH hCausal hValue = RawCausal
--   { causalHash :: hCausal,
--     valueHash :: hValue,
--     parents :: [hCausal]
--   }

-- instance Hashable RawBranch where
--   tokens b =
--     [ H.accumulateToken (terms b),
--       H.accumulateToken (types b),
--       H.accumulateToken (patches b),
--       H.accumulateToken (children b)
--     ]

-- instance Hashable RawCausal where
--   tokens c =
--     [ H.accumulateToken (causalHash c),
--       H.accumulateToken (valueHash c),
--       H.accumulateToken (parents c)
--     ]

-- getV1RootBranchHash :: MonadIO m => CodebasePath -> m (Maybe V1.Branch.Hash)
-- getV1RootBranchHash root = listDirectory (V1.branchHeadDir root) <&> \case
--   [single] -> Just . V1.Branch.Hash . Hash.unsafeFromBase32Hex $ Text.pack single
--   _ -> Nothing

-- -- | Look for an ObjectId corresponding to the provided V1 hash.
-- --  Returns Left if not found.
-- lookupObject :: DB m => V1EntityRef -> m (Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId)))
-- lookupObject r@(runV1 . v1EntityRefToHash -> h) =
--   getObjectIdByBase32Hex (Base32Hex.fromHash h) <&> \case
--     Nothing -> Left r
--     Just i -> Right (V1 h, i)

-- -- | Look for a CausalHashId corresponding to the provided V1 hash.
-- --  Returns Left if not found.
-- lookupCausal :: DB m => V1.Branch.Hash -> m (Either (V1 Hash) (V1 Hash, (V2 Hash, Db.CausalHashId)))
-- lookupCausal (V1.unRawHash -> h) =
--   Db.queryMaybe sql (Only (Base32Hex.fromHash h)) <&> \case
--     Nothing -> Left (V1 h)
--     Just (v2Hash, id) -> Right (V1 h, (Base32Hex.toHash <$> v2Hash, id))
--   where
--     sql =
--       [here|
--     SELECT new_hash.base32, new_hash_id
--     FROM causal_old
--     INNER JOIN hash old_hash ON old_hash_id = old_hash.id
--     INNER JOIN hash new_hash ON new_hash_id = new_hash.id
--     WHERE old_hash.base32 = ?
--   |]

-- -- | no Maybes here, as all relevant ObjectId can be known in advance
-- saveTypeBlobForReferent :: DB m => V2.ReferentId Db.ObjectId -> Type2S -> m ()
-- saveTypeBlobForReferent r type2s =
--   let blob = S.putBytes (S.V1.putTypeR (V2.putReference V2.putObjectId) putV V2.putUnit) type2s
--    in Db.saveTypeOfReferent r blob

-- -- | Multiple hashes can map to a single object!
-- getObjectIdByBase32Hex :: DB m => Base32Hex -> m (Maybe (V2 Hash, Db.ObjectId))
-- getObjectIdByBase32Hex h =
--   fmap (first (V2 . Base32Hex.toHash)) <$> Db.queryMaybe sql (Only h)
--   where
--     sql =
--       [here|
--     SELECT object.id
--     FROM hash
--     INNER JOIN hash_object ON hash_object.hash_id = hash.id
--     INNER JOIN object ON hash_object.object_id = object.id
--     WHERE hash.base32 = ?
--   |]

-- augmentLookup :: Ord a => (a -> b) -> Map a b -> a -> b
-- augmentLookup f m a = fromMaybe (f a) (Map.lookup a m)

-- saveReferenceAsReference2 :: DB m => Reference -> m (V2.Reference Db.HashId)
-- saveReferenceAsReference2 = mapMOf Db.referenceTraversal Db.saveHashByteString

-- -- | load a term component by its hash.
-- --  A v1 term component is split across an arbitrary number of files.
-- --  We have to 1) figure out what all the filenames are (preloaded into
-- --  `termDirComponents`), 2) load them all,
-- loadTerm1 ::
--   MonadIO m =>
--   MonadError FatalError m =>
--   CodebasePath ->
--   Map (V1 Hash) [Reference.Id] ->
--   V1 Hash ->
--   m [(Term, Type)]
-- loadTerm1 rootDir componentsFromDir h = case Map.lookup h componentsFromDir of
--   Nothing -> throwError $ MissingTermHash h
--   Just set -> case toList set of
--     [] -> error "Just [] shouldn't occur here."
--     Reference.Id h _i n : _etc -> for [0 .. n -1] \i -> do
--       let r = Reference.Id h i n
--       term <-
--         V1.FC.getTerm (S.get fmtV) (S.get fmtA) rootDir r
--           >>= maybe (throwError $ MissingTerm r) pure
--       typeOfTerm <-
--         V1.FC.getTypeOfTerm (S.get fmtV) (S.get fmtA) rootDir r
--           >>= maybe (throwError $ MissingTypeOfTerm r) pure
--       pure (term, typeOfTerm)

-- loadDecl1 ::
--   MonadIO m =>
--   MonadError FatalError m =>
--   CodebasePath ->
--   Map (V1 Hash) [Reference.Id] ->
--   V1 Hash ->
--   m [Decl]
-- loadDecl1 rootDir componentsFromDir h = case Map.lookup h componentsFromDir of
--   Nothing -> throwError $ MissingDeclHash h
--   Just set -> case toList set of
--     [] -> error "Just [] shouldn't occur here."
--     Reference.Id h _i n : _etc -> for [0 .. n -1] \i -> do
--       let r = Reference.Id h i n
--       V1.FC.getDecl (S.get fmtV) (S.get fmtA) rootDir r
--         >>= maybe (throwError $ MissingDecl r) pure

-- -- | load a patch
-- loadPatch1 :: (MonadIO m, MonadError FatalError m) => [Char] -> V1 Hash -> m (Patch.Patch Reference)
-- loadPatch1 rootDir h = do
--   let file = V1.editsPath rootDir (runV1 h)
--   ifM
--     (doesFileExist file)
--     ( S.getFromFile' S.V1.getEdits file >>= \case
--         Left _err -> throwError (InvalidPatch h)
--         Right edits -> pure edits
--     )
--     (throwError $ MissingPatch h)

-- -- 3) figure out what their combined dependencies are
-- matchTerm1Dependencies ::
--   DB m =>
--   V1 Hash ->
--   [(Term, Type)] ->
--   m (Either [V1EntityRef] (V1 Hash -> (V2 Hash, Db.ObjectId)))
-- matchTerm1Dependencies componentHash tms =
--   let -- Get a list of Eithers corresponding to the non-self dependencies of this term.
--       lookupDependencyObjects ::
--         DB m => (Term, Type) -> m [Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId))]
--       lookupDependencyObjects (term, typeOfTerm) = traverse lookupObject deps
--         where
--           (termTypeDeps, termTermDeps) =
--             partitionEithers
--               . map LD.toReference
--               . toList
--               $ Term.labeledDependencies term
--           deps =
--             nubOrd $
--               [Decl1 (V1 h) | Reference.Derived h _i _n <- toList $ Type.dependencies typeOfTerm]
--                 <> [Decl1 (V1 h) | Reference.Derived h _i _n <- termTypeDeps]
--                 <> [ Term1 (V1 h) | Reference.Derived h _i _n <- termTermDeps, h /= runV1 componentHash -- don't include self-refs 😬
--                    ]
--    in do
--         -- check the lefts, if empty then everything is on the right;
--         -- else return left.
--         (missing, found) <- partitionEithers <$> foldMapM lookupDependencyObjects tms
--         pure $ case missing of
--           [] -> Right (makeLookup found $ "term " ++ show componentHash)
--           missing -> Left missing

-- matchDecl1Dependencies ::
--   DB m => V1 Hash -> [Decl] -> m (Either [V1EntityRef] (V1 Hash -> Db.ObjectId))
-- matchDecl1Dependencies componentHash decls = error "todo" -- let
-- -- lookupDependencyObjects
-- --   :: DB m => Decl -> m [Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId))]
-- -- lookupDependencyObjects decl = traverse lookupObject . nubOrd $
-- --   [ Decl1 (V1 h) | Reference.Derived h _i _n <- toList (DD.declDependencies decl)
-- --             , V1 h /= componentHash ]
-- -- in do
-- --   (missing, found) <- partitionEithers <$> foldMapM lookupDependencyObjects decls
-- --   pure $ case missing of
-- --     [] -> Right (makeLookup found $ "decl " ++ show componentHash)
-- --     missing -> Left missing

-- matchPatch1Dependencies ::
--   DB m =>
--   String ->
--   Patch ->
--   m (Either [V1EntityRef] (V1 Hash -> (V2 Hash, Db.ObjectId)))
-- matchPatch1Dependencies description (Patch.Patch tms tps) = do
--   deps :: [Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId))] <-
--     traverse lookupObject . nubOrd $
--       [ Term1 (V1 h) | (r, e) <- Relation.toList tms, Reference.Derived h _i _n <- r : TermEdit.references e
--       ]
--         ++ [ Decl1 (V1 h) | (r, e) <- Relation.toList tps, Reference.Derived h _i _n <- r : TypeEdit.references e
--            ]
--   let (missing, found) = partitionEithers deps
--   pure $ case missing of
--     [] -> Right (makeLookup found description)
--     missing -> Left missing

-- -- | multiple lookups needed in converting branch
-- --  data CBDepLookup
-- matchCausalBranch1Dependencies ::
--   DB m =>
--   String ->
--   V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw ->
--   m (Either [V1EntityRef] (V1 Hash -> Db.ObjectId, V1 Hash -> (V2 Hash, Db.CausalHashId)))
-- matchCausalBranch1Dependencies description cb@(V1.Causal.rawHead -> b) = do
--   deps <-
--     traverse lookupObject . nubOrd $
--       -- history
--       [Branch1 h | h <- V1.Causal.rawTails cb]
--         ++
--         -- terms
--         [ Term1 (V1 h)
--           | Referent.Ref (Reference.Derived h _i _n) <-
--               (toList . Relation.dom . Star3.d1 . V1.Branch._termsR) b
--         ]
--         ++ [ Term1 (V1 h)
--              | Referent.Ref (Reference.Derived h _i _n) <-
--                  (toList . Relation.dom . Star3.d1 . V1.Branch._termsR) b
--            ]
--         ++
--         -- term metadata
--         [ Term1 (V1 h)
--           | Reference.Derived h _i _n <-
--               (map snd . toList . Relation.ran . Star3.d3 . V1.Branch._termsR) b
--         ]
--         ++
--         -- types
--         [ Decl1 (V1 h)
--           | Reference.Derived h _i _n <-
--               (toList . Relation.dom . Star3.d1 . V1.Branch._typesR) b
--         ]
--         ++
--         -- type metadata
--         [ Term1 (V1 h)
--           | Reference.Derived h _i _n <-
--               (map snd . toList . Relation.ran . Star3.d3 . V1.Branch._typesR) b
--         ]
--         ++ [Branch1 h | h <- toList (V1.Branch._childrenR b)]
--         ++ [Patch1 (V1 h) | h <- toList (V1.Branch._editsR b)]

--   causalParents <- traverse lookupCausal (V1.Causal.rawTails cb)

--   let (missingEntities, foundObjects) = partitionEithers deps
--   let (missingParents, foundParents) = partitionEithers causalParents

--   error "todo"

-- -- pure $ case missingEntities of
-- --   [] -> Right ( makeLookup foundObjects description
-- --               , makeCausalLookup foundParents description )
-- --   missing -> Left missing

-- makeCausalLookup :: [(V1 Hash, (V2 Hash, Db.CausalHashId))] -> String -> V1 Hash -> (V2 Hash, Db.CausalHashId)
-- makeCausalLookup l description a =
--   let m = Map.fromList l
--    in case Map.lookup a m of
--         Just b -> b
--         Nothing ->
--           error $
--             "Somehow I don't have the CausalHashId for "
--               ++ show (Base32Hex.fromHash (runV1 a))
--               ++ " in the map for "
--               ++ description

-- makeLookup :: [(V1 Hash, (V2 Hash, Db.ObjectId))] -> String -> V1 Hash -> (V2 Hash, Db.ObjectId)
-- makeLookup l lookupDescription a =
--   let m = Map.fromList l
--    in case Map.lookup a m of
--         Just b -> b
--         Nothing ->
--           error $
--             "Somehow I don't have the ObjectId for "
--               ++ show (Base32Hex.fromHash (runV1 a))
--               ++ " in the map for "
--               ++ lookupDescription

-- --
-- createTypeSearchIndicesForReferent :: DB m => (V2.ReferentId Db.ObjectId) -> Type -> m ()
-- createTypeSearchIndicesForReferent r typ = do
--   let typeForIndexing = Type.removeAllEffectVars typ

--   -- add the term to the type index
--   typeReferenceForIndexing :: (V2.Reference Db.HashId) <-
--     saveReferenceAsReference2 (Type.toReference typeForIndexing)

--   Db.addToFindByTypeIndex r typeReferenceForIndexing

--   -- add the term to the type mentions index
--   typeMentionsForIndexing :: [V2.Reference Db.HashId] <-
--     traverse
--       saveReferenceAsReference2
--       (toList $ Type.toReferenceMentions typeForIndexing)

--   traverse_ (Db.addToFindByTypeMentionsIndex r) typeMentionsForIndexing
--   where
--     addTermToFindByTypeIndex :: DB m => (V2.ReferentId Db.ObjectId) -> Reference -> m ()
--     addTermToFindByTypeIndex termRef typeRef = do
--       typeRef2 :: (V2.Reference Db.HashId) <-
--         saveReferenceAsReference2 typeRef
--       Db.addToFindByTypeIndex termRef typeRef2
--     addTermToTypeMentionsIndex ::
--       (DB m, Foldable f) => (V2.ReferentId Db.ObjectId) -> f Reference -> m ()
--     addTermToTypeMentionsIndex termRef typeRefs = do
--       typeRefs2 :: [V2.Reference Db.HashId] <-
--         traverse saveReferenceAsReference2 (toList typeRefs)
--       traverse_ (Db.addToFindByTypeMentionsIndex termRef) typeRefs2

-- createDependencyIndexForTerm :: DB m => V2.ReferenceId Db.ObjectId -> Term2 Db.ObjectId-> m ()
-- createDependencyIndexForTerm tmRef@(V2.ReferenceId selfId _i) tm = error "todo"

-- -- let
-- --   -- get the term dependencies
-- --   dependencies :: Set (Reference.ReferenceH Db.ObjectId)
-- --   dependencies = Term.dependencies $ Term.hmap (fromMaybe selfId) tm
-- --   -- and convert them to Reference2
-- --   dependencies2 :: [V2.Reference Db.ObjectId]
-- --   dependencies2 = over Db.referenceTraversal id <$> toList dependencies
-- --   -- and then add all of these to the dependency index
-- -- in traverse_ (Db.addDependencyToIndex tmRef) dependencies2

-- createDependencyIndexForDecl :: DB m => V2.ReferenceId Db.ObjectId -> Decl2S -> m ()
-- createDependencyIndexForDecl tmRef@(V2.ReferenceId selfId _i) decl =
--   traverse_ (Db.addDependencyToIndex tmRef)
--     . toList
--     . DD.declDependencies
--     $ DD.rmapDecl (fmap $ fromMaybe selfId) decl

-- saveTermComponent :: DB m => V1 Hash -> V2 Hash -> Term2ComponentS -> m Db.ObjectId
-- saveTermComponent h1 h2 component = do
--   h1Id <- Db.saveHashByteString (runV1 h1)
--   h2Id <- Db.saveHashByteString (runV2 h2)
--   o <- Db.saveObject h2Id V2.TermComponent blob
--   Db.saveHashObject h1Id o 1
--   Db.saveHashObject h2Id o 2
--   pure o
--   where
--     blob = S.putBytes (S.V1.putFoldable V2.putTerm) component

-- saveDeclComponent :: DB m => Db.HashId -> [Decl2S] -> m Db.ObjectId
-- saveDeclComponent h component = error "todo" -- do
-- -- o <- Db.saveObject h V2.DeclComponent blob
-- -- Db.saveHashObject h o 2
-- -- pure o
-- -- where
-- -- blob = S.putBytes (S.V1.putFoldable (V2.putDecl putObjectId putV putA)) component

-- savePatch :: DB m => Db.HashId -> Patch2S -> m ()
-- savePatch h p = do
--   o <- Db.saveObject h V2.Patch (S.putBytes V2.putEdits p)
--   Db.saveHashObject h o 2

-- -- saveBranch :: DB m => Db.HashId ->

-- -- | Loads a dir with format <root>/base32-encoded-reference.id...
-- --  into a map from Hash to component references
-- componentMapForDir :: forall m. MonadIO m => FilePath -> m (Map (V1 Hash) [Reference.Id])
-- componentMapForDir root = listDirectory root <&> foldl' insert mempty
--   where
--     insert m filename = case V1.componentIdFromString filename of
--       Nothing -> m -- skip silently
--       Just r@(Reference.Id h _i _n) ->
--         Map.unionWith (<>) m (Map.singleton (V1 h) [r])

-- existsObjectWithHash :: DB m => Hash -> m Bool
-- existsObjectWithHash h = Db.queryExists sql [Base32Hex.fromHash h]
--   where
--     sql =
--       [here|
--     SELECT 1
--     FROM hash INNER JOIN hash_object ON hash.id = hash_object.hash_id
--     WHERE base32 = ?
--   |]

-- -- | Given a V1 term component, convert and save it to the V2 codebase
-- -- Pre-requisite: all hash-identified entities in the V1 component have
-- -- already been converted and added to the V2 codebase, apart from self-
-- -- references.
-- convertTerm1 :: DB m => (V1 Hash -> (V2 Hash, Db.ObjectId)) -> V1 Hash -> [(Term, Type)] -> m ()
-- convertTerm1 lookup hash1 v1component = do

--   -- construct v2 term component for hashing
--   let v2componentH :: Term2ComponentH =
--         map (buildTerm2H (fst . lookup) hash1 . fst) v1component
--   -- note: we'd need some special care here if we want to make sure that this
--     -- hash function is identity for simple references
--   let hash2 = V2 (H.accumulate' v2componentH)

--   -- construct v2 term component for serializing
--   let v2componentS :: [Term2 Db.ObjectId] =
--         map (buildTerm2S (snd . lookup) hash1 . fst) v1component

--   -- serialize the v2 term component
--   componentObjectId :: Db.ObjectId <- saveTermComponent hash1 hash2 v2componentS

--   -- construct v2 types for each component element, and save the types to the
--   -- to the indices
--   for_ (zip3 [0 ..] v1component v2componentS) $ \(i, (_term1, typ1), term2) -> do
--     let r = V2.ReferenceId componentObjectId i
--     let rt = V2.ReferentIdRef r

--     saveTypeBlobForReferent rt (buildTermType2S (snd . lookup) typ1)
--     createTypeSearchIndicesForReferent rt typ1
--     createDependencyIndexForTerm r term2

-- convertDecl1 :: DB m => (V1 Hash -> (V2 Hash, Db.ObjectId)) -> V1 Hash -> [Decl] -> m ()
-- convertDecl1 lookup hash1 v1component = do
--   -- construct v2 decl component for hashing
--   let v2componentH :: Decl2ComponentH =
--         map (buildDecl2H (fst . lookup) hash1) v1component
--   let hash2 = V2 (H.hash v2componentH)

--   let v2componentS :: Decl2ComponentS =
--         map (buildDecl2S (snd . lookup) hash1) v1component

--   componentObjectId :: Db.ObjectId <- saveDeclComponent hash1 hash2 v2ComponentS

--   let v2componentI :: [Decl2I] =
--         map (buildDecl2I hash2) v2componentH

--   for_ (zip v2componentI [0..]) $ \(decl2, i) -> do
--     let r = V2.ReferenceId componentObjectId i

--     for_ (zip
--             (DD.constructorTypes (DD.asDataDecl decl2))
--             [0..]) $ \(type2, j) -> do
--       let rt = V2.ReferentIdCon r j
--       saveTypeBlobForReferent rt type2
--       createTypeSearchIndicesForReferent rt type1 -- type1 because `find` uses Hashes

--     createDependencyIndexForDecl r decl2

-- convertCausalBranch1 ::
--   DB m =>
--   (V1 Hash -> Db.ObjectId) ->
--   (V1 Hash -> (V2 Hash, Db.CausalHashId)) ->
--   -- -> V1 Hash
--   V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw ->
--   m ()
-- convertCausalBranch1 lookupObject lookupCausal causalBranch1 = error "todo" -- do
-- -- let branch1Hash = V1.currentHash causalBranch1
-- --     rawBranch2 :: RawBranch = convertBranch1 (V1.rawHead causalBranch1)

-- -- -- branch2Id <- Db.saveObject branch1Hash
-- --     branch2Hash :: V2 Hash = H.hash rawBranch2
-- -- lookupObject <- pure ()
-- --     -- rawCausal2 :: RawCausal = convertCausal1
-- -- -- rawBranch2S
-- -- -- rawCausal2 :: RawCausal <- convertCausal1 lookup rawBranch2 (V1.rawTails causalBranch1)

-- -- -- rawBranch2S
-- -- -- saveBranch2 rawBranch2
-- -- -- saveCausal2 rawCausal2
-- -- error "todo"
-- -- -- Due to switching reference types, and reference's hash's having previously
-- -- -- incorporated the extra `len` field, definitions and patches will not end up
-- -- -- having the same hashes as before. :-\
-- -- -- This means we have to hash them again and perhaps store old and new hashes
-- -- -- separately.
-- -- where
-- -- indexBranch2S :: RawBranch -> RawBranch2S
-- -- indexBranch2S b = RawBranch
-- --   (Map.fromList
-- --     [((ns, over Db.referent2Traversal (lookupObject . V1) r),
-- --       Set.map (over Db.reference2Traversal (lookupObject . V1)) mds)
-- --     |((ns, r), mds) <- Map.toList (terms b)])
-- --   (Map.fromList
-- --     [((ns, over Db.reference2Traversal (lookupObject . V1) r),
-- --       Set.map (over Db.reference2Traversal (lookupObject . V1)) mds)
-- --     |((ns, r), mds) <- Map.toList (types b)])
-- --   (Map.fromList [])
-- --   (Map.fromList [])
-- --   -- <$> tms <*> tps <*> pchs <*> chn where
-- --   -- tms = Map.fromList <$> traverse indexTerm (Map.toList (terms b))
-- --   -- indexTerm :: DB m
-- --   --           => ((NameSegment, Db.Referent2 Hash), Set (V2.Reference Hash))
-- --   --           -> m ((NameSegment, Db.Referent2 Db.ObjectId), Set (V2.Reference Db.ObjectId))
-- --   -- indexTerm ((ns, r), mds) = (,) <$> k <*> v where
-- --   --   k = (ns, over Db.referentTraversal lookupObject r)
-- --   --   v = Set.map

-- -- convertBranch1 :: V1.Branch.Raw -> RawBranch
-- -- convertBranch1 b = RawBranch
-- --   -- terms
-- --   (Map.fromList
-- --       [ ((ns, over Db.referentTraversal id r), mdSet)
-- --       | (r, ns) <- Relation.toList . Star3.d1 $ V1.Branch._termsR b
-- --       , let mdSet :: Set (V2.Reference Hash)
-- --             mdSet = Set.fromList
-- --                   . fmap (over Db.referenceTraversal id . snd)
-- --                   . Set.toList
-- --                   . Relation.lookupDom r
-- --                   . Star3.d3
-- --                   $ V1.Branch._termsR b
-- --       ])
-- --   -- types
-- --   (Map.fromList
-- --       [ ((ns, over Db.referenceTraversal id r), mdSet)
-- --       | (r, ns) <- Relation.toList . Star3.d1 $ V1.Branch._typesR b
-- --       , let mdSet :: Set (V2.Reference Hash)
-- --             mdSet = Set.fromList
-- --                   . fmap (over Db.referenceTraversal id . snd)
-- --                   . Set.toList
-- --                   . Relation.lookupDom r
-- --                   . Star3.d3
-- --                   $ V1.Branch._typesR b
-- --       ])
-- --   -- patches
-- --   (V1.Branch._editsR b)
-- --   -- children
-- --   (runV2 . fst . lookupCausal . V1 . V1.unRawHash <$> V1.Branch._childrenR b)

-- voidTermAnnotations ::
--   V1.TermR tmRef tpRef tmLink tpLink (V1.TypeR tpRef vt at) blankRepr ap v a ->
--   V1.TermR tmRef tpRef tmLink tpLink (V1.TypeR tpRef vt ()) Void () v ()
-- voidTermAnnotations =
--   void . Term.extraMap id id id id void undefined (const ())

-- ----- graveyard
-- ---- |True if `h` (just the hash!) is interned in the DB
-- --knownHash :: DB m => Hash -> m Bool
-- --knownHash h = anyExists $ Db.query sql [Base32Hex.fromHash h] where
-- --  sql = [here| SELECT 1 FROM hash WHERE base32 = ? |]

-- --saveReference :: DB m => ReferenceH h -> m Db.ReferenceId
-- --saveReference r = insert r >> fmap fromJust (loadReference r) where
-- --  insert = \case
-- --    Reference.Builtin t -> execute sql (Just t, Nothing)
-- --    Reference.DerivedId idH -> do
-- --      rdId <- saveReferenceDerived idH
-- --      Db.execute sql (Nothing, Just rdId)
-- --  sql = [here|
-- --    INSERT OR IGNORE INTO reference (builtin, reference_derived_id)
-- --    VALUES (?, ?)
-- --  |]

-- --loadReferenceByHashId :: DB m => ReferenceH HashId -> m (Maybe ReferenceId)
-- --loadReferenceByHashId = \case
-- --  Reference.Builtin t -> queryMaybe sqlb (Only t)
-- --  Reference.DerivedId idH ->
-- --    loadReferenceDerivedByHashId idH >>= \case
-- --      Nothing -> pure Nothing
-- --      Just rdId -> queryMaybe sqld (Only rdId)
-- --  where
-- --  sqlb = [here| SELECT id FROM reference WHERE builtin = ? |]
-- --  sqld = [here| SELECT id FROM reference WHERE reference_derived_id = ? |]

-- --saveReferenceDerived :: DB m => Reference.Id -> m Db.ReferenceDerivedId
-- --saveReferenceDerived r@(Reference.Id h i _n) = do
-- --  hashId <- saveHashByteString h
-- --  saveReferenceDerivedByHashId (Reference.IdH hashId i _n)
-- --
-- --saveReferenceDerivedByHashId :: DB m => Reference.IdH Db.HashId -> m Db.ReferenceDerivedId
-- --saveReferenceDerivedByHashId r@(Reference.IdH hashId i _n) =
-- --  insert hashId i >> fmap fromJust (loadReferenceDerivedByHashId r) where
-- --  insert h i = liftIO $ execute sql (h, i) where
-- --    sql = [here|
-- --      INSERT OR IGNORE INTO reference_derived (hash_id, component_index)
-- --      VALUES (?, ?)
-- --    |]
-- --
-- --loadReferenceDerivedByHashId :: DB m => Reference.IdH Db.HashId -> m (Maybe Db.ReferenceDerivedId)
-- --loadReferenceDerivedByHashId (Reference.IdH h i _n) =
-- --  queryMaybe sql (h, i) where
-- --  sql = [here|
-- --    SELECT id FROM reference_derived
-- --    WHERE hash_id = ? AND component_index = ?
-- --  |]

-- --saveReferentDerived :: DB m => Referent.Id -> m ReferentDerivedId
-- --saveReferentDerived = error "todo"
-- --loadReferentDerived :: DB m => Referent.Id -> m (Maybe ReferentDerivedId)
-- --loadReferentDerived = error "todo"
-- --
-- --saveReferentDerivedByReferenceDerivedId :: DB m => Referent' ReferenceDerivedId -> m ReferentDerivedId
-- --saveReferentDerivedByReferenceDerivedId r = do
-- --  liftIO $ execute sql r
-- --  fmap fromJust (loadReferenceDerivedByReferenceDerivedId r)
-- --  where
-- --  sql = [here|
-- --    INSERT OR IGNORE INTO referent_derived
-- --      (reference_derived_id, constructor_id, constructor_type)
-- --    VALUES (?, ?, ?)
-- --  |]
-- --loadReferentDerivedByReferenceDerivedId :: DB m => Referent' ReferenceDerivedId -> m (Maybe ReferentDerivedId)
-- --loadReferentDerivedByReferenceDerivedId r =  queryMaybe . query sql r where
-- --  sql = [here|
-- --    SELECT id FROM referent_derived
-- --    WHERE reference_derived_id = ?
-- --      AND constructor_id = ?
-- --      AND constructor_type = ?
-- --  |]

-- buildTerm2H :: (V1 Hash -> V2 Hash) -> V1 Hash -> Term -> Term2 Hash
-- buildTerm2H lookup hash1 =
--   voidTermAnnotations . Term.rmap
--     (over Db.referenceTraversal (fmap runV2 . lookupTerm . V1))
--     (over Db.referenceTraversal (runV2 . lookupType . V1))
--     ( over Db.referent2ConTraversal (runV2 . lookupType . V1)
--         . over Db.referentRefTraversal (fmap runV2 . lookupTerm . V1)
--     )
--   where
--     lookupTerm :: V1 Hash -> Maybe (V2 Hash)
--     lookupTerm h | h == hash1 = Nothing
--     lookupTerm h = Just (lookup h)
--     lookupType :: V1 Hash -> V2 Hash
--     lookupType = lookup

-- buildTerm2S :: (V1 Hash -> Db.ObjectId) -> V1 Hash -> Term -> Term2 Db.ObjectId
-- buildTerm2S lookup hash1 =
--   voidTermAnnotations . Term.rmap
--     (over Db.referenceTraversal (lookupTerm . V1))
--     (over Db.referenceTraversal (lookupType . V1))
--     ( over Db.referent2ConTraversal (lookupType . V1)
--         . over Db.referentRefTraversal (lookupTerm . V1)
--     )
--   where
--     lookupTerm :: V1 Hash -> Maybe Db.ObjectId
--     lookupTerm h | h == hash1 = Nothing
--     lookupTerm h = Just (lookup h)
--     lookupType :: V1 Hash -> Db.ObjectId
--     lookupType = lookup

-- buildTermType2S :: (V1 Hash -> Db.ObjectId) -> Type -> Type2S
-- buildTermType2S lookup =
--   void . Type.rmap (over Db.referenceTraversal (lookup . V1))

-- buildDecl2H :: (V1 Hash -> V2 Hash) -> V1 Hash -> Decl -> Decl2 Hash
-- buildDecl2H lookup =
--   void . DD.rmapDecl (over Db.referenceTraversal (fmap runV2 . lookup' . V1))
--   where
--     lookup' :: V1 Hash -> Maybe (V2 hash)
--     lookup' h | h == hash1 = Nothing
--     lookup' h = Just (lookup h)

-- buildDecl2I :: V2 Hash -> Decl2 Hash -> Decl2I
-- buildDecl2I self =
--   DD.rmapDecl (over Db.reference2Traversal (fmap runV2 . fromMaybe self . V2))
