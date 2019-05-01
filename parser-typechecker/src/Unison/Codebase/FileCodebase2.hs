{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.FileCodebase2 where

-- import           Control.Concurrent             ( forkIO
--                                                 , killThread
--                                                 )
-- import           Control.Monad                  ( filterM
--                                                 , forever
--                                                 , when
--                                                 )
import           Control.Monad.Error.Class      ( MonadError
                                                , throwError
                                                )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.STM              ( atomically )
import qualified Data.Bytes.Get                as Get
import qualified Data.ByteString               as BS
import           Data.Foldable                  ( traverse_, toList )
import           Data.List                      ( isSuffixOf
                                                , partition
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes, fromMaybe, isJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( encodeUtf8, decodeUtf8 )
import           System.Directory               ( createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , listDirectory
                                                , removeFile
                                                , removeDirectoryRecursive
                                                )
import           System.FilePath                ( FilePath
                                                , takeBaseName
                                                , takeDirectory
                                                , takeExtension
                                                , takeFileName
                                                , (</>)
                                                )
import           Text.Read                      ( readMaybe )
import qualified Unison.Builtin                as Builtin
import           Unison.Codebase2               ( Codebase(Codebase)
                                                , Err(InvalidBranchFile)
                                                )
import           Unison.Codebase.Causal2         ( Causal0, C0Hash(..) )
-- import qualified Unison.Codebase.Branch2        as Branch
import           Unison.Codebase.Branch2         ( Branch )
import qualified Unison.Codebase.Branch2        as Branch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1
                                               as V1
import qualified Unison.Codebase.Watch         as Watch
import qualified Unison.Hash                   as Hash
import qualified Unison.Reference              as Reference
import           Unison.Reference               ( Reference )
import qualified Unison.Term                   as Term
import qualified Unison.Util.TQueue            as TQueue
import           Unison.Var                     ( Var )
-- import Debug.Trace


-- checks if `path` looks like a unison codebase
minimalCodebaseStructure :: FilePath -> [FilePath]
minimalCodebaseStructure path =
  [branchesPath path
  ,path </> "terms"
  ,path </> "types"]
  -- todo: add data constructor paths or whatever that ends up being

-- checks if a minimal codebase structure exists at `path`
exists :: FilePath -> IO Bool
exists path =
  all id <$> traverse doesDirectoryExist (minimalCodebaseStructure path)

-- creates a minimal codebase structure at `path`
initialize :: FilePath -> IO ()
initialize path =
  traverse_ (createDirectoryIfMissing True) (minimalCodebaseStructure path)

deserializeRawBranch :: (MonadIO m, MonadError Err m)
  => FilePath -> Branch.Hash -> m (Causal0 Branch.Raw Branch.Raw)
deserializeRawBranch root (C0Hash h) = do
  let ubf = branchPath root h
  bytes <- liftIO $ BS.readFile ubf
  case Get.runGetS (V1.getCausal0 V1.getRawBranch) bytes of
    Left err -> throwError $ InvalidBranchFile ubf err
    Right c0 -> pure c0


branchFromFile :: (MonadIO m, MonadError Err m)
               => FilePath -> Branch.Hash -> m (Branch m)
branchFromFile root h = error "todo" -- do
  -- let ubf = branchPath root h
  -- bytes <- liftIO $ BS.readFile ubf
  -- case Get.runGetS V1.getBranch0 bytes of
  --   Left err     -> throwError $ InvalidBranchFile ubf err
  --   Right branch -> pure branch

-- -- branchToFile :: FilePath -> Branch -> IO ()
-- -- branchToFile = S.putWithParentDirs V0.putBranch
--
-- -- loads branch from file and prints errors to stdout
-- -- do we use this?
-- -- branchFromFile' :: FilePath -> IO (Maybe Branch)
-- -- branchFromFile' ubf = go =<< runExceptT (branchFromFile ubf)
-- --   where
-- --     go (Left e) = do
-- --       liftIO $ putStrLn (show e)
-- --       pure Nothing
-- --     go (Right b) = pure (Just b)
--
-- -- Tries to load root branch from `dir`; if there is more than one head,
-- -- they get merged.
-- -- todo: might want to have richer return type that reflects merges that
-- -- may have been done
-- -- branchFromDirectory :: FilePath -> IO (Maybe Branch)
-- -- branchFromDirectory dir = do
-- --   exists <- doesDirectoryExist dir
-- --   case exists of
-- --     False -> pure Nothing
-- --     True  -> do
-- --       bos <- traverse branchFromFile'
-- --         =<< filesInPathMatchingExtension dir ".ubf"
-- --       pure $ case catMaybes bos of
-- --         []  -> Nothing
-- --         bos -> Just (mconcat bos)
--
-- -- filesInPathMatchingExtension :: FilePath -> String -> IO [FilePath]
-- -- filesInPathMatchingExtension path extension =
-- --   doesDirectoryExist path >>= \ok -> if ok
-- --     then
-- --       fmap (path </>)
-- --         <$> (filter (((==) extension) . takeExtension) <$> listDirectory path)
-- --     else pure []

-- -- isValidBranchDirectory :: FilePath -> IO Bool
-- -- isValidBranchDirectory path =
-- --   not . null <$> filesInPathMatchingExtension path ".ubf"

termDir, declDir :: FilePath -> Reference.Id -> FilePath
termDir path r = path </> "terms" </> componentId r
declDir path r = path </> "types" </> componentId r

encodeBuiltinName :: Text -> FilePath
encodeBuiltinName = Hash.base58s . Hash.fromBytes . encodeUtf8

-- -- decodeBuiltinName :: FilePath -> Maybe Text
-- -- decodeBuiltinName p =
-- --   decodeUtf8 . Hash.toBytes <$> Hash.fromBase58 (Text.pack p)

builtinTermDir, builtinTypeDir :: FilePath -> Text -> FilePath
builtinTermDir path name =
  path </> "terms" </> "_builtin" </> encodeBuiltinName name
builtinTypeDir path name =
  path </> "types" </> "_builtin" </> encodeBuiltinName name
builtinDir :: FilePath -> Reference -> Maybe FilePath
builtinDir path r@(Reference.Builtin name) =
  if Builtin.isBuiltinTerm r then Just (builtinTermDir path name)
  else if Builtin.isBuiltinType r then Just (builtinTypeDir path name)
  else Nothing
builtinDir _ _ = Nothing

termPath, typePath, declPath :: FilePath -> Reference.Id -> FilePath
termPath path r = termDir path r </> "compiled.ub"
typePath path r = termDir path r </> "type.ub"
declPath path r = declDir path r </> "compiled.ub"

componentId :: Reference.Id -> String
componentId (Reference.Id h 0 1) = Hash.base58s h
componentId (Reference.Id h i n) =
  Hash.base58s h <> "-" <> show i <> "-" <> show n

branchesPath :: FilePath -> FilePath
branchesPath path = path </> "branches"

branchPath :: FilePath -> Hash.Hash -> FilePath
branchPath path h = branchesPath path </> Hash.base58s h

touchDependentFile :: Reference.Id -> FilePath -> IO ()
touchDependentFile dependent fp = do
  createDirectoryIfMissing True (fp </> "dependents")
  writeFile (fp </> "dependents" </> componentId dependent) ""

-- -- -- todo: this is base58-i-n ?
-- -- parseHash :: String -> Maybe Reference.Id
-- -- parseHash s = case splitOn "-" s of
-- --   [h]       -> makeId h 0 1
-- --   [h, i, n] -> do
-- --     x <- readMaybe i
-- --     y <- readMaybe n
-- --     makeId h x y
-- --   _ -> Nothing
-- --  where
-- --   makeId h i n = (\x -> Reference.Id x i n) <$> Hash.fromBase58 (Text.pack h)
-- --
-- -- -- todo: builtin data decls (optional, unit, pair) should just have a regular
-- -- -- hash-based reference, rather than being Reference.Builtin
-- -- -- and we should verify that this doesn't break the runtime
-- -- codebase1
-- --   :: Var v => a -> S.Format v -> S.Format a -> FilePath -> Codebase IO v a
-- -- codebase1 builtinTypeAnnotation (S.Format getV putV) (S.Format getA putA) path
-- --   = let
-- --       getTerm h = S.getFromFile (V0.getTerm getV getA) (termPath path h)
-- --       putTerm h e typ = do
-- --         S.putWithParentDirs (V0.putTerm putV putA) (termPath path h) e
-- --         S.putWithParentDirs (V0.putType putV putA) (typePath path h) typ
-- --         let declDependencies :: Set Reference
-- --             declDependencies = Term.referencedDataDeclarations e
-- --               <> Term.referencedEffectDeclarations e
-- --         -- Add the term as a dependent of its dependencies
-- --         let err = "FileCodebase.putTerm found reference to unknown builtin."
-- --             deps = Term.dependencies' e
-- --         traverse_
-- --           (touchDependentFile h  . fromMaybe (error err) . builtinDir path)
-- --           [ r | r@(Reference.Builtin _) <- Set.toList $ deps]
-- --         traverse_ (touchDependentFile h . termDir path)
-- --           $ [ r | Reference.DerivedId r <- Set.toList $ Term.dependencies' e ]
-- --         traverse_ (touchDependentFile h . declDir path)
-- --           $ [ r | Reference.DerivedId r <- Set.toList declDependencies ]
-- --       getTypeOfTerm r = case r of
-- --         Reference.Builtin _ -> pure $
-- --           fmap (const builtinTypeAnnotation) <$> Map.lookup r Builtin.builtins0
-- --         Reference.DerivedId h ->
-- --           S.getFromFile (V0.getType getV getA) (typePath path h)
-- --       getDecl h = S.getFromFile
-- --         (V0.getEither (V0.getEffectDeclaration getV getA)
-- --                       (V0.getDataDeclaration getV getA)
-- --         )
-- --         (declPath path h)
-- --       putDecl h decl = S.putWithParentDirs
-- --         (V0.putEither (V0.putEffectDeclaration putV putA)
-- --                       (V0.putDataDeclaration putV putA)
-- --         )
-- --         (declPath path h)
-- --         decl
-- --       branches = map Text.pack <$> do
-- --         files <- listDirectory (branchesPath path)
-- --         let paths = (branchesPath path </>) <$> files
-- --         fmap takeFileName <$> filterM isValidBranchDirectory paths
-- --
-- --       getBranch name = branchFromDirectory (branchPath path name)
-- --
-- --       -- delete any leftover branch files "before" this one,
-- --       -- and write this one if it doesn't already exist.
-- --       overwriteBranch :: BranchName -> Branch -> IO ()
-- --       overwriteBranch name branch = do
-- --         let newBranchHash = Hash.base58s . Branch.toHash $ branch
-- --         (match, nonmatch) <-
-- --           partition (\s -> newBranchHash == takeBaseName s)
-- --             <$> filesInPathMatchingExtension (branchPath path name) ".ubf"
-- --         let isBefore :: Branch -> FilePath -> IO Bool
-- --             isBefore b ubf =
-- --               maybe False (`Branch.before` b) <$> branchFromFile' ubf
-- --         -- delete any existing .ubf files
-- --         traverse_ removeFile =<< filterM (isBefore branch) nonmatch
-- --         -- save new branch data under <base58>.ubf
-- --         when (null match) $ branchToFile
-- --           (branchPath path name </> newBranchHash <> ".ubf")
-- --           branch
-- --
-- --       mergeBranch name branch = do
-- --         target <- getBranch name
-- --         let newBranch = case target of
-- --               -- merge with existing branch if present
-- --               Just existing -> Branch.merge branch existing
-- --               -- or save new branch
-- --               Nothing       -> branch
-- --         overwriteBranch name newBranch
-- --         pure newBranch
-- --
-- --       deleteBranch name = removeDirectoryRecursive (branchPath path name)
-- --
-- --
-- --       dependents :: Reference -> IO (Set Reference.Id)
-- --       dependents r = do
-- --         d  <- dir
-- --         e  <- doesDirectoryExist (d </> "dependents")
-- --         if e then do
-- --               ls <- listDirectory (d </> "dependents")
-- --               pure . Set.fromList $ ls >>= (toList . parseHash)
-- --         else pure Set.empty
-- --        where
-- --         dir = case r of
-- --           Reference.Builtin name ->
-- --             pure $ (if Builtin.isBuiltinTerm r
-- --                     then builtinTermDir
-- --                     else builtinTypeDir) path name
-- --           Reference.DerivedId id -> do
-- --             b <- isJust <$> getTerm id
-- --             pure $ (if b then termDir else declDir) path id
-- --
-- --       branchUpdates :: IO (IO (), IO (Set BranchName))
-- --       branchUpdates = do
-- --         branchFileChanges      <- TQueue.newIO
-- --         (cancelWatch, watcher) <- Watch.watchDirectory' (branchesPath path)
-- --         -- add .ubf file changes to intermediate queue
-- --         watcher1               <- forkIO $ do
-- --           forever $ do
-- --             (filePath, _) <- watcher
-- --             when (".ubf" `isSuffixOf` filePath)
-- --               $ atomically
-- --               . TQueue.enqueue branchFileChanges
-- --               $ filePath
-- --         -- smooth out intermediate queue
-- --         pure
-- --           $ ( cancelWatch >> killThread watcher1
-- --             , Set.map ubfPathToName . Set.fromList <$> Watch.collectUntilPause
-- --               branchFileChanges
-- --               400000
-- --             )
-- --     in
-- --       Codebase getTerm
-- --                getTypeOfTerm
-- --                putTerm
-- --                getDecl
-- --                putDecl
-- --                branches
-- --                getBranch
-- --                mergeBranch
-- --                deleteBranch
-- --                branchUpdates
-- --                dependents
-- --                builtinTypeAnnotation
-- --
-- -- ubfPathToName :: FilePath -> BranchName
-- -- ubfPathToName = Text.pack . takeFileName . takeDirectory
