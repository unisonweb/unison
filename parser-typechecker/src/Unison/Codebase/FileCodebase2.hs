{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.FileCodebase2 where

import           Control.Monad                  ( forever, foldM, unless, when )
import           Control.Monad.Extra            ( unlessM )
import           UnliftIO                       ( MonadIO
                                                , MonadUnliftIO
                                                , liftIO )
import           UnliftIO.Concurrent            ( forkIO
                                                , killThread
                                                )
import           UnliftIO.STM                   ( atomically )
import qualified Data.Char                     as Char
import           Data.Foldable                  ( traverse_
                                                , toList
                                                , forM_
                                                , for_
                                                )
import           Data.List                      ( isSuffixOf )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( encodeUtf8
                                                -- , decodeUtf8
                                                )
import           UnliftIO.Directory             ( createDirectoryIfMissing
                                                , doesFileExist
                                                , doesDirectoryExist
                                                , listDirectory
                                                , createDirectory
                                                , removeFile
                                                , doesPathExist
                                                -- , removeDirectoryRecursive
                                                )
import           System.FilePath                ( FilePath
                                                , takeBaseName
                                                , takeFileName
                                                , (</>)
                                                )
import           System.Directory               ( copyFile )
import           System.Path                    ( replaceRoot
                                                , createDir
                                                , subDirs
                                                , files
                                                , dirPath
                                                )
import           Text.Read                      ( readMaybe )
import qualified Unison.Builtin2               as Builtin
import qualified Unison.Codebase2              as Codebase
import           Unison.Codebase2               ( Codebase(Codebase) )
import           Unison.Codebase.Causal2        ( Causal
                                                , RawHash(..)
                                                )
import qualified Unison.Codebase.Causal2       as Causal
import           Unison.Codebase.Branch2        ( Branch )
import qualified Unison.Codebase.Branch2       as Branch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1
                                               as V1

import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Watch         as Watch
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Hash                   as Hash
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent(..) )
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Util.TQueue            as TQueue
import           Unison.Var                     ( Var )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
-- import Debug.Trace

type CodebasePath = FilePath

data Err
  = InvalidBranchFile FilePath String
  | InvalidEditsFile FilePath String
  | NoBranchHead FilePath
  | CantParseBranchHead FilePath
  | AmbiguouslyTypeAndTerm Reference.Id
  | UnknownTypeOrTerm Reference
  deriving Show

termsDir, typesDir, branchesDir, branchHeadDir, editsDir
  :: CodebasePath -> FilePath
termsDir root = root </> "terms"
typesDir root = root </> "types"
branchesDir root = root </> "branches"
branchHeadDir root = branchesDir root </> "_head"
editsDir root = root </> "patches"

termDir, declDir :: CodebasePath -> Reference.Id -> FilePath
termDir root r = termsDir root </> componentId r
declDir root r = typesDir root </> componentId r

referenceToDir :: Reference -> FilePath
referenceToDir r = case r of
  Reference.Builtin name -> "_builtin" </> encodeFileName name
  Reference.DerivedId hash -> componentId hash

dependentsDir :: CodebasePath -> Reference -> FilePath
dependentsDir root r = root </> "dependents" </> referenceToDir r

watchesDir :: CodebasePath -> Text -> FilePath
watchesDir root UF.RegularWatch = root </> "watches" </> "_cache"
watchesDir root kind = root </> "watches" </> encodeFileName kind

typeIndexDir :: CodebasePath -> Reference -> FilePath
typeIndexDir root r = root </> "type-index" </> referenceToDir r

typeMentionsIndexDir :: CodebasePath -> Reference -> FilePath
typeMentionsIndexDir root r = root </> "type-mentions-index" </> referenceToDir r

-- https://superuser.com/questions/358855/what-characters-are-safe-in-cross-platform-file-names-for-linux-windows-and-os
encodeFileName :: Text -> FilePath
encodeFileName t = let
  go ('/' : rem) = "$forward-slash$" <> go rem
  go ('\\' : rem) = "$back-slash$" <> go rem
  go (':' : rem) = "$colon$" <> go rem
  go ('*' : rem) = "$star$" <> go rem
  go ('?' : rem) = "$question-mark$" <> go rem
  go ('"' : rem) = "$double-quote$" <> go rem
  go ('<' : rem) = "$less-than$" <> go rem
  go ('>' : rem) = "$greater-than$" <> go rem
  go ('|' : rem) = "$pipe$" <> go rem
  go ('$' : rem) = "$$" <> go rem
  go (c : rem) | not (Char.isPrint c && Char.isAscii c)
                 = "$b58" <> b58 [c] <> "$" <> go rem
               | otherwise = c : go rem
  go [] = []
  b58 = Hash.base58s . Hash.fromBytes . encodeUtf8 . Text.pack
  in if t == "." then "$dot$"
     else if t == ".." then "$dotdot$"
     else go (Text.unpack t)

termPath, typePath, declPath :: CodebasePath -> Reference.Id -> FilePath
termPath path r = termDir path r </> "compiled.ub"
typePath path r = termDir path r </> "type.ub"
declPath path r = declDir path r </> "compiled.ub"

branchPath :: CodebasePath -> Hash.Hash -> FilePath
branchPath root h = branchesDir root </> Hash.base58s h ++ ".ub"

editsPath :: CodebasePath -> Hash.Hash -> FilePath
editsPath root h = editsDir root </> Hash.base58s h ++ ".up"

touchIdFile :: Reference.Id -> FilePath -> IO ()
touchIdFile id fp = do
  createDirectoryIfMissing True fp
  -- note: contents of the file are equal to the name, rather than empty, to
  -- hopefully avoid git getting clever about treating deletions as renames
  writeFile (fp </> componentId id) (componentId id)

-- checks if `path` looks like a unison codebase
minimalCodebaseStructure :: CodebasePath -> [FilePath]
minimalCodebaseStructure root =
  [ termsDir root
  , typesDir root
  , branchesDir root
  , branchHeadDir root
  , editsDir root
  ]

-- checks if a minimal codebase structure exists at `path`
exists :: CodebasePath -> IO Bool
exists root =
  and <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

-- creates a minimal codebase structure at `path`
initialize :: CodebasePath -> IO ()
initialize path =
  traverse_ (createDirectoryIfMissing True) (minimalCodebaseStructure path)

getRootBranch :: MonadIO m => CodebasePath -> m (Branch m)
getRootBranch root =
  liftIO (listDirectory $ branchHeadDir root) >>= \case
    []       -> failWith $ NoBranchHead (branchHeadDir root)
    [single] -> go single
    conflict -> traverse go conflict >>= \case
      x : xs -> foldM Branch.merge x xs
      []     -> failWith $ NoBranchHead (branchHeadDir root)
 where
  go single = case Hash.fromBase58 (Text.pack single) of
    Nothing -> failWith $ CantParseBranchHead single
    Just h  -> branchFromFiles root (RawHash h)
  branchFromFiles :: MonadIO m => FilePath -> Branch.Hash -> m (Branch m)
  branchFromFiles rootDir = Branch.read
    (deserializeRawBranch rootDir)
    (deserializeEdits rootDir)

  deserializeRawBranch
    :: MonadIO m
    => CodebasePath
    -> Causal.Deserialize m Branch.Raw Branch.Raw
  deserializeRawBranch root (RawHash h) = do
    let ubf = branchPath root h
    liftIO (S.getFromFile' (V1.getCausal0 V1.getRawBranch) ubf) >>= \case
      Left err -> failWith $ InvalidBranchFile ubf err
      Right c0 -> pure c0
  deserializeEdits :: MonadIO m => CodebasePath -> Branch.EditHash -> m Patch
  deserializeEdits root h =
    let file = editsPath root h in
    liftIO (S.getFromFile' V1.getEdits file) >>= \case
      Left err -> failWith $ InvalidEditsFile file err
      Right edits -> pure edits

putRootBranch :: MonadIO m => CodebasePath -> Branch m -> m ()
putRootBranch root b = do
  Branch.sync (hashExists root)
              (serializeRawBranch root)
              (serializeEdits root)
              b
  updateCausalHead (branchHeadDir root) (Branch._history b)

hashExists :: MonadIO m => CodebasePath -> Branch.Hash -> m Bool
hashExists root (RawHash h) = liftIO $ doesFileExist (branchPath root h)

serializeRawBranch
  :: (MonadIO m) => CodebasePath -> Causal.Serialize m Branch.Raw Branch.Raw
serializeRawBranch root (RawHash h) = liftIO
  . S.putWithParentDirs (V1.putRawCausal V1.putRawBranch) (branchPath root h)

serializeEdits
  :: MonadIO m => CodebasePath -> Branch.EditHash -> m Patch -> m ()
serializeEdits root h medits = do
  edits <- medits
  unlessM (liftIO $ doesFileExist (editsPath root h))
    $ liftIO
    $ S.putWithParentDirs V1.putEdits (editsPath root h) edits

-- `headDir` is like ".unison/branches/head", or ".unison/edits/head";
-- not ".unison"
updateCausalHead :: MonadIO m => FilePath -> Causal n h e -> m ()
updateCausalHead headDir c = do
  let (RawHash h) = Causal.currentHash c
      hs = Hash.base58s h
  -- write new head
  exists <- doesDirectoryExist headDir
  unless exists $ createDirectory headDir
  liftIO $ writeFile (headDir </> hs) ""
  -- delete existing heads
  liftIO $ fmap (filter (/= hs)) (listDirectory headDir)
       >>= traverse_ (removeFile . (headDir </>))

-- decodeBuiltinName :: FilePath -> Maybe Text
-- decodeBuiltinName p =
--   decodeUtf8 . Hash.toBytes <$> Hash.fromBase58 (Text.pack p)

componentId :: Reference.Id -> String
componentId (Reference.Id h 0 1) = Hash.base58s h
componentId (Reference.Id h i n) =
  Hash.base58s h <> "-" <> show i <> "-" <> show n

-- todo: this is base58-i-n ?
parseHash :: String -> Maybe Reference.Id
parseHash s = case splitOn "-" s of
  [h]       -> makeId h 0 1
  [h, i, n] -> do
    x <- readMaybe i
    y <- readMaybe n
    makeId h x y
  _ -> Nothing
 where
  makeId h i n = (\x -> Reference.Id x i n) <$> Hash.fromBase58 (Text.pack h)

-- Adapted from
-- http://hackage.haskell.org/package/fsutils-0.1.2/docs/src/System-Path.html
copyDir :: (FilePath -> Bool) -> FilePath -> FilePath -> IO ()
copyDir predicate from to = do
  createDirectoryIfMissing True to
  d <- createDir from
  when (predicate $ dirPath d) $ do
    forM_ (subDirs d)
      $ \path -> copyDir predicate path (replaceRoot from to path)
    forM_ (files d) $ \path -> do
      exists <- doesFileExist to
      unless exists . copyFile path $ replaceRoot from to path

copyFromGit :: MonadIO m => FilePath -> FilePath -> m ()
copyFromGit = (liftIO .) . flip
  (copyDir (\x -> not ((".git" `isSuffixOf` x) || ("_head" `isSuffixOf` x))))

writeAllTermsAndTypes
  :: forall m v a
   . MonadIO m
  => Var v
  => S.Put v
  -> S.Put a
  -> Codebase m v a
  -> FilePath
  -> Branch m
  -> m ()
writeAllTermsAndTypes putV putA codebase localPath branch = do
  Branch.sync (hashExists localPath) serialize (serializeEdits localPath) branch
  updateCausalHead (branchHeadDir localPath) $ Branch._history branch
 where
  serialize :: Causal.Serialize m Branch.Raw Branch.Raw
  serialize rh rawBranch = do
    writeBranch $ Causal.rawHead rawBranch
    serializeRawBranch localPath rh rawBranch
  calamity i =
    error
      $  "Calamity! Somebody deleted "
      <> show i
      <> " from the codebase while I wasn't looking."
  writeBranch :: Branch.Raw -> m ()
  writeBranch (Branch.Raw terms types _ _) = do
    for_ (toList $ Star3.fact types) $ \case
      Reference.DerivedId i -> do
        alreadyExists <- liftIO . doesPathExist $ termPath localPath i
        unless alreadyExists $ do
          mayDecl <- Codebase.getTypeDeclaration codebase i
          maybe (calamity i) (putDecl putV putA localPath i) mayDecl
      _ -> pure ()
    -- Write all terms
    for_ (toList $ Star3.fact terms) $ \case
      Ref r@(Reference.DerivedId i) -> do
        alreadyExists <- liftIO . doesPathExist $ termPath localPath i
        unless alreadyExists $ do
          mayTerm <- Codebase.getTerm codebase i
          mayType <- Codebase.getTypeOfTerm codebase r
          fromMaybe (calamity i)
                    (putTerm putV putA localPath i <$> mayTerm <*> mayType)
          -- If the term is a test, write the cached value too.
          mayTest <- Codebase.getWatch codebase UF.TestWatch i
          maybe (pure ()) (putWatch putV putA localPath UF.TestWatch i) mayTest
      _ -> pure ()

putTerm
  :: MonadIO m
  => Var v
  => S.Put v
  -> S.Put a
  -> FilePath
  -> Reference.Id
  -> Term.AnnotatedTerm v a
  -> Type.AnnotatedType v a
  -> m ()
putTerm putV putA path h e typ = liftIO $ do
  let rootTypeHash = Type.toReference typ
      typeMentions = Type.toReferenceMentions typ
  S.putWithParentDirs (V1.putTerm putV putA) (termPath path h) e
  S.putWithParentDirs (V1.putType putV putA) (typePath path h) typ
  -- Add the term as a dependent of its dependencies
  let deps = deleteComponent h $ Term.dependencies e <> Type.dependencies typ
  traverse_ (touchIdFile h . dependentsDir path) deps
  traverse_ (touchIdFile h . typeMentionsIndexDir path) typeMentions
  touchIdFile h . typeIndexDir path $ rootTypeHash

putDecl
  :: MonadIO m
  => Var v
  => S.Put v
  -> S.Put a
  -> FilePath
  -> Reference.Id
  -> Codebase.Decl v a
  -> m ()
putDecl putV putA path h decl = liftIO $ do
  S.putWithParentDirs
    (V1.putEither (V1.putEffectDeclaration putV putA)
                  (V1.putDataDeclaration putV putA)
    )
    (declPath path h)
    decl
  traverse_ (touchIdFile h . dependentsDir path) deps
 where
  deps = deleteComponent h . DD.dependencies $ either DD.toDataDecl id decl

putWatch
  :: MonadIO m
  => Var v
  => S.Put v
  -> S.Put a
  -> FilePath
  -> UF.WatchKind
  -> Reference.Id
  -> Codebase.Term v a
  -> m ()
putWatch putV putA path k id e = liftIO $ S.putWithParentDirs
  (V1.putTerm putV putA)
  (watchesDir path (Text.pack k) </> componentId id <> ".ub")
  e

-- builds a `Codebase IO v a`, given serializers for `v` and `a`
codebase1
  :: forall m v a
   . MonadUnliftIO m
  => Var v => a -> S.Format v -> S.Format a -> CodebasePath -> Codebase m v a
codebase1 builtinTypeAnnotation (S.Format getV putV) (S.Format getA putA) path
  = let c = Codebase getTerm
                     getTypeOfTerm
                     getDecl
                     (putTerm putV putA path)
                     (putDecl putV putA path)
                     (getRootBranch path)
                     (putRootBranch path)
                     (branchHeadUpdates path)
                     dependents
                     (copyFromGit path)
                     -- This is fine as long as watat doesn't call
                     -- syncToDirectory c
                     (writeAllTermsAndTypes putV putA c)
                     watches
                     getWatch
                     (putWatch putV putA path)
                     getTermsOfType
                     getTermsMentioningType
    in  c
 where
  getTerm h = liftIO $ S.getFromFile (V1.getTerm getV getA) (termPath path h)
  getTypeOfTerm r = liftIO $ case r of
    Reference.Builtin _ ->
      pure
        $   fmap (const builtinTypeAnnotation)
        <$> Map.lookup r Builtin.termRefTypes
    Reference.DerivedId h ->
      S.getFromFile (V1.getType getV getA) (typePath path h)
  getDecl h = liftIO $ S.getFromFile
    (V1.getEither (V1.getEffectDeclaration getV getA)
                  (V1.getDataDeclaration getV getA)
    )
    (declPath path h)

  dependents :: Reference -> m (Set Reference.Id)
  dependents r = listDirAsIds (dependentsDir path r)

  getTermsOfType :: Reference -> m (Set Reference.Id)
  getTermsOfType r = listDirAsIds (typeIndexDir path r)

  getTermsMentioningType :: Reference -> m (Set Reference.Id)
  getTermsMentioningType r = listDirAsIds (typeMentionsIndexDir path r)

  listDirAsIds :: FilePath -> m (Set Reference.Id)
  listDirAsIds d = do
    e <- doesDirectoryExist d
    if e
      then do
        ls <- listDirectory d
        pure . Set.fromList $ ls >>= (toList . parseHash)
      else pure Set.empty

  watches :: UF.WatchKind -> m [Reference.Id]
  watches k = liftIO $ do
    let wp = watchesDir path (Text.pack k)
    createDirectoryIfMissing True wp
    ls <- listDirectory wp
    pure $ ls >>= (toList . parseHash . takeFileName)

  getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Codebase.Term v a))
  getWatch k id = liftIO $ do
    let wp = watchesDir path (Text.pack k)
    createDirectoryIfMissing True wp
    S.getFromFile (V1.getTerm getV getA) (wp </> componentId id <> ".ub")

-- watches in `branchHeadDir root` for externally deposited heads;
-- parse them, and return them
branchHeadUpdates
  :: MonadUnliftIO m => CodebasePath -> m (m (), m (Set Branch.Hash))
branchHeadUpdates root = do
  branchHeadChanges      <- TQueue.newIO
  (cancelWatch, watcher) <- Watch.watchDirectory' (branchHeadDir root)
--  -- add .ubf file changes to intermediate queue
  watcher1               <-
    forkIO
    $ forever
    $ do
      -- Q: what does watcher return on a file deletion?
      -- A: nothing
        (filePath, _) <- watcher
        case hashFromFilePath filePath of
          Nothing -> failWith $ CantParseBranchHead filePath
          Just h ->
            atomically . TQueue.enqueue branchHeadChanges $ Branch.Hash h
  -- smooth out intermediate queue
  pure
    ( cancelWatch >> killThread watcher1
    , Set.fromList <$> Watch.collectUntilPause branchHeadChanges 400000
    )

hashFromFilePath :: FilePath -> Maybe Hash.Hash
hashFromFilePath = Hash.fromBase58 . Text.pack . takeBaseName

failWith :: MonadIO m => Err -> m a
failWith = fail . show

deleteComponent :: Reference.Id -> Set Reference -> Set Reference
deleteComponent r rs = Set.difference rs
  (Reference.members . Reference.componentFor . Reference.DerivedId $ r)
