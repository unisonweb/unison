{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.FileCodebase2 where

import           Control.Monad                  ( forever )
import           Control.Monad.Extra            ( unlessM )
import           UnliftIO                       ( MonadIO
                                                , MonadUnliftIO
                                                , liftIO )
import           UnliftIO.Concurrent            ( forkIO
                                                , killThread
                                                )
import           UnliftIO.STM                   ( atomically )
import qualified Data.Char                     as Char
import           Data.Foldable                  ( traverse_, toList )
import           Data.List.Split                ( splitOn )
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
                                                , removeFile
                                                -- , removeDirectoryRecursive
                                                )
import           System.FilePath                ( FilePath
                                                , takeBaseName
                                                , (</>)
                                                )
import           Text.Read                      ( readMaybe )
import qualified Unison.Builtin2               as Builtin
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
import qualified Unison.Reference              as Reference
import           Unison.Reference               ( Reference )
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Util.TQueue            as TQueue
import           Unison.Var                     ( Var )
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

termsDir, typesDir, branchesDir, branchHeadDir, editsDir :: CodebasePath -> FilePath
termsDir root = root </> "terms"
typesDir root = root </> "types"
branchesDir root = root </> "branches"
branchHeadDir root = branchesDir root </> "head"
editsDir root = root </> "edits"

termDir, declDir :: CodebasePath -> Reference.Id -> FilePath
termDir root r = termsDir root </> componentId r
declDir root r = typesDir root </> componentId r

dependentsDir :: CodebasePath -> Reference -> FilePath
dependentsDir root r = root </> "dependents" </> case r of
  Reference.Builtin name -> "_builtin" </> encodeFileName name
  Reference.DerivedId hash -> componentId hash

watchesDir :: CodebasePath -> Text -> FilePath
watchesDir root kind = root </> "watches" </> encodeFileName kind


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

touchDependentFile :: Reference.Id -> FilePath -> IO ()
touchDependentFile dependent fp = do
  createDirectoryIfMissing True fp
  writeFile (fp </> componentId dependent) ""

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
  all id <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

-- creates a minimal codebase structure at `path`
initialize :: CodebasePath -> IO ()
initialize path =
  traverse_ (createDirectoryIfMissing True) (minimalCodebaseStructure path)

getRootBranch
  :: MonadIO m => CodebasePath -> m (Branch m)
getRootBranch root = do
  (liftIO $ listDirectory (branchHeadDir root)) >>= \case
    [] -> failWith $ NoBranchHead (branchHeadDir root)
    [single] -> case Hash.fromBase58 (Text.pack single) of
      Nothing -> failWith $ CantParseBranchHead single
      Just h -> branchFromFiles root (RawHash h)
    _conflict ->
      -- todo: might want a richer return type that reflects these merges
      error "todo; load all and merge?"
  where
  branchFromFiles :: MonadIO m
                  => FilePath -> Branch.Hash -> m (Branch m)
  branchFromFiles rootDir rootHash =
    Branch.read (deserializeRawBranch rootDir)
                (deserializeEdits rootDir) rootHash

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

putRootBranch
  :: MonadIO m => CodebasePath -> Branch m -> m ()
putRootBranch root b = do
  Branch.sync hashExists (serializeRawBranch root) (serializeEdits root) b
  updateCausalHead (branchHeadDir root) (Branch._history b)
  where
  hashExists :: MonadIO m => Branch.Hash -> m Bool
  hashExists (RawHash h) = liftIO $ doesFileExist (branchPath root h)
  serializeRawBranch
    :: (MonadIO m)
    => CodebasePath
    -> Causal.Serialize m Branch.Raw Branch.Raw
  serializeRawBranch root (RawHash h) rc = liftIO $
    S.putWithParentDirs
      (V1.putRawCausal V1.putRawBranch) (branchPath root h) rc
  serializeEdits :: MonadIO m
    => CodebasePath -> Branch.EditHash -> m Patch -> m ()
  serializeEdits root h medits = do
    edits <- medits
    unlessM (liftIO $ doesFileExist (editsPath root h)) $
      liftIO $ S.putWithParentDirs V1.putEdits (editsPath root h) edits


-- `headDir` is like ".unison/branches/head", or ".unison/edits/head";
-- not ".unison"
updateCausalHead :: MonadIO m => FilePath -> Causal n h e -> m ()
updateCausalHead headDir c = do
  let (RawHash h) = Causal.currentHash c
  -- delete existing heads
  liftIO $ listDirectory headDir >>= traverse_ removeFile
  -- write new head
  liftIO $ writeFile (headDir </> Hash.base58s h) ""

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

-- builds a `Codebase IO v a`, given serializers for `v` and `a`
codebase1
  :: (MonadUnliftIO m, Var v)
  => a -> S.Format v -> S.Format a -> CodebasePath -> Codebase m v a
codebase1 builtinTypeAnnotation (S.Format getV putV) (S.Format getA putA) path =
  Codebase getTerm
           getTypeOfTerm
           getDecl
           putTerm
           putDecl
           (getRootBranch path)
           (putRootBranch path)
           (branchHeadUpdates path)
           dependents
  where
    getTerm h = liftIO $ S.getFromFile (V1.getTerm getV getA) (termPath path h)
    putTerm h e typ = liftIO $ do
      S.putWithParentDirs (V1.putTerm putV putA) (termPath path h) e
      S.putWithParentDirs (V1.putType putV putA) (typePath path h) typ
      -- Add the term as a dependent of its dependencies
      let deps = deleteComponent h $ Term.dependencies e <> Type.dependencies typ
      traverse_ (touchDependentFile h . dependentsDir path) deps
    getTypeOfTerm r = liftIO $ case r of
      Reference.Builtin _ -> pure $
        fmap (const builtinTypeAnnotation) <$> Map.lookup r Builtin.termRefTypes
      Reference.DerivedId h ->
        S.getFromFile (V1.getType getV getA) (typePath path h)
    getDecl h = liftIO $ S.getFromFile
      (V1.getEither (V1.getEffectDeclaration getV getA)
                    (V1.getDataDeclaration getV getA)
      )
      (declPath path h)
    putDecl h decl = liftIO $ do
      S.putWithParentDirs
        (V1.putEither (V1.putEffectDeclaration putV putA)
                      (V1.putDataDeclaration putV putA)
        )
        (declPath path h)
        decl
      traverse_ (touchDependentFile h . dependentsDir path) deps
      where deps = deleteComponent h . DD.dependencies . either DD.toDataDecl id $ decl

    dependents :: MonadIO m =>
                  Reference -> m (Set Reference.Id)
    dependents r = do
      let d = dependentsDir path r
      e <- doesDirectoryExist d
      if e then do
        ls <- listDirectory d
        pure . Set.fromList $ ls >>= (toList . parseHash)
      else pure Set.empty

-- watches in `branchHeadDir root` for externally deposited heads;
-- parse them, and return them
branchHeadUpdates :: MonadUnliftIO m
                  => CodebasePath -> m (m (), m (Set Branch.Hash))
branchHeadUpdates root = do
  branchHeadChanges      <- TQueue.newIO
  (cancelWatch, watcher) <- Watch.watchDirectory' (branchHeadDir root)
--  -- add .ubf file changes to intermediate queue
  watcher1               <- forkIO $ do
    forever $ do
      -- Q: what does watcher return on a file deletion?
      -- A: nothing
      (filePath, _) <- watcher
      case hashFromFilePath filePath of
        Nothing -> failWith $ CantParseBranchHead filePath
        Just h -> atomically . TQueue.enqueue branchHeadChanges $ Branch.Hash h
  -- smooth out intermediate queue
  pure
    $ ( cancelWatch >> killThread watcher1
      , Set.fromList <$> Watch.collectUntilPause branchHeadChanges 400000
      )

hashFromFilePath :: FilePath -> Maybe Hash.Hash
hashFromFilePath = Hash.fromBase58 . Text.pack . takeBaseName

failWith :: MonadIO m => Err -> m a
failWith = fail . show

deleteComponent :: Reference.Id -> Set Reference -> Set Reference
deleteComponent r rs = Set.difference rs
  (Reference.members . Reference.componentFor . Reference.DerivedId $ r)
