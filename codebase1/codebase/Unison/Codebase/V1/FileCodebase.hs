{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.V1.FileCodebase
  ( getRootBranch, -- used by Git module
    codebaseExists, -- used by Main
    getCodebaseDir,
    termsDir,
    reflogPath,
    getTerm,
    getTypeOfTerm,
    getDecl,
    getWatch,
    deserializeEdits,
  )
where

import Control.Error (ExceptT (..), runExceptT)
import Control.Monad.Catch (catch)
import Control.Monad.Extra (ifM)
import Data.Either.Extra (maybeToEither)
import Data.Functor ((<&>))
import qualified Data.Text as Text
import System.Directory (getHomeDirectory)
import qualified System.Directory
import System.FilePath ((</>))
import qualified U.Util.Base32Hex as Base32Hex
import qualified U.Util.Hash as Hash
import qualified Unison.Codebase.V1.Branch.Raw as Branch
import Unison.Codebase.V1.Branch.Raw (BranchHash (..), EditHash (..))
import qualified Unison.Codebase.V1.Causal.Raw as Causal
import qualified Unison.Codebase.V1.DataDeclaration as DD
import Unison.Codebase.V1.Patch (Patch (..))
import Unison.Codebase.V1.Reference (Reference)
import qualified Unison.Codebase.V1.Reference as Reference
import qualified Unison.Codebase.V1.Serialization.Serialization as S
import qualified Unison.Codebase.V1.Serialization.V1 as V1
import Unison.Codebase.V1.Term (Term)
import Unison.Codebase.V1.Type (Type)
import UnliftIO (MonadIO)
import UnliftIO (IOException)
import UnliftIO (MonadIO (liftIO))
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)

newtype CodebasePath = CodebasePath FilePath

data WatchKind = RegularWatch | TestWatch deriving (Eq, Ord, Show)

getCodebaseDir :: Maybe FilePath -> IO FilePath
getCodebaseDir = maybe getHomeDirectory pure

data Err
  = InvalidBranchFile FilePath String
  | InvalidEditsFile FilePath String
  | NoBranchHead FilePath
  | CantParseBranchHead FilePath
  | AmbiguouslyTypeAndTerm Reference.Id
  | UnknownTypeOrTerm Reference
  deriving (Show)

codebasePath :: FilePath
codebasePath = ".unison" </> "v1"

termsDir, typesDir, branchesDir, branchHeadDir, editsDir :: CodebasePath -> FilePath
termsDir (CodebasePath root) = root </> codebasePath </> "terms"
typesDir (CodebasePath root) = root </> codebasePath </> "types"
branchesDir (CodebasePath root) = root </> codebasePath </> "paths"
branchHeadDir root = branchesDir root </> "_head"
editsDir (CodebasePath root) = root </> codebasePath </> "patches"

termDir, declDir :: CodebasePath -> Reference.Id -> FilePath
termDir root r = termsDir root </> componentIdToString r
declDir root r = typesDir root </> componentIdToString r

watchesDir :: CodebasePath -> WatchKind -> FilePath
watchesDir (CodebasePath root) k =
  root </> codebasePath </> "watches" </> case k of
    RegularWatch -> "_cache"
    TestWatch -> "test"

watchPath :: CodebasePath -> WatchKind -> Reference.Id -> FilePath
watchPath root kind id =
  watchesDir root kind </> componentIdToString id <> ".ub"

termPath, typePath, declPath :: CodebasePath -> Reference.Id -> FilePath
termPath path r = termDir path r </> "compiled.ub"
typePath path r = termDir path r </> "type.ub"
declPath path r = declDir path r </> "compiled.ub"

branchPath :: CodebasePath -> BranchHash -> FilePath
branchPath root (BranchHash h) = branchesDir root </> hashToString h ++ ".ub"

editsPath :: CodebasePath -> EditHash -> FilePath
editsPath root (EditHash h) = editsDir root </> hashToString h ++ ".up"

reflogPath :: CodebasePath -> FilePath
reflogPath (CodebasePath root) = root </> codebasePath </> "reflog"

-- checks if `path` looks like a unison codebase
minimalCodebaseStructure :: CodebasePath -> [FilePath]
minimalCodebaseStructure root = [branchHeadDir root]

-- checks if a minimal codebase structure exists at `path`
codebaseExists :: MonadIO m => CodebasePath -> m Bool
codebaseExists root =
  and <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

deserializeEdits :: MonadIO m => CodebasePath -> Branch.EditHash -> m Patch
deserializeEdits root h =
  let file = editsPath root h
   in S.getFromFile' V1.getEdits file >>= \case
        Left err -> failWith $ InvalidEditsFile file err
        Right edits -> pure edits

data GetRootBranchError
  = NoRootBranch
  | CouldntParseRootBranch FilePath
  | CouldntLoadRootBranch BranchHash
  | ConflictedRootBranch [FilePath]
  deriving (Show)

getRootBranch ::
  forall m.
  MonadIO m =>
  CodebasePath ->
  m (Either GetRootBranchError (Causal.Raw Branch.Raw Branch.Raw))
getRootBranch root =
  ifM
    (codebaseExists root)
    (listDirectory (branchHeadDir root) >>= filesToBranch)
    (pure $ Left NoRootBranch)
  where
    filesToBranch :: [FilePath] -> m (Either GetRootBranchError (Causal.Raw Branch.Raw Branch.Raw))
    filesToBranch = \case
      [] -> pure $ Left NoRootBranch
      [single] -> runExceptT $ fileToBranch single
      conflict -> pure $ Left $ ConflictedRootBranch conflict
    fileToBranch :: FilePath -> ExceptT GetRootBranchError m (Causal.Raw Branch.Raw Branch.Raw)
    fileToBranch single = ExceptT $ case hashFromString single of
      Nothing -> pure . Left $ CouldntParseRootBranch single
      Just (BranchHash -> h) ->
        branchFromFiles root h
          <&> maybeToEither (CouldntLoadRootBranch h)
    branchFromFiles :: MonadIO m => CodebasePath -> BranchHash -> m (Maybe (Causal.Raw Branch.Raw Branch.Raw))
    branchFromFiles rootDir h = do
      fileExists <- doesFileExist (branchPath rootDir h)
      if fileExists
        then Just <$> deserializeRawBranch rootDir h
        else pure Nothing
      where
        deserializeRawBranch ::
          MonadIO m => CodebasePath -> BranchHash -> m (Causal.Raw Branch.Raw Branch.Raw)
        deserializeRawBranch root h = do
          let ubf = branchPath root h
          S.getFromFile' (V1.getCausal0 V1.getRawBranch) ubf >>= \case
            Left err -> failWith $ InvalidBranchFile ubf err
            Right c0 -> pure c0

-- here
hashFromString :: String -> Maybe Hash.Hash
hashFromString = fmap (Hash.fromBase32Hex . Base32Hex.fromByteString) . Base32Hex.textToByteString . Text.pack

-- here
hashToString :: Hash.Hash -> String
hashToString = Text.unpack . Base32Hex.toText . Hash.toBase32Hex

-- hashFromFilePath :: FilePath -> Maybe Hash.Hash
-- hashFromFilePath = hashFromString . takeBaseName

-- here
componentIdToString :: Reference.Id -> String
componentIdToString = Text.unpack . Reference.toText . Reference.DerivedId

-- here
-- componentIdFromString :: String -> Maybe Reference.Id
-- componentIdFromString = Reference.idFromText . Text.pack

-- here
-- referentFromString :: String -> Maybe Referent
-- referentFromString = Referent.fromText . Text.pack

-- referentIdFromString :: String -> Maybe Referent.Id
-- referentIdFromString s = referentFromString s >>= \case
--   Referent.Ref (Reference.DerivedId r) -> Just $ Referent.Ref' r
--   Referent.Con (Reference.DerivedId r) i t -> Just $ Referent.Con' r i t
--   _ -> Nothing

-- here
-- referentToString :: Referent -> String
-- referentToString = Text.unpack . Referent.toText

getTerm :: (MonadIO m, Ord v) => S.Get v -> S.Get a -> CodebasePath -> Reference.Id -> m (Maybe (Term v a))
getTerm getV getA path h = S.getFromFile (V1.getTerm getV getA) (termPath path h)

getTypeOfTerm :: (MonadIO m, Ord v) => S.Get v -> S.Get a -> CodebasePath -> Reference.Id -> m (Maybe (Type v a))
getTypeOfTerm getV getA path h = S.getFromFile (V1.getType getV getA) (typePath path h)

getDecl ::
  (MonadIO m, Ord v) =>
  S.Get v ->
  S.Get a ->
  CodebasePath ->
  Reference.Id ->
  m (Maybe (DD.Decl v a))
getDecl getV getA root h =
  S.getFromFile
    ( V1.getEither
        (V1.getEffectDeclaration getV getA)
        (V1.getDataDeclaration getV getA)
    )
    (declPath root h)

getWatch ::
  (MonadIO m, Ord v) =>
  S.Get v ->
  S.Get a ->
  CodebasePath ->
  WatchKind ->
  Reference.Id ->
  m (Maybe (Term v a))
getWatch getV getA path k id = do
  let wp = watchesDir path k
  createDirectoryIfMissing True wp
  S.getFromFile (V1.getTerm getV getA) (watchPath path k id)

failWith :: MonadIO m => Err -> m a
failWith = liftIO . fail . show

-- | A version of listDirectory that returns mempty if the directory doesn't exist
listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory dir =
  liftIO $
    System.Directory.listDirectory dir `catch` (\(_ :: IOException) -> pure mempty)

-- -- | delete all the elements of a given reference component from a set
-- deleteComponent :: Reference.Id -> Set Reference -> Set Reference
-- deleteComponent r rs = Set.difference rs
--   (Reference.members . Reference.componentFor . Reference.DerivedId $ r)
