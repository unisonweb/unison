{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.Common
  ( Err (..),
    SyncToDir,
    SimpleLens,
    codebaseExists,
    codebasePath,
    hashExists,
    -- dirs (parent of all the files)
    branchHeadDir,
    dependentsDir,
    dependentsDir',
    typeIndexDir,
    typeIndexDir',
    typeMentionsIndexDir,
    typeMentionsIndexDir',
    watchesDir,
    -- paths (looking up one file)
    branchPath,
    declPath,
    editsPath,
    reflogPath,
    termPath,
    typePath,
    watchPath,
    -- core stuff
    formatAnn,
    getDecl,
    putDecl,
    putRootBranch,
    getTerm,
    getTypeOfTerm,
    putTerm,
    getWatch,
    putWatch,
    updateCausalHead,
    serializeEdits,
    deserializeEdits,
    serializeRawBranch,
    branchFromFiles,
    branchHashesByPrefix,
    termReferencesByPrefix,
    termReferentsByPrefix,
    typeReferencesByPrefix,
    -- stringing
    hashFromFilePath,
    componentIdFromString,
    componentIdToString,
    referentIdFromString,
    -- touching files
    touchIdFile,
    touchReferentFile,
    touchReferentIdFile,
    -- util
    copyFileWithParents,
    doFileOnce,
    failWith,
    listDirectory,
    -- expose for tests :|
    encodeFileName,
    decodeFileName,
    getRootBranch,
  )
where

import Control.Error (ExceptT (..), runExceptT)
import Control.Lens (Lens, to, use, (%=))
import Control.Monad.Catch (catch)
import Control.Monad.State (MonadState)
import qualified Data.ByteString.Base16 as ByteString (decodeBase16, encodeBase16)
import qualified Data.Char as Char
import Data.Either.Extra (maybeToEither)
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified System.Directory
import System.FilePath
  ( takeBaseName,
    takeDirectory,
    (</>),
  )
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Causal
  ( Causal,
    RawHash (..),
  )
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.Patch (Patch (..))
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1 as V1
import Unison.Codebase.ShortBranchHash (ShortBranchHash (..))
import qualified Unison.Codebase.ShortBranchHash as SBH
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.ConstructorType as CT
import qualified Unison.DataDeclaration as DD
import qualified Unison.Hash as Hash
import Unison.Parser (Ann (External))
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.UnisonFile as UF
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Timing (time)
import Unison.Var (Var)
import UnliftIO.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeFile,
  )
import UnliftIO.IO.File (writeBinaryFile)

data Err
  = InvalidBranchFile FilePath String
  | InvalidEditsFile FilePath String
  | NoBranchHead FilePath
  | CantParseBranchHead FilePath
  | AmbiguouslyTypeAndTerm Reference.Id
  | UnknownTypeOrTerm Reference
  deriving (Show)

type SimpleLens s a = Lens s s a a

codebasePath :: FilePath
codebasePath = ".unison" </> "v1"

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())

-- Write Branch and its dependents to the dest codebase, and set it as the root.
type SyncToDir m v a =
  S.Format v ->
  S.Format a ->
  CodebasePath -> -- src codebase
  CodebasePath -> -- dest codebase
  SyncMode ->
  Branch m -> -- new dest root branch
  m ()

termsDir,
  typesDir,
  branchesDir,
  branchHeadDir,
  editsDir ::
    CodebasePath -> FilePath
termsDir root = root </> codebasePath </> "terms"
typesDir root = root </> codebasePath </> "types"
branchesDir root = root </> codebasePath </> "paths"
branchHeadDir root = branchesDir root </> "_head"
editsDir root = root </> codebasePath </> "patches"

termDir, declDir :: CodebasePath -> Reference.Id -> FilePath
termDir root r = termsDir root </> componentIdToString r
declDir root r = typesDir root </> componentIdToString r

referenceToDir :: Reference -> FilePath
referenceToDir r = case r of
  Reference.Builtin name -> "_builtin" </> encodeFileName (Text.unpack name)
  Reference.DerivedId hash -> componentIdToString hash

dependentsDir', typeIndexDir', typeMentionsIndexDir' :: FilePath -> FilePath
dependentsDir :: CodebasePath -> Reference -> FilePath
dependentsDir root r = dependentsDir' root </> referenceToDir r
dependentsDir' root = root </> codebasePath </> "dependents"

watchesDir :: CodebasePath -> Text -> FilePath
watchesDir root UF.RegularWatch =
  root </> codebasePath </> "watches" </> "_cache"
watchesDir root kind =
  root </> codebasePath </> "watches" </> encodeFileName (Text.unpack kind)

watchPath :: CodebasePath -> UF.WatchKind -> Reference.Id -> FilePath
watchPath root kind id =
  watchesDir root (Text.pack kind) </> componentIdToString id <> ".ub"

typeIndexDir :: CodebasePath -> Reference -> FilePath
typeIndexDir root r = typeIndexDir' root </> referenceToDir r

typeIndexDir' root = root </> codebasePath </> "type-index"

typeMentionsIndexDir :: CodebasePath -> Reference -> FilePath
typeMentionsIndexDir root r = typeMentionsIndexDir' root </> referenceToDir r

typeMentionsIndexDir' root = root </> codebasePath </> "type-mentions-index"

decodeFileName :: FilePath -> String
decodeFileName =
  let go ('$' : tl) = case span (/= '$') tl of
        ("forward-slash", _ : tl) -> '/' : go tl
        ("back-slash", _ : tl) -> '\\' : go tl
        ("colon", _ : tl) -> ':' : go tl
        ("star", _ : tl) -> '*' : go tl
        ("question-mark", _ : tl) -> '?' : go tl
        ("double-quote", _ : tl) -> '\"' : go tl
        ("less-than", _ : tl) -> '<' : go tl
        ("greater-than", _ : tl) -> '>' : go tl
        ("pipe", _ : tl) -> '|' : go tl
        ('x' : hex, _ : tl) -> decodeHex hex ++ go tl
        ("", _ : tl) -> '$' : go tl
        (s, _ : tl) -> '$' : s ++ '$' : go tl -- unknown escapes left unchanged
        (s, []) -> s
      go (hd : tl) = hd : go tl
      go [] = []
      decodeHex :: String -> String
      decodeHex s =
        either (const s) (Text.unpack . decodeUtf8)
          . ByteString.decodeBase16
          . encodeUtf8
          . Text.pack
          $ s
   in \case
        "$dot$" -> "."
        "$dotdot$" -> ".."
        t -> go t

-- https://superuser.com/questions/358855/what-characters-are-safe-in-cross-platform-file-names-for-linux-windows-and-os
encodeFileName :: String -> FilePath
encodeFileName =
  let go ('/' : rem) = "$forward-slash$" <> go rem
      go ('\\' : rem) = "$back-slash$" <> go rem
      go (':' : rem) = "$colon$" <> go rem
      go ('*' : rem) = "$star$" <> go rem
      go ('?' : rem) = "$question-mark$" <> go rem
      go ('"' : rem) = "$double-quote$" <> go rem
      go ('<' : rem) = "$less-than$" <> go rem
      go ('>' : rem) = "$greater-than$" <> go rem
      go ('|' : rem) = "$pipe$" <> go rem
      go ('$' : rem) = "$$" <> go rem
      go (c : rem)
        | not (Char.isPrint c && Char.isAscii c) =
          "$x" <> encodeHex [c] <> "$" <> go rem
        | otherwise = c : go rem
      go [] = []
      encodeHex :: String -> String
      encodeHex =
        Text.unpack . Text.toUpper . ByteString.encodeBase16
          . encodeUtf8
          . Text.pack
   in \case
        "." -> "$dot$"
        ".." -> "$dotdot$"
        t -> go t

termPath, typePath, declPath :: CodebasePath -> Reference.Id -> FilePath
termPath path r = termDir path r </> "compiled.ub"
typePath path r = termDir path r </> "type.ub"
declPath path r = declDir path r </> "compiled.ub"

branchPath :: CodebasePath -> Branch.Hash -> FilePath
branchPath root (RawHash h) = branchesDir root </> hashToString h ++ ".ub"

editsPath :: CodebasePath -> Branch.EditHash -> FilePath
editsPath root h = editsDir root </> hashToString h ++ ".up"

reflogPath :: CodebasePath -> FilePath
reflogPath root = root </> codebasePath </> "reflog"

touchIdFile :: MonadIO m => Reference.Id -> FilePath -> m ()
touchIdFile id fp =
  touchFile (fp </> encodeFileName (componentIdToString id))

touchReferentFile :: MonadIO m => Referent -> FilePath -> m ()
touchReferentFile id fp =
  touchFile (fp </> encodeFileName (referentToString id))

touchReferentIdFile :: MonadIO m => Referent.Id -> FilePath -> m ()
touchReferentIdFile = touchReferentFile . Referent.fromId

touchFile :: MonadIO m => FilePath -> m ()
touchFile fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  writeBinaryFile fp mempty

-- checks if `path` looks like a unison codebase
minimalCodebaseStructure :: CodebasePath -> [FilePath]
minimalCodebaseStructure root = [branchHeadDir root]

-- checks if a minimal codebase structure exists at `path`
codebaseExists :: MonadIO m => CodebasePath -> m Bool
codebaseExists root =
  and <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

-- | load a branch w/ children from a FileCodebase
branchFromFiles :: MonadIO m => Branch.Cache m -> CodebasePath -> Branch.Hash -> m (Maybe (Branch m))
branchFromFiles cache rootDir h = time "FileCodebase.Common.branchFromFiles" $ do
  fileExists <- doesFileExist (branchPath rootDir h)
  if fileExists
    then
      Just
        <$> Branch.cachedRead
          cache
          (deserializeRawBranch rootDir)
          (deserializeEdits rootDir)
          h
    else pure Nothing
  where
    deserializeRawBranch ::
      MonadIO m => CodebasePath -> Causal.Deserialize m Branch.Raw Branch.Raw
    deserializeRawBranch root h = do
      let ubf = branchPath root h
      S.getFromFile' (V1.getCausal0 V1.getRawBranch) ubf >>= \case
        Left err -> failWith $ InvalidBranchFile ubf err
        Right c0 -> pure c0

deserializeEdits :: MonadIO m => CodebasePath -> Branch.EditHash -> m Patch
deserializeEdits root h =
  let file = editsPath root h
   in S.getFromFile' V1.getEdits file >>= \case
        Left err -> failWith $ InvalidEditsFile file err
        Right edits -> pure edits

getRootBranch ::
  forall m.
  MonadIO m =>
  Branch.Cache m ->
  CodebasePath ->
  m (Either Codebase.GetRootBranchError (Branch m))
getRootBranch cache root =
  time "FileCodebase.Common.getRootBranch" $
    ifM
      (codebaseExists root)
      (listDirectory (branchHeadDir root) >>= filesToBranch)
      (pure $ Left Codebase.NoRootBranch)
  where
    filesToBranch :: [FilePath] -> m (Either Codebase.GetRootBranchError (Branch m))
    filesToBranch = \case
      [] -> pure $ Left Codebase.NoRootBranch
      [single] -> runExceptT $ fileToBranch single
      conflict ->
        runExceptT (traverse fileToBranch conflict) >>= \case
          Right (x : xs) -> Right <$> foldM Branch.merge x xs
          Right _ -> error "FileCodebase.getRootBranch.conflict can't be empty."
          Left e -> Left <$> pure e

    fileToBranch :: String -> ExceptT Codebase.GetRootBranchError m (Branch m)
    fileToBranch single = ExceptT $ case hashFromString single of
      Nothing -> pure . Left $ Codebase.CouldntParseRootBranch single
      Just (Branch.Hash -> h) ->
        branchFromFiles cache root h
          <&> maybeToEither (Codebase.CouldntLoadRootBranch h)

-- | only syncs branches and edits -- no dependencies
putRootBranch :: MonadIO m => CodebasePath -> Branch m -> m ()
putRootBranch root b = do
  Branch.sync
    (hashExists root)
    (serializeRawBranch root)
    (serializeEdits root)
    b
  updateCausalHead (branchHeadDir root) (Branch._history b)

hashExists :: MonadIO m => CodebasePath -> Branch.Hash -> m Bool
hashExists root h = doesFileExist (branchPath root h)

serializeRawBranch ::
  (MonadIO m) => CodebasePath -> Causal.Serialize m Branch.Raw Branch.Raw
serializeRawBranch root h =
  S.putWithParentDirs (V1.putRawCausal V1.putRawBranch) (branchPath root h)

serializeEdits ::
  MonadIO m => CodebasePath -> Branch.EditHash -> m Patch -> m ()
serializeEdits root h medits =
  unlessM (doesFileExist (editsPath root h)) $ do
    edits <- medits
    S.putWithParentDirs V1.putEdits (editsPath root h) edits

-- `headDir` is like ".unison/branches/head", or ".unison/edits/head";
-- not ".unison"; a little weird.  I guess the reason this doesn't take
-- the codebase root path is because it's applicable to any causal.
-- We just have one though, and I suppose that won't change any time soon.
updateCausalHead :: MonadIO m => FilePath -> Causal n h e -> m ()
updateCausalHead headDir c = do
  let (RawHash h) = Causal.currentHash c
      hs = hashToString h
  -- write new head
  touchFile (headDir </> hs)
  -- delete existing heads
  fmap (filter (/= hs)) (listDirectory headDir)
    >>= traverse_ (removeFile . (headDir </>))

-- here
hashFromString :: String -> Maybe Hash.Hash
hashFromString = Hash.fromBase32Hex . Text.pack

-- here
hashToString :: Hash.Hash -> String
hashToString = Hash.base32Hexs

hashFromFilePath :: FilePath -> Maybe Hash.Hash
hashFromFilePath = hashFromString . takeBaseName

-- here
componentIdToString :: Reference.Id -> String
componentIdToString = Text.unpack . Reference.toText . Reference.DerivedId

-- here
componentIdFromString :: String -> Maybe Reference.Id
componentIdFromString = Reference.idFromText . Text.pack

-- here
referentFromString :: String -> Maybe Referent
referentFromString = Referent.fromText . Text.pack

referentIdFromString :: String -> Maybe Referent.Id
referentIdFromString s =
  referentFromString s >>= \case
    Referent.Ref (Reference.DerivedId r) -> Just $ Referent.Ref' r
    Referent.Con (Reference.DerivedId r) i t -> Just $ Referent.Con' r i t
    _ -> Nothing

-- here
referentToString :: Referent -> String
referentToString = Text.unpack . Referent.toText

copyFileWithParents :: MonadIO m => FilePath -> FilePath -> m ()
copyFileWithParents src dest =
  unlessM (doesFileExist dest) $ do
    createDirectoryIfMissing True (takeDirectory dest)
    copyFile src dest

-- Use State and Lens to do some specified thing at most once, to create a file.
doFileOnce ::
  forall m s h.
  (MonadIO m, MonadState s m, Ord h) =>
  CodebasePath ->
  SimpleLens s (Set h) -> -- lens to track if `h` is already done
  (CodebasePath -> h -> FilePath) -> -- done if this filepath exists
  (h -> m ()) -> -- do!
  h ->
  m ()
doFileOnce destPath l getFilename f h =
  unlessM (use (l . to (Set.member h))) $ do
    l %= Set.insert h
    unlessM (doesFileExist (getFilename destPath h)) (f h)

getTerm :: (MonadIO m, Ord v) => S.Get v -> S.Get a -> CodebasePath -> Reference.Id -> m (Maybe (Term v a))
getTerm getV getA path h = S.getFromFile (V1.getTerm getV getA) (termPath path h)

getTypeOfTerm :: (MonadIO m, Ord v) => S.Get v -> S.Get a -> CodebasePath -> Reference.Id -> m (Maybe (Type v a))
getTypeOfTerm getV getA path h = S.getFromFile (V1.getType getV getA) (typePath path h)

putTerm ::
  MonadIO m =>
  Var v =>
  S.Put v ->
  S.Put a ->
  CodebasePath ->
  Reference.Id ->
  Term v a ->
  Type v a ->
  m ()
putTerm putV putA path h e typ = do
  let typeForIndexing = Type.removeAllEffectVars typ
      rootTypeHash = Type.toReference typeForIndexing
      typeMentions = Type.toReferenceMentions typeForIndexing
  S.putWithParentDirs (V1.putTerm putV putA) (termPath path h) e
  S.putWithParentDirs (V1.putType putV putA) (typePath path h) typ
  -- Add the term as a dependent of its dependencies
  let r = Referent.Ref (Reference.DerivedId h)
  let deps = deleteComponent h $ Term.dependencies e <> Type.dependencies typ
  traverse_ (touchIdFile h . dependentsDir path) deps
  traverse_ (touchReferentFile r . typeMentionsIndexDir path) typeMentions
  touchReferentFile r (typeIndexDir path rootTypeHash)

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

putDecl ::
  MonadIO m =>
  Var v =>
  S.Put v ->
  S.Put a ->
  CodebasePath ->
  Reference.Id ->
  DD.Decl v a ->
  m ()
putDecl putV putA path h decl = do
  S.putWithParentDirs
    ( V1.putEither
        (V1.putEffectDeclaration putV putA)
        (V1.putDataDeclaration putV putA)
    )
    (declPath path h)
    decl
  traverse_ (touchIdFile h . dependentsDir path) deps
  traverse_ addCtorToTypeIndex ctors
  where
    deps = deleteComponent h . DD.dependencies $ either DD.toDataDecl id decl
    r = Reference.DerivedId h
    decl' = either DD.toDataDecl id decl
    addCtorToTypeIndex (r, typ) = do
      let rootHash = Type.toReference typ
          typeMentions = Type.toReferenceMentions typ
      touchReferentFile r (typeIndexDir path rootHash)
      traverse_ (touchReferentFile r . typeMentionsIndexDir path) typeMentions
    ct = DD.constructorType decl
    ctors =
      [ (Referent.Con r i ct, Type.removeAllEffectVars t)
        | (t, i) <- DD.constructorTypes decl' `zip` [0 ..]
      ]

getWatch ::
  (MonadIO m, Ord v) =>
  S.Get v ->
  S.Get a ->
  CodebasePath ->
  UF.WatchKind ->
  Reference.Id ->
  m (Maybe (Term v a))
getWatch getV getA path k id = do
  let wp = watchesDir path (Text.pack k)
  createDirectoryIfMissing True wp
  S.getFromFile (V1.getTerm getV getA) (wp </> componentIdToString id <> ".ub")

putWatch ::
  MonadIO m =>
  Var v =>
  S.Put v ->
  S.Put a ->
  CodebasePath ->
  UF.WatchKind ->
  Reference.Id ->
  Term v a ->
  m ()
putWatch putV putA root k id e =
  S.putWithParentDirs
    (V1.putTerm putV putA)
    (watchPath root k id)
    e

loadReferencesByPrefix ::
  MonadIO m => FilePath -> ShortHash -> m (Set Reference.Id)
loadReferencesByPrefix dir sh = do
  refs <-
    mapMaybe Reference.fromShortHash
      . filter (SH.isPrefixOf sh)
      . mapMaybe SH.fromString
      <$> listDirectory dir
  pure $ Set.fromList [i | Reference.DerivedId i <- refs]

termReferencesByPrefix,
  typeReferencesByPrefix ::
    MonadIO m => CodebasePath -> ShortHash -> m (Set Reference.Id)
termReferencesByPrefix root = loadReferencesByPrefix (termsDir root)
typeReferencesByPrefix root = loadReferencesByPrefix (typesDir root)

-- returns all the derived terms and derived constructors
-- that have `sh` as a prefix
termReferentsByPrefix ::
  MonadIO m =>
  (CodebasePath -> Reference.Id -> m (Maybe (DD.Decl v a))) ->
  CodebasePath ->
  ShortHash ->
  m (Set Referent.Id)
termReferentsByPrefix _ root sh@SH.Builtin {} =
  Set.map Referent.Ref' <$> termReferencesByPrefix root sh
-- builtin types don't provide any referents we could match against,
-- only decl types do. Those get handled in the next case.
termReferentsByPrefix getDecl root sh@SH.ShortHash {} = do
  terms <- termReferencesByPrefix root sh
  ctors <- do
    -- clear out any CID from the SH, so we can use it to find a type decl
    types <- typeReferencesByPrefix root sh {SH.cid = Nothing}
    foldMapM collectCtors types
  pure (Set.map Referent.Ref' terms <> ctors)
  where
    -- load up the Decl for `ref` to see how many constructors it has,
    -- and what constructor type
    collectCtors ref =
      getDecl root ref <&> \case
        Nothing -> mempty
        Just decl ->
          Set.fromList
            [ con
              | i <- [0 .. ctorCount -1],
                let con = Referent.Con' ref i ct,
                SH.isPrefixOf sh $ Referent.toShortHashId con
            ]
          where
            ct = either (const CT.Effect) (const CT.Data) decl
            ctorCount = length . DD.constructors' $ DD.asDataDecl decl

branchHashesByPrefix :: MonadIO m => CodebasePath -> ShortBranchHash -> m (Set Branch.Hash)
branchHashesByPrefix codebasePath p =
  fmap (Set.fromList . join) . for [branchesDir] $ \f -> do
    let dir = f codebasePath
    paths <- filter (isPrefixOf . Text.unpack . SBH.toText $ p) <$> listDirectory dir
    let refs = paths >>= (toList . filenameToHash)
    pure refs
  where
    filenameToHash :: String -> Maybe Branch.Hash
    filenameToHash f = case Text.splitOn "." $ Text.pack f of
      [h, "ub"] -> Causal.RawHash <$> Hash.fromBase32Hex h
      _ -> Nothing

failWith :: MonadIO m => Err -> m a
failWith = liftIO . fail . show

-- | A version of listDirectory that returns mempty if the directory doesn't exist
listDirectory :: MonadIO m => FilePath -> m [FilePath]
listDirectory dir =
  liftIO $
    System.Directory.listDirectory dir `catch` (\(_ :: IOException) -> pure mempty)

-- | delete all the elements of a given reference component from a set
deleteComponent :: Reference.Id -> Set Reference -> Set Reference
deleteComponent r rs =
  Set.difference
    rs
    (Reference.members . Reference.componentFor . Reference.DerivedId $ r)
