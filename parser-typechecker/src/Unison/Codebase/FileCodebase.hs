--{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- todo: delete

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Codebase.FileCodebase
( getRootBranch        -- used by Git module
, branchHashesByPrefix -- used by Git module
, branchFromFiles      -- used by Git module
, BranchLoadMode(..)   -- used by Git module
, codebase1  -- used by Main
, exists     -- used by Main
, initialize -- used by Main
-- todo: where are these used?
, decodeFileName
, encodeFileName
, codebasePath
, initCodebaseAndExit
, initCodebase
, getCodebaseOrExit
, getCodebaseDir
) where

import Unison.Prelude

import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Exception             ( catchIO )
import           UnliftIO.Concurrent            ( forkIO
                                                , killThread
                                                )
import           UnliftIO.STM                   ( atomically )
import qualified Data.Char                     as Char
import qualified Data.Hex                      as Hex
import           Data.List                      ( isSuffixOf, isPrefixOf )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           UnliftIO.Directory             ( createDirectoryIfMissing
                                                , doesFileExist
                                                , doesDirectoryExist
                                                , listDirectory
                                                , createDirectory
                                                , removeFile
                                                , doesPathExist
                                                )
import           System.FilePath                ( FilePath
                                                , takeBaseName
                                                , takeFileName
                                                , (</>)
                                                )
import           System.Directory               ( copyFile, getHomeDirectory )
import           System.Path                    ( replaceRoot
                                                , createDir
                                                , subDirs
                                                , files
                                                , dirPath
                                                )
import           System.Exit                    ( exitFailure, exitSuccess )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase(Codebase)
                                                , BuiltinAnnotation
                                                )
import           Unison.Codebase.Causal         ( Causal
                                                , RawHash(..)
                                                )
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.BranchLoadMode ( BranchLoadMode(FailIfMissing, EmptyIfMissing) )
import qualified Unison.Codebase.Reflog        as Reflog
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1
                                               as V1

import           Unison.Codebase.Patch          ( Patch(..) )
import qualified Unison.Codebase.Watch         as Watch
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Hash                   as Hash
import           Unison.Parser                  ( Ann(External) )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent(..) )
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import           Unison.Type                    ( Type )
import qualified Unison.Type                   as Type
import qualified Unison.Util.TQueue            as TQueue
import           Unison.Var                     ( Var )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Pretty            as P
import qualified Unison.PrettyTerminal         as PT
import           Unison.Symbol                  ( Symbol )
import Unison.Codebase.ShortBranchHash (ShortBranchHash(..))
import qualified Unison.Codebase.ShortBranchHash as SBH
import qualified Unison.Util.TrieMap as TMap

type CodebasePath = FilePath

data Err
  = InvalidBranchFile FilePath String
  | InvalidEditsFile FilePath String
  | NoBranchHead FilePath
  | CantParseBranchHead FilePath
  | AmbiguouslyTypeAndTerm Reference.Id
  | UnknownTypeOrTerm Reference
  deriving Show

codebasePath :: FilePath
codebasePath = ".unison" </> "v1"

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())

initCodebaseAndExit :: Maybe FilePath -> IO ()
initCodebaseAndExit mdir = do
  dir <- getCodebaseDir mdir
  _ <- initCodebase dir
  exitSuccess

initCodebase :: FilePath -> IO (Codebase IO Symbol Ann)
initCodebase dir = do
  let path = dir </> codebasePath
  let theCodebase = codebase1 V1.formatSymbol formatAnn path
  let prettyDir = P.endSentence . P.string $ dir
  whenM (exists path) $
    do PT.putPrettyLn'
         .  P.warnCallout
         .  P.wrap
         $  "It looks like there's already a codebase in: "
         <> prettyDir
       exitFailure
  PT.putPrettyLn'
    .  P.warnCallout
    .  P.wrap
    $  "Initializing a new codebase in: "
    <> prettyDir
  initialize path
  Codebase.initializeCodebase theCodebase
  pure theCodebase

-- get the codebase in dir, or in the home directory if not provided.
getCodebaseOrExit :: Maybe FilePath -> IO (Codebase IO Symbol Ann)
getCodebaseOrExit mdir = do
  (dir, errMsg) <- case mdir of
    Just dir -> pure
      ( dir
      , "No codebase exists in "
          <> (P.endSentence . P.string $ dir)
          <> P.newline
          <> "Run `ucm -codebase "
          <> P.string dir
          <> " init` to create one, then try again!" )
    Nothing -> do
      dir <- getHomeDirectory
      let errMsg = P.lines [ "No codebase exists in " <> (P.endSentence . P.string $ dir)
                           , "Run `ucm init` to create one, then try again!" ]
      pure (dir, errMsg)

  let path = dir </> codebasePath
  let theCodebase = codebase1 V1.formatSymbol formatAnn path
  Codebase.initializeBuiltinCode theCodebase
  unlessM (exists path) $ do
    PT.putPrettyLn'. P.warnCallout . P.wrap $ errMsg
    exitFailure
  pure theCodebase

getCodebaseDir :: Maybe FilePath -> IO FilePath
getCodebaseDir mdir =
  case mdir of Just dir -> pure dir
               Nothing  -> getHomeDirectory

termsDir, typesDir, branchesDir, branchHeadDir, editsDir
  :: CodebasePath -> FilePath
termsDir root = root </> "terms"
typesDir root = root </> "types"
branchesDir root = root </> "paths"
branchHeadDir root = branchesDir root </> "_head"
editsDir root = root </> "patches"

termDir, declDir :: CodebasePath -> Reference.Id -> FilePath
termDir root r = termsDir root </> componentIdToString r
declDir root r = typesDir root </> componentIdToString r

referenceToDir :: Reference -> FilePath
referenceToDir r = case r of
  Reference.Builtin name -> "_builtin" </> encodeFileName (Text.unpack name)
  Reference.DerivedId hash -> componentIdToString hash

dependentsDir :: CodebasePath -> Reference -> FilePath
dependentsDir root r = root </> "dependents" </> referenceToDir r

watchesDir :: CodebasePath -> Text -> FilePath
watchesDir root UF.RegularWatch = root </> "watches" </> "_cache"
watchesDir root kind = root </> "watches" </> encodeFileName (Text.unpack kind)

typeIndexDir :: CodebasePath -> Reference -> FilePath
typeIndexDir root r = root </> "type-index" </> referenceToDir r

typeMentionsIndexDir :: CodebasePath -> Reference -> FilePath
typeMentionsIndexDir root r = root </> "type-mentions-index" </> referenceToDir r

-- todo: decodeFileName & encodeFileName shouldn't use base58; recommend $xFF$
decodeFileName :: FilePath -> String
decodeFileName = go where
  go ('$':tl) = case span (/= '$') tl of
    ("forward-slash", _:tl) -> '/' : go tl
    ("back-slash", _:tl) ->  '\\' : go tl
    ("colon", _:tl) -> ':' : go tl
    ("star", _:tl) -> '*' : go tl
    ("question-mark", _:tl) -> '?' : go tl
    ("double-quote", _:tl) -> '\"' : go tl
    ("less-than", _:tl) -> '<' : go tl
    ("greater-than", _:tl) -> '>' : go tl
    ("pipe", _:tl) -> '|' : go tl
    ('x':hex, _:tl) -> decodeHex hex ++ go tl
    ("",_:tl) -> '$' : go tl
    (s,_:tl) -> s ++ go tl
    (s,[]) -> s
  go (hd:tl) = hd : tl
  go [] = []
  decodeHex :: String -> String
  decodeHex s = maybe s (Text.unpack . decodeUtf8)
              . Hex.unhex . encodeUtf8 . Text.pack $ s

-- https://superuser.com/questions/358855/what-characters-are-safe-in-cross-platform-file-names-for-linux-windows-and-os
encodeFileName :: String -> FilePath
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
                 = "$x" <> encodeHex [c] <> "$" <> go rem
               | otherwise = c : go rem
  go [] = []
  encodeHex = Text.unpack . decodeUtf8 . Hex.hex . encodeUtf8 . Text.pack
  in if t == "." then "$dot$"
     else if t == ".." then "$dotdot$"
     else go t

termPath, typePath, declPath :: CodebasePath -> Reference.Id -> FilePath
termPath path r = termDir path r </> "compiled.ub"
typePath path r = termDir path r </> "type.ub"
declPath path r = declDir path r </> "compiled.ub"

branchPath :: CodebasePath -> Hash.Hash -> FilePath
branchPath root h = branchesDir root </> hashToString h ++ ".ub"

editsPath :: CodebasePath -> Hash.Hash -> FilePath
editsPath root h = editsDir root </> hashToString h ++ ".up"

reflogPath :: CodebasePath -> FilePath
reflogPath root = root </> "reflog"

touchIdFile :: Reference.Id -> FilePath -> IO ()
touchIdFile id fp = do
  createDirectoryIfMissing True fp
  -- note: contents of the file are equal to the name, rather than empty, to
  -- hopefully avoid git getting clever about treating deletions as renames
  let n = componentIdToString id
  writeFile (fp </> encodeFileName n) n

touchReferentFile :: Referent -> FilePath -> IO ()
touchReferentFile id fp = do
  createDirectoryIfMissing True fp
  -- note: contents of the file are equal to the name, rather than empty, to
  -- hopefully avoid git getting clever about treating deletions as renames
  let n = referentToString id
  writeFile (fp </> encodeFileName n) n

-- checks if `path` looks like a unison codebase
minimalCodebaseStructure :: CodebasePath -> [FilePath]
minimalCodebaseStructure root = [ branchHeadDir root ]

-- checks if a minimal codebase structure exists at `path`
exists :: MonadIO m => CodebasePath -> m Bool
exists root =
  and <$> traverse doesDirectoryExist (minimalCodebaseStructure root)

-- creates a minimal codebase structure at `path`
initialize :: CodebasePath -> IO ()
initialize path =
  traverse_ (createDirectoryIfMissing True) (minimalCodebaseStructure path)

branchFromFiles :: MonadIO m => BranchLoadMode -> FilePath -> Branch.Hash -> m (Branch m)
branchFromFiles loadMode rootDir h@(RawHash h') = do
  fileExists <- doesFileExist (branchPath rootDir h')
  if fileExists || loadMode == FailIfMissing then
    Branch.read (deserializeRawBranch rootDir)
                (deserializeEdits rootDir)
                h
  else
    pure Branch.empty
 where
  deserializeRawBranch
    :: MonadIO m => CodebasePath -> Causal.Deserialize m Branch.Raw Branch.Raw
  deserializeRawBranch root (RawHash h) = do
    let ubf = branchPath root h
    liftIO (S.getFromFile' (V1.getCausal0 V1.getRawBranch) ubf) >>= \case
      Left  err -> failWith $ InvalidBranchFile ubf err
      Right c0  -> pure c0
  deserializeEdits :: MonadIO m => CodebasePath -> Branch.EditHash -> m Patch
  deserializeEdits root h =
    let file = editsPath root h
    in  liftIO (S.getFromFile' V1.getEdits file) >>= \case
          Left  err   -> failWith $ InvalidEditsFile file err
          Right edits -> pure edits

-- returns Nothing if `root` has no root branch (in `branchHeadDir root`)
getRootBranch ::
  MonadIO m => BranchLoadMode -> CodebasePath -> m (Branch m)
getRootBranch loadMode root = do
  ifM (exists root)
    (liftIO (listDirectory $ branchHeadDir root) >>= \case
      []       -> missing
      [single] -> go single
      conflict -> traverse go conflict >>= \case
        x : xs -> foldM Branch.merge x xs
        []     -> missing
    )
    missing
 where
  go single = case hashFromString single of
    Nothing -> failWith $ CantParseBranchHead single
    Just h  -> branchFromFiles FailIfMissing root (RawHash h)
  missing = case loadMode of
    FailIfMissing -> failWith . NoBranchHead $ branchHeadDir root
    EmptyIfMissing -> pure $ Branch.empty

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
      hs = hashToString h
  -- write new head
  exists <- doesDirectoryExist headDir
  unless exists $ createDirectory headDir
  liftIO $ writeFile (headDir </> hs) hs
  -- delete existing heads
  liftIO $ fmap (filter (/= hs)) (listDirectory headDir)
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

-- here
referentToString :: Referent -> String
referentToString = Text.unpack . Referent.toText

-- Adapted from
-- http://hackage.haskell.org/package/fsutils-0.1.2/docs/src/System-Path.html
copyDir :: (FilePath -> Bool) -> FilePath -> FilePath -> IO ()
copyDir predicate from to = do
  createDirectoryIfMissing True to
  -- createDir doesn't create a new directory on disk,
  -- it creates a description of an existing directory,
  -- and it crashes if `from` doesn't exist.
  d <- createDir from
  when (predicate $ dirPath d) $ do
    forM_ (subDirs d)
      $ \path -> copyDir predicate path (replaceRoot from to path)
    forM_ (files d) $ \path -> do
      exists <- doesFileExist to
      unless exists . copyFile path $ replaceRoot from to path

copyFromGit :: MonadIO m => FilePath -> FilePath -> m ()
copyFromGit to from = liftIO . whenM (doesDirectoryExist from) $
  copyDir (\x -> not ((".git" `isSuffixOf` x) || ("_head" `isSuffixOf` x)))
          from to

-- Create a codebase structure at `localPath` if none exists, and
-- copy (merge) all codebase elements from the current codebase into it.
syncToDirectory
  :: forall m v a
   . (MonadUnliftIO m)
  => Var v
  => Codebase.BuiltinAnnotation a
  => S.Format v
  -> S.Format a
  -> Codebase m v a
  -> FilePath
  -> Branch m
  -> m (Branch m)
syncToDirectory fmtV fmtA codebase localPath branch = do
  b <- (liftIO . exists) localPath
  if b then do
    let code = codebase1 fmtV fmtA localPath
    remoteRoot <- Codebase.getRootBranch code
    Branch.sync (hashExists localPath) serialize (serializeEdits localPath) branch
    merged <- Branch.merge branch remoteRoot
    Codebase.putRootBranch code merged
    pure merged
  else do
    Branch.sync (hashExists localPath) serialize (serializeEdits localPath) branch
    updateCausalHead (branchHeadDir localPath) $ Branch._history branch
    pure branch
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
          maybe (calamity i) (putDecl (S.put fmtV) (S.put fmtA) localPath i) mayDecl
      Reference.Builtin{} -> pure ()
    -- Write all terms
    for_ (toList $ Star3.fact terms) $ \case
      Ref r@(Reference.DerivedId i) -> do
        alreadyExists <- liftIO . doesPathExist $ termPath localPath i
        unless alreadyExists $ do
          mayTerm <- Codebase.getTerm codebase i
          mayType <- Codebase.getTypeOfTerm codebase r
          fromMaybe (calamity i)
                    (putTerm (S.put fmtV) (S.put fmtA) localPath i <$> mayTerm <*> mayType)
          -- If the term is a test, write the cached value too.
          mayTest <- Codebase.getWatch codebase UF.TestWatch i
          maybe (pure ()) (putWatch (S.put fmtV) (S.put fmtA) localPath UF.TestWatch i) mayTest
      Ref Reference.Builtin{} -> pure ()
      Con{} -> pure ()

putTerm
  :: MonadIO m
  => Var v
  => S.Put v
  -> S.Put a
  -> FilePath
  -> Reference.Id
  -> Term.AnnotatedTerm v a
  -> Type v a
  -> m ()
putTerm putV putA path h e typ = liftIO $ do
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

putDecl
  :: MonadIO m
  => Var v
  => S.Put v
  -> S.Put a
  -> FilePath
  -> Reference.Id
  -> DD.Decl v a
  -> m ()
putDecl putV putA path h decl = liftIO $ do
  S.putWithParentDirs
    (V1.putEither (V1.putEffectDeclaration putV putA)
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
    let rootHash     = Type.toReference typ
        typeMentions = Type.toReferenceMentions typ
    touchReferentFile r (typeIndexDir path rootHash)
    traverse_ (touchReferentFile r . typeMentionsIndexDir path) typeMentions
  ct = DD.constructorType decl
  ctors =
    [ (Referent.Con r i ct, Type.removeAllEffectVars t)
    | (t,i) <- DD.constructorTypes decl' `zip` [0..] ]

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
  (watchesDir path (Text.pack k) </> componentIdToString id <> ".ub")
  e


shortestUniquePrefixLengthTerms :: forall m. MonadIO m => CodebasePath -> m Int
shortestUniquePrefixLengthTerms codebasePath = liftIO $ do
    allHashes <- getAllTermHashes
    pure $ TMap.minUniquePrefix allHashes
  where 
    getAllTermIDs :: IO [Reference.Id]
    getAllTermIDs = (fmap join) . for [termsDir, typesDir] $ \f -> do
      let dir = f codebasePath
      paths <- listDirectory dir
      let refs = paths >>= (toList . componentIdFromString)
      pure refs
    getAllTermHashes = do
      allIds <- getAllTermIDs
      let hashInfo = fmap (\ref@(Reference.Id hash _ _) -> (Text.unpack $ Hash.base32Hex hash, ref)) allIds
          hashMap = TMap.fromList hashInfo
      pure hashMap
      


referencesByPrefix :: MonadIO m => CodebasePath -> Text -> m (Set Reference.Id)
referencesByPrefix codebasePath p =
  liftIO $ fmap (Set.fromList . join) . for [termsDir, typesDir] $ \f -> do
    let dir = f codebasePath
    paths <- filter (isPrefixOf $ Text.unpack p) <$> listDirectory dir
    let refs = paths >>= (toList . componentIdFromString)
    pure refs

branchHashesByPrefix :: MonadIO m => CodebasePath -> ShortBranchHash -> m (Set Branch.Hash)
branchHashesByPrefix codebasePath p =
  liftIO $ fmap (Set.fromList . join) . for [branchesDir] $ \f -> do
    let dir = f codebasePath
    paths <- filter (isPrefixOf . Text.unpack . SBH.toText $ p) <$> listDirectory dir
    let refs = paths >>= (toList . filenameToHash)
    pure refs
  where
    filenameToHash :: String -> Maybe Branch.Hash
    filenameToHash f = case Text.splitOn "." $ Text.pack f of
      [h, "ub"] -> Causal.RawHash <$> Hash.fromBase32Hex h
      _ -> Nothing

-- builds a `Codebase IO v a`, given serializers for `v` and `a`
codebase1
  :: forall m v a
   . MonadUnliftIO m
  => Var v
  => BuiltinAnnotation a
  => S.Format v -> S.Format a -> CodebasePath -> Codebase m v a
codebase1 fmtV@(S.Format getV putV) fmtA@(S.Format getA putA) path =
  let c =
        Codebase
          getTerm
          getTypeOfTerm
          getDecl
          (putTerm putV putA path)
          (putDecl putV putA path)
          (getRootBranch FailIfMissing path)
          (putRootBranch path)
          (branchHeadUpdates path)
          (branchFromFiles EmptyIfMissing path)
          dependents
          -- Just copies all the files from a to-be-supplied path to `path`.
          (copyFromGit path)
          -- This is fine as long as watat doesn't call
          -- syncToDirectory c
          (syncToDirectory fmtV fmtA c)
          watches
          getWatch
          (putWatch putV putA path)
          getReflog
          appendReflog
          getTermsOfType
          getTermsMentioningType
   -- todo: maintain a trie of references to come up with this number
          (shortestUniquePrefixLengthTerms path)
   -- The same trie can be used to make this lookup fast:
          (referencesByPrefix path)
          (pure 10)
          (branchHashesByPrefix path)
   in c
  where
    getTerm h = liftIO $ S.getFromFile (V1.getTerm getV getA) (termPath path h)
    getTypeOfTerm h = liftIO $ S.getFromFile (V1.getType getV getA) (typePath path h)
    getDecl h =
      liftIO $
      S.getFromFile
        (V1.getEither (V1.getEffectDeclaration getV getA) (V1.getDataDeclaration getV getA))
        (declPath path h)
    dependents :: Reference -> m (Set Reference.Id)
    dependents r = listDirAsIds (dependentsDir path r)
    getTermsOfType :: Reference -> m (Set Referent)
    getTermsOfType r = listDirAsReferents (typeIndexDir path r)
    getTermsMentioningType :: Reference -> m (Set Referent)
    getTermsMentioningType r = listDirAsReferents (typeMentionsIndexDir path r)
  -- todo: revisit these
    listDirAsIds :: FilePath -> m (Set Reference.Id)
    listDirAsIds d = do
      e <- doesDirectoryExist d
      if e
        then do
          ls <- fmap decodeFileName <$> listDirectory d
          pure . Set.fromList $ ls >>= (toList . componentIdFromString)
        else pure Set.empty
    listDirAsReferents :: FilePath -> m (Set Referent)
    listDirAsReferents d = do
      e <- doesDirectoryExist d
      if e
        then do
          ls <- fmap decodeFileName <$> listDirectory d
          pure . Set.fromList $ ls >>= (toList . referentFromString)
        else pure Set.empty
    watches :: UF.WatchKind -> m [Reference.Id]
    watches k =
      liftIO $ do
        let wp = watchesDir path (Text.pack k)
        createDirectoryIfMissing True wp
        ls <- listDirectory wp
        pure $ ls >>= (toList . componentIdFromString . takeFileName)
    getWatch :: UF.WatchKind -> Reference.Id -> m (Maybe (Codebase.Term v a))
    getWatch k id =
      liftIO $ do
        let wp = watchesDir path (Text.pack k)
        createDirectoryIfMissing True wp
        S.getFromFile (V1.getTerm getV getA) (wp </> componentIdToString id <> ".ub")
    getReflog :: m [Reflog.Entry]
    getReflog =
      liftIO
        (do contents <- TextIO.readFile (reflogPath path)
            let lines = Text.lines contents
            let entries = parseEntry <$> lines
            pure entries) `catchIO`
      const (pure [])
      where
        parseEntry t = fromMaybe (err t) (Reflog.fromText t)
        err t = error $
          "I couldn't understand this line in " ++ reflogPath path ++ "\n\n" ++
          Text.unpack t
    appendReflog :: Text -> Branch m -> Branch m -> m ()
    appendReflog reason old new =
      let
        t = Reflog.toText $
          Reflog.Entry (Branch.headHash old) (Branch.headHash new) reason
      in liftIO $ TextIO.appendFile (reflogPath path) (t <> "\n")

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

failWith :: MonadIO m => Err -> m a
failWith = fail . show

deleteComponent :: Reference.Id -> Set Reference -> Set Reference
deleteComponent r rs = Set.difference rs
  (Reference.members . Reference.componentFor . Reference.DerivedId $ r)
