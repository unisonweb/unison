{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase
  ( getRootBranch, -- used by Git module
    branchHashesByPrefix, -- used by Git module
    branchFromFiles, -- used by Git module
    codebase1, -- used by Main
    codebase1', -- used by Test/Git
    codebaseExists, -- used by Main
    initCodebaseAndExit,
    initCodebase,
    getCodebaseOrExit,
    getCodebaseDir,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory
  ( canonicalizePath,
    getHomeDirectory,
  )
import System.Environment (getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
  ( takeFileName,
  )
import Unison.Codebase
  ( BuiltinAnnotation,
    Codebase (Codebase),
    CodebasePath,
  )
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.FileCodebase.Common
  ( Err (CantParseBranchHead),
    ---

    ---

    ---
    branchFromFiles,
    branchHashesByPrefix,
    branchHeadDir,
    codebaseExists,
    componentIdFromString,
    decodeFileName,
    dependentsDir,
    ---
    failWith,
    formatAnn,
    getDecl,
    getRootBranch,
    getTerm,
    getTypeOfTerm,
    getWatch,
    hashFromFilePath,
    listDirectory,
    putDecl,
    putRootBranch,
    putTerm,
    putWatch,
    referentIdFromString,
    reflogPath,
    termReferencesByPrefix,
    termReferentsByPrefix,
    typeIndexDir,
    typeMentionsIndexDir,
    typeReferencesByPrefix,
    watchesDir,
  )
import qualified Unison.Codebase.FileCodebase.Common as Common
import qualified Unison.Codebase.FileCodebase.SlimCopyRegenerateIndex as Sync
import qualified Unison.Codebase.Reflog as Reflog
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1 as V1
import qualified Unison.Codebase.Watch as Watch
import Unison.Parser (Ann ())
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Cache as Cache
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as TQueue
import Unison.Var (Var)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent
  ( forkIO,
    killThread,
  )
import UnliftIO.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
  )
import UnliftIO.Exception (catchIO)
import UnliftIO.STM (atomically)

initCodebaseAndExit :: Maybe FilePath -> IO ()
initCodebaseAndExit mdir = do
  dir <- getCodebaseDir mdir
  cache <- Cache.cache
  _ <- initCodebase cache dir
  exitSuccess

-- initializes a new codebase here (i.e. `ucm -codebase dir init`)
initCodebase :: Branch.Cache IO -> FilePath -> IO (Codebase IO Symbol Ann)
initCodebase cache path = do
  theCodebase <- codebase1 cache V1.formatSymbol Common.formatAnn path
  prettyDir <- P.string <$> canonicalizePath path

  whenM (codebaseExists path) $
    do
      PT.putPrettyLn'
        . P.wrap
        $ "It looks like there's already a codebase in: "
          <> prettyDir
      exitFailure

  PT.putPrettyLn'
    . P.wrap
    $ "Initializing a new codebase in: "
      <> prettyDir
  Codebase.initializeCodebase theCodebase
  pure theCodebase

-- get the codebase in dir, or in the home directory if not provided.
getCodebaseOrExit :: Branch.Cache IO -> Maybe FilePath -> IO (Codebase IO Symbol Ann)
getCodebaseOrExit cache mdir = do
  dir <- getCodebaseDir mdir
  progName <- getProgName
  prettyDir <- P.string <$> canonicalizePath dir
  let errMsg = getNoCodebaseErrorMsg ((P.text . Text.pack) progName) prettyDir mdir
  let theCodebase = codebase1 cache V1.formatSymbol formatAnn dir
  unlessM (codebaseExists dir) $ do
    PT.putPrettyLn' errMsg
    exitFailure
  theCodebase

getNoCodebaseErrorMsg :: IsString s => P.Pretty s -> P.Pretty s -> Maybe FilePath -> P.Pretty s
getNoCodebaseErrorMsg executable prettyDir mdir =
  let secondLine =
        case mdir of
          Just dir ->
            "Run `" <> executable <> " -codebase " <> fromString dir
              <> " init` to create one, then try again!"
          Nothing ->
            "Run `" <> executable <> " init` to create one there,"
              <> " then try again;"
              <> " or `"
              <> executable
              <> " -codebase <dir>` to load a codebase from someplace else!"
   in P.lines
        [ "No codebase exists in " <> prettyDir <> ".",
          secondLine
        ]

getCodebaseDir :: Maybe FilePath -> IO FilePath
getCodebaseDir = maybe getHomeDirectory pure

-- builds a `Codebase IO v a`, given serializers for `v` and `a`
codebase1 ::
  forall m v a.
  MonadUnliftIO m =>
  Var v =>
  BuiltinAnnotation a =>
  Branch.Cache m ->
  S.Format v ->
  S.Format a ->
  CodebasePath ->
  m (Codebase m v a)
codebase1 = codebase1' Sync.syncToDirectory

codebase1' ::
  forall m v a.
  MonadUnliftIO m =>
  Var v =>
  BuiltinAnnotation a =>
  Common.SyncToDir m v a ->
  Branch.Cache m ->
  S.Format v ->
  S.Format a ->
  CodebasePath ->
  m (Codebase m v a)
codebase1' syncToDirectory branchCache fmtV@(S.Format getV putV) fmtA@(S.Format getA putA) path = do
  termCache <- Cache.semispaceCache 8192
  typeOfTermCache <- Cache.semispaceCache 8192
  declCache <- Cache.semispaceCache 1024
  let c =
        Codebase
          (Cache.applyDefined termCache $ getTerm getV getA path)
          (Cache.applyDefined typeOfTermCache $ getTypeOfTerm getV getA path)
          (Cache.applyDefined declCache $ getDecl getV getA path)
          (putTerm putV putA path)
          (putDecl putV putA path)
          (getRootBranch branchCache path)
          (putRootBranch path)
          (branchHeadUpdates path)
          (branchFromFiles branchCache path)
          dependents
          (flip (syncToDirectory fmtV fmtA) path)
          (syncToDirectory fmtV fmtA path)
          watches
          (getWatch getV getA path)
          (putWatch putV putA path)
          getReflog
          appendReflog
          getTermsOfType
          getTermsMentioningType
          -- todo: maintain a trie of references to come up with this number
          (pure 10)
          -- The same trie can be used to make this lookup fast:
          (termReferencesByPrefix path)
          (typeReferencesByPrefix path)
          (termReferentsByPrefix (getDecl getV getA) path)
          (pure 10)
          (branchHashesByPrefix path)
   in pure c
  where
    dependents :: Reference -> m (Set Reference.Id)
    dependents r = listDirAsIds (dependentsDir path r)
    getTermsOfType :: Reference -> m (Set Referent.Id)
    getTermsOfType r = listDirAsReferents (typeIndexDir path r)
    getTermsMentioningType :: Reference -> m (Set Referent.Id)
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
    listDirAsReferents :: FilePath -> m (Set Referent.Id)
    listDirAsReferents d = do
      e <- doesDirectoryExist d
      if e
        then do
          ls <- fmap decodeFileName <$> listDirectory d
          pure . Set.fromList $ ls >>= (toList . referentIdFromString)
        else pure Set.empty
    watches :: UF.WatchKind -> m [Reference.Id]
    watches k =
      liftIO $ do
        let wp = watchesDir path (Text.pack k)
        createDirectoryIfMissing True wp
        ls <- listDirectory wp
        pure $ ls >>= (toList . componentIdFromString . takeFileName)
    getReflog :: m [Reflog.Entry]
    getReflog =
      liftIO
        ( do
            contents <- TextIO.readFile (reflogPath path)
            let lines = Text.lines contents
            let entries = parseEntry <$> lines
            pure entries
        )
        `catchIO` const (pure [])
      where
        parseEntry t = fromMaybe (err t) (Reflog.fromText t)
        err t =
          error $
            "I couldn't understand this line in " ++ reflogPath path ++ "\n\n"
              ++ Text.unpack t
    appendReflog :: Text -> Branch m -> Branch m -> m ()
    appendReflog reason old new =
      let t =
            Reflog.toText $
              Reflog.Entry (Branch.headHash old) (Branch.headHash new) reason
       in liftIO $ TextIO.appendFile (reflogPath path) (t <> "\n")

-- watches in `branchHeadDir root` for externally deposited heads;
-- parse them, and return them
branchHeadUpdates ::
  MonadUnliftIO m => CodebasePath -> m (m (), m (Set Branch.Hash))
branchHeadUpdates root = do
  branchHeadChanges <- TQueue.newIO
  (cancelWatch, watcher) <- Watch.watchDirectory' (branchHeadDir root)
  --  -- add .ubf file changes to intermediate queue
  watcher1 <-
    forkIO $
      forever $
        do
          -- Q: what does watcher return on a file deletion?
          -- A: nothing
          (filePath, _) <- watcher
          case hashFromFilePath filePath of
            Nothing -> failWith $ CantParseBranchHead filePath
            Just h ->
              atomically . TQueue.enqueue branchHeadChanges $ Branch.Hash h
  -- smooth out intermediate queue
  pure
    ( cancelWatch >> killThread watcher1,
      Set.fromList <$> Watch.collectUntilPause branchHeadChanges 400000
    )
