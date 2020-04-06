{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase
( getRootBranch        -- used by Git module
, branchHashesByPrefix -- used by Git module
, branchFromFiles      -- used by Git module
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
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           UnliftIO.Directory             ( createDirectoryIfMissing
                                                , doesDirectoryExist
                                                , listDirectory
                                                )
import           System.FilePath                ( FilePath
                                                , takeFileName
                                                , (</>)
                                                )
import           System.Directory               (
                                                 getHomeDirectory
                                                , canonicalizePath
                                                )
import           System.Exit                    ( exitFailure, exitSuccess )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase(Codebase)
                                                , BuiltinAnnotation
                                                )
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Reflog        as Reflog
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1
                                               as V1
import qualified Unison.Codebase.Watch         as Watch
import           Unison.Parser                  (Ann() )
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.Util.TQueue            as TQueue
import           Unison.Var                     ( Var )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Pretty            as P
import qualified Unison.PrettyTerminal         as PT
import           Unison.Symbol                  ( Symbol )
import Unison.Codebase.FileCodebase.Common
import Unison.Codebase.FileCodebase.Reserialize (syncToDirectory)
--import Unison.Codebase.FileCodebase.CopyRegenerateIndex (syncToDirectory)
--import Unison.Codebase.FileCodebase.CopyFilterIndex (syncToDirectory)

initCodebaseAndExit :: Maybe FilePath -> IO ()
initCodebaseAndExit mdir = do
  dir <- getCodebaseDir mdir
  _ <- initCodebase dir
  exitSuccess

-- initializes a new codebase here (i.e. `ucm -codebase dir init`)
initCodebase :: FilePath -> IO (Codebase IO Symbol Ann)
initCodebase dir = do
  let path = dir </> codebasePath
  let theCodebase = codebase1 V1.formatSymbol formatAnn path
  prettyDir <- P.string <$> canonicalizePath dir

  whenM (exists path) $
    do PT.putPrettyLn'
         .  P.wrap
         $  "It looks like there's already a codebase in: "
         <> prettyDir
       exitFailure

  PT.putPrettyLn'
    .  P.wrap
    $  "Initializing a new codebase in: "
    <> prettyDir
  initialize path
  Codebase.initializeCodebase theCodebase
  pure theCodebase

-- get the codebase in dir, or in the home directory if not provided.
getCodebaseOrExit :: Maybe FilePath -> IO (Codebase IO Symbol Ann)
getCodebaseOrExit mdir = do
  dir <- getCodebaseDir mdir
  prettyDir <- P.string <$> canonicalizePath dir
  let errMsg = P.lines
        [ "No codebase exists in " <> prettyDir
        , "Run `ucm -codebase " <> prettyDir
          <> " init` to create one, then try again!"]
  let path = dir </> codebasePath
  let theCodebase = codebase1 V1.formatSymbol formatAnn path
  Codebase.initializeBuiltinCode theCodebase
  unlessM (exists path) $ do
    PT.putPrettyLn' errMsg
    exitFailure
  pure theCodebase

getCodebaseDir :: Maybe FilePath -> IO FilePath
getCodebaseDir mdir =
  case mdir of Just dir -> pure dir
               Nothing  -> getHomeDirectory

-- builds a `Codebase IO v a`, given serializers for `v` and `a`
codebase1
  :: forall m v a
   . MonadUnliftIO m
  => Var v
  => BuiltinAnnotation a
  => S.Format v -> S.Format a -> CodebasePath -> Codebase m v a
--codebase1 (S.Format getV putV) (S.Format getA putA) path =
codebase1 fmtV@(S.Format getV putV) fmtA@(S.Format getA putA) path =
  let c =
        Codebase
          (getTerm getV getA path)
          (getTypeOfTerm getV getA path)
          (getDecl getV getA path)
          (putTerm putV putA path)
          (putDecl putV putA path)
          (getRootBranch path)
          (putRootBranch path)
          (branchHeadUpdates path)
          (branchFromFiles path)
          dependents
          -- Just copies all the files from a to-be-supplied path to `path`.
          (copyFromGit path)
          (syncToDirectory fmtV fmtA path)
--          (syncToDirectory getV getA path)
--          (syncToDirectory path)
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
   in c
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
