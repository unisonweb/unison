{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor.Command
  ( Command (..),
    AmbientAbilities,
    LexedSource,
    Source,
    SourceName,
    TypecheckingResult,
    LoadSourceResult (..),
    UseCache,
    EvalResult,
    commandName,
    lookupEvalResult,
  )
where

import Control.Lens (view, _5)
-- TODO: Don't import backend, but move dependencies to own modules

import Data.Configurator.Types (Configured)
import qualified Data.Map as Map
import Unison.Codebase (Preprocessing, PushGitBranchOpts)
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import Unison.Codebase.Editor.AuthorInfo (AuthorInfo)
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.RemoteRepo
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Codebase.IntegrityCheck (IntegrityResult)
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Reflog as Reflog
import qualified Unison.Codebase.Runtime as Runtime
import Unison.Codebase.ShortBranchHash
  ( ShortBranchHash,
  )
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.Type (GitError)
import qualified Unison.CommandLine.FuzzySelect as Fuzzy
import Unison.DataDeclaration (Decl)
import qualified Unison.Hash as H
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import Unison.Name (Name)
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.Parser as Parser
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Result
  ( Note,
    Result,
  )
import Unison.Server.Backend
  ( DefinitionResults,
    IncludeCycles,
    ShallowListEntry,
  )
import Unison.Server.QueryResult (QueryResult)
import qualified Unison.Server.SearchResult as SR
import qualified Unison.Server.SearchResult' as SR'
import Unison.ShortHash (ShortHash)
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import Unison.Util.Free (Free)
import qualified Unison.Util.Free as Free
import qualified Unison.WatchKind as WK
import UnliftIO (UnliftIO)
import qualified UnliftIO

type AmbientAbilities v = [Type v Ann]

type SourceName = Text

type Source = Text

type LexedSource = (Text, [L.Token L.Lexeme])

data LoadSourceResult
  = InvalidSourceNameError
  | LoadError
  | LoadSuccess Text

type TypecheckingResult v =
  Result
    (Seq (Note v Ann))
    (Either (UF.UnisonFile v Ann) (UF.TypecheckedUnisonFile v Ann))

data
  Command
    i -- Input type
    v -- Type of variables in the codebase
    a -- Result of running the command
  where
  -- Escape hatch.
  Eval :: IO a -> Command i v a
  UI :: Command i v ()
  API :: Command i v ()
  DocsToHtml ::
    Branch IO -> -- Root branch

    -- | namespace source
    Path ->
    -- | file destination
    FilePath ->
    Command i v ()
  HQNameQuery ::
    Maybe Path ->
    Branch IO ->
    [HQ.HashQualified Name] ->
    Command i v QueryResult
  LoadSearchResults ::
    [SR.SearchResult] ->
    Command i v [SR'.SearchResult' v Ann]
  GetDefinitionsBySuffixes ::
    Maybe Path ->
    Branch IO ->
    IncludeCycles ->
    [HQ.HashQualified Name] ->
    Command i v (DefinitionResults v)
  FindShallow ::
    Path.Absolute ->
    Command i v [ShallowListEntry v Ann]
  ConfigLookup :: Configured a => Text -> Command i v (Maybe a)
  Input :: Command i v i
  -- Presents some output to the user
  Notify :: Output v -> Command i v ()
  NotifyNumbered :: NumberedOutput v -> Command i v NumberedArgs
  -- literally just write some terms and types .unison/{terms,types}
  AddDefsToCodebase :: UF.TypecheckedUnisonFile v Ann -> Command i v ()
  -- the hash length needed to disambiguate any definition in the codebase
  CodebaseHashLength :: Command i v Int
  TypeReferencesByShortHash :: ShortHash -> Command i v (Set Reference)
  TermReferencesByShortHash :: ShortHash -> Command i v (Set Reference)
  TermReferentsByShortHash :: ShortHash -> Command i v (Set Referent)
  -- the hash length needed to disambiguate any branch in the codebase
  BranchHashLength :: Command i v Int
  BranchHashesByPrefix :: ShortBranchHash -> Command i v (Set Branch.CausalHash)
  ParseType ::
    NamesWithHistory ->
    LexedSource ->
    Command i v (Either (Parser.Err v) (Type v Ann))
  LoadSource :: SourceName -> Command i v LoadSourceResult
  Typecheck ::
    AmbientAbilities v ->
    NamesWithHistory ->
    SourceName ->
    LexedSource ->
    Command i v (TypecheckingResult v)
  TypecheckFile ::
    UF.UnisonFile v Ann ->
    [Type v Ann] ->
    Command i v (TypecheckingResult v)
  -- Evaluate all watched expressions in a UnisonFile and return
  -- their results, keyed by the name of the watch variable. The tuple returned
  -- has the form:
  --   (hash, (ann, sourceTerm, evaluatedTerm, isCacheHit))
  --
  -- where
  --   `hash` is the hash of the original watch expression definition
  --   `ann` gives the location of the watch expression
  --   `sourceTerm` is a closed term (no free vars) for the watch expression
  --   `evaluatedTerm` is the result of evaluating that `sourceTerm`
  --   `isCacheHit` is True if the result was computed by just looking up
  --   in a cache
  --
  -- It's expected that the user of this action might add the
  -- `(hash, evaluatedTerm)` mapping to a cache to make future evaluations
  -- of the same watches instantaneous.

  Evaluate ::
    Bool -> -- sandboxed
    PPE.PrettyPrintEnv ->
    UF.TypecheckedUnisonFile v Ann ->
    Command i v (Either Runtime.Error (EvalResult v))
  -- Evaluate a single closed definition
  Evaluate1 :: Bool -> PPE.PrettyPrintEnv -> UseCache -> Term v Ann -> Command i v (Either Runtime.Error (Term v Ann))
  -- Add a cached watch to the codebase
  PutWatch :: WK.WatchKind -> Reference.Id -> Term v Ann -> Command i v ()
  -- Loads any cached watches of the given kind
  LoadWatches :: WK.WatchKind -> Set Reference -> Command i v [(Reference, Term v Ann)]
  -- Loads a root branch from some codebase, returning `Nothing` if not found.
  -- Any definitions in the head of the requested root that aren't in the local
  -- codebase are copied there.
  LoadLocalRootBranch :: Command i v (Branch IO)
  -- Like `LoadLocalRootBranch`.
  LoadLocalBranch :: Branch.CausalHash -> Command i v (Branch IO)
  -- Merge two branches, using the codebase for the LCA calculation where possible.
  Merge :: Branch.MergeMode -> Branch IO -> Branch IO -> Command i v (Branch IO)
  ViewRemoteGitBranch ::
    ReadGitRemoteNamespace ->
    Git.GitBranchBehavior ->
    (Branch IO -> (Free (Command i v) r)) ->
    Command i v (Either GitError r)
  -- we want to import as little as possible, so we pass the SBH/path as part
  -- of the `RemoteNamespace`.  The Branch that's returned should be fully
  -- imported and not retain any resources from the remote codebase
  ImportRemoteGitBranch ::
    ReadGitRemoteNamespace ->
    SyncMode ->
    -- | A preprocessing step to perform on the branch before it's imported.
    -- This is sometimes useful for minimizing the number of definitions to sync.
    -- Simply pass 'pure' if you don't need to do any pre-processing.
    Preprocessing IO ->
    Command i v (Either GitError (Branch IO))
  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch IO -> Command i v ()
  SyncRemoteGitBranch :: WriteGitRepo -> PushGitBranchOpts -> (Branch IO -> IO (Either e (Branch IO))) -> Command i v (Either GitError (Either e (Branch IO)))
  AppendToReflog :: Text -> Branch IO -> Branch IO -> Command i v ()
  -- load the reflog in file (chronological) order
  LoadReflog :: Command i v [Reflog.Entry Branch.CausalHash]
  LoadTerm :: Reference.Id -> Command i v (Maybe (Term v Ann))
  -- LoadTermComponent :: H.Hash -> Command m i v (Maybe [Term v Ann])
  LoadTermComponentWithTypes :: H.Hash -> Command i v (Maybe [(Term v Ann, Type v Ann)])
  -- todo: change this to take Reference and return DeclOrBuiltin
  -- todo: change this to LoadDecl
  LoadType :: Reference.Id -> Command i v (Maybe (Decl v Ann))
  LoadDeclComponent :: H.Hash -> Command i v (Maybe [Decl v Ann])
  LoadTypeOfTerm :: Reference -> Command i v (Maybe (Type v Ann))
  PutTerm :: Reference.Id -> Term v Ann -> Type v Ann -> Command i v ()
  PutDecl :: Reference.Id -> Decl v Ann -> Command i v ()
  -- todo: eliminate these hopefully
  -- (why, again? because we can know from the Reference?)
  IsTerm :: Reference -> Command i v Bool
  IsType :: Reference -> Command i v Bool
  -- IsDerivedTerm :: H.Hash -> Command m i v Bool
  -- IsDerivedType :: H.Hash -> Command m i v Bool

  -- | Get the immediate (not transitive) dependents of the given reference
  -- This might include historical definitions not in any current path; these
  -- should be filtered by the caller of this command if that's not desired.
  GetDependents :: Reference -> Command i v (Set Reference)
  GetDependentsOfComponent :: H.Hash -> Command i v (Set Reference)
  GetTermsOfType :: Type v Ann -> Command i v (Set Referent)
  GetTermsMentioningType :: Type v Ann -> Command i v (Set Referent)
  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> [String] -> Command i v (Runtime.WatchResults v Ann)
  CreateAuthorInfo :: Text -> Command i v (AuthorInfo v Ann)
  RuntimeMain :: Command i v (Type v Ann)
  RuntimeTest :: Command i v (Type v Ann)
  ClearWatchCache :: Command i v ()
  AnalyzeCodebaseIntegrity :: Command i v IntegrityResult
  MakeStandalone :: PPE.PrettyPrintEnv -> Reference -> String -> Command i v (Maybe Runtime.Error)
  -- | Trigger an interactive fuzzy search over the provided options and return all
  -- selected results.
  FuzzySelect ::
    -- | Configure the selection.
    Fuzzy.Options ->
    -- | Select the text to fuzzy find on
    (a -> Text) ->
    -- | The elements to select from
    [a] ->
    -- | The selected results, or Nothing if a failure occurred.
    Command i v (Maybe [a])
  -- | This allows us to implement MonadUnliftIO for (Free (Command i v)).
  -- Ideally we will eventually remove the Command type entirely and won't need
  -- this anymore.
  CmdUnliftIO :: Command i v (UnliftIO (Free (Command i v)))
  UCMVersion :: Command i v UCMVersion

instance MonadIO (Free (Command i v)) where
  liftIO io = Free.eval $ Eval io

instance MonadUnliftIO (Free (Command i v)) where
  withRunInIO f = do
    UnliftIO.UnliftIO toIO <- Free.eval CmdUnliftIO
    liftIO $ f toIO

type UseCache = Bool

type EvalResult v =
  ( [(v, Term v ())],
    Map v (Ann, WK.WatchKind, Reference.Id, Term v (), Term v (), Runtime.IsCacheHit)
  )

lookupEvalResult :: Ord v => v -> EvalResult v -> Maybe (Term v ())
lookupEvalResult v (_, m) = view _5 <$> Map.lookup v m

commandName :: Command i v a -> String
commandName = \case
  Eval {} -> "Eval"
  API -> "API"
  UI -> "UI"
  DocsToHtml {} -> "DocsToHtml"
  ConfigLookup {} -> "ConfigLookup"
  Input -> "Input"
  Notify {} -> "Notify"
  NotifyNumbered {} -> "NotifyNumbered"
  AddDefsToCodebase {} -> "AddDefsToCodebase"
  CodebaseHashLength -> "CodebaseHashLength"
  TypeReferencesByShortHash {} -> "TypeReferencesByShortHash"
  TermReferencesByShortHash {} -> "TermReferencesByShortHash"
  TermReferentsByShortHash {} -> "TermReferentsByShortHash"
  BranchHashLength -> "BranchHashLength"
  BranchHashesByPrefix {} -> "BranchHashesByPrefix"
  ParseType {} -> "ParseType"
  LoadSource {} -> "LoadSource"
  Typecheck {} -> "Typecheck"
  TypecheckFile {} -> "TypecheckFile"
  Evaluate {} -> "Evaluate"
  Evaluate1 {} -> "Evaluate1"
  PutWatch {} -> "PutWatch"
  LoadWatches {} -> "LoadWatches"
  LoadLocalRootBranch -> "LoadLocalRootBranch"
  LoadLocalBranch {} -> "LoadLocalBranch"
  Merge {} -> "Merge"
  ViewRemoteGitBranch {} -> "ViewRemoteGitBranch"
  ImportRemoteGitBranch {} -> "ImportRemoteGitBranch"
  SyncLocalRootBranch {} -> "SyncLocalRootBranch"
  SyncRemoteGitBranch {} -> "SyncRemoteGitBranch"
  AppendToReflog {} -> "AppendToReflog"
  LoadReflog -> "LoadReflog"
  LoadTerm {} -> "LoadTerm"
  LoadTermComponentWithTypes {} -> "LoadTermComponentWithTypes"
  LoadType {} -> "LoadType"
  LoadTypeOfTerm {} -> "LoadTypeOfTerm"
  LoadDeclComponent {} -> "LoadDeclComponent"
  PutTerm {} -> "PutTerm"
  PutDecl {} -> "PutDecl"
  IsTerm {} -> "IsTerm"
  IsType {} -> "IsType"
  GetDependents {} -> "GetDependents"
  GetDependentsOfComponent {} -> "GetDependentsOfComponent"
  GetTermsOfType {} -> "GetTermsOfType"
  GetTermsMentioningType {} -> "GetTermsMentioningType"
  Execute {} -> "Execute"
  CreateAuthorInfo {} -> "CreateAuthorInfo"
  RuntimeMain -> "RuntimeMain"
  RuntimeTest -> "RuntimeTest"
  HQNameQuery {} -> "HQNameQuery"
  LoadSearchResults {} -> "LoadSearchResults"
  GetDefinitionsBySuffixes {} -> "GetDefinitionsBySuffixes"
  FindShallow {} -> "FindShallow"
  ClearWatchCache {} -> "ClearWatchCache"
  MakeStandalone {} -> "MakeStandalone"
  FuzzySelect {} -> "FuzzySelect"
  CmdUnliftIO {} -> "UnliftIO"
  UCMVersion {} -> "UCMVersion"
  AnalyzeCodebaseIntegrity -> "AnalyzeCodebaseIntegrity"
