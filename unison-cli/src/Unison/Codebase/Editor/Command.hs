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
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.Type (GitError)
import qualified Unison.CommandLine.FuzzySelect as Fuzzy
import Unison.DataDeclaration (Decl)
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.Parser as Parser
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Result (Note, Result)
import Unison.Server.Backend (DefinitionResults, IncludeCycles, ShallowListEntry)
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
    (Either Names (UF.TypecheckedUnisonFile v Ann))

data
  Command
    m -- Command monad
    i -- Input type
    v -- Type of variables in the codebase
    a -- Result of running the command
  where
  -- Escape hatch.
  Eval :: m a -> Command m i v a
  UI :: Command m i v ()
  API :: Command m i v ()
  DocsToHtml ::
    Branch m -> -- Root branch

    -- | namespace source
    Path ->
    -- | file destination
    FilePath ->
    Command m i v ()
  HQNameQuery ::
    Maybe Path ->
    Branch m ->
    [HQ.HashQualified Name] ->
    Command m i v QueryResult
  LoadSearchResults ::
    [SR.SearchResult] ->
    Command m i v [SR'.SearchResult' v Ann]
  GetDefinitionsBySuffixes ::
    Maybe Path ->
    Branch m ->
    IncludeCycles ->
    [HQ.HashQualified Name] ->
    Command m i v (DefinitionResults v)
  FindShallow ::
    Path.Absolute ->
    Command m i v [ShallowListEntry v Ann]
  ConfigLookup :: Configured a => Text -> Command m i v (Maybe a)
  Input :: Command m i v i
  -- Presents some output to the user
  Notify :: Output v -> Command m i v ()
  NotifyNumbered :: NumberedOutput v -> Command m i v NumberedArgs
  -- the hash length needed to disambiguate any definition in the codebase
  CodebaseHashLength :: Command m i v Int
  TypeReferencesByShortHash :: ShortHash -> Command m i v (Set Reference)
  TermReferencesByShortHash :: ShortHash -> Command m i v (Set Reference)
  TermReferentsByShortHash :: ShortHash -> Command m i v (Set Referent)
  -- the hash length needed to disambiguate any branch in the codebase
  BranchHashLength :: Command m i v Int
  BranchHashesByPrefix :: ShortBranchHash -> Command m i v (Set Branch.CausalHash)
  ParseType ::
    NamesWithHistory ->
    LexedSource ->
    Command m i v (Either (Parser.Err v) (Type v Ann))
  LoadSource :: SourceName -> Command m i v LoadSourceResult
  Typecheck ::
    AmbientAbilities v ->
    NamesWithHistory ->
    SourceName ->
    LexedSource ->
    Command m i v (TypecheckingResult v)
  TypecheckFile ::
    UF.UnisonFile v Ann ->
    [Type v Ann] ->
    Command m i v (TypecheckingResult v)
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
    Command m i v (Either Runtime.Error (EvalResult v))
  -- Evaluate a single closed definition
  Evaluate1 :: Bool -> PPE.PrettyPrintEnv -> UseCache -> Term v Ann -> Command m i v (Either Runtime.Error (Term v Ann))
  -- Add a cached watch to the codebase
  PutWatch :: WK.WatchKind -> Reference.Id -> Term v Ann -> Command m i v ()
  -- Loads any cached watches of the given kind
  LoadWatches :: WK.WatchKind -> Set Reference -> Command m i v [(Reference, Term v Ann)]
  -- Like `LoadLocalRootBranch`.
  LoadLocalBranch :: Branch.CausalHash -> Command m i v (Branch m)
  -- Merge two branches, using the codebase for the LCA calculation where possible.
  Merge :: Branch.MergeMode -> Branch m -> Branch m -> Command m i v (Branch m)
  ViewRemoteGitBranch ::
    ReadGitRemoteNamespace ->
    Git.GitBranchBehavior ->
    (Branch m -> (Free (Command m i v) r)) ->
    Command m i v (Either GitError r)
  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch m -> Command m i v ()
  AppendToReflog :: Text -> Branch m -> Branch m -> Command m i v ()
  -- load the reflog in file (chronological) order
  LoadReflog :: Command m i v [Reflog.Entry Branch.CausalHash]
  PutTerm :: Reference.Id -> Term v Ann -> Type v Ann -> Command m i v ()
  PutDecl :: Reference.Id -> Decl v Ann -> Command m i v ()
  -- IsDerivedTerm :: H.Hash -> Command m i v Bool
  -- IsDerivedType :: H.Hash -> Command m i v Bool

  GetTermsMentioningType :: Type v Ann -> Command m i v (Set Referent)
  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> [String] -> Command m i v (Runtime.WatchResults v Ann)
  CreateAuthorInfo :: Text -> Command m i v (AuthorInfo v Ann)
  RuntimeMain :: Command m i v (Type v Ann)
  RuntimeTest :: Command m i v (Type v Ann)
  ClearWatchCache :: Command m i v ()
  AnalyzeCodebaseIntegrity :: Command m i v IntegrityResult
  MakeStandalone :: PPE.PrettyPrintEnv -> Reference -> String -> Command m i v (Maybe Runtime.Error)
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
    Command m i v (Maybe [a])
  -- | This allows us to implement MonadUnliftIO for (Free (Command m i v)).
  -- Ideally we will eventually remove the Command type entirely and won't need
  -- this anymore.
  CmdUnliftIO :: Command m i v (UnliftIO (Free (Command m i v)))
  UCMVersion :: Command m i v UCMVersion

instance MonadIO m => MonadIO (Free (Command m i v)) where
  liftIO io = Free.eval $ Eval (liftIO io)

instance MonadIO m => MonadUnliftIO (Free (Command m i v)) where
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

commandName :: Command m i v a -> String
commandName = \case
  Eval {} -> "Eval"
  API -> "API"
  UI -> "UI"
  DocsToHtml {} -> "DocsToHtml"
  ConfigLookup {} -> "ConfigLookup"
  Input -> "Input"
  Notify {} -> "Notify"
  NotifyNumbered {} -> "NotifyNumbered"
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
  LoadLocalBranch {} -> "LoadLocalBranch"
  Merge {} -> "Merge"
  ViewRemoteGitBranch {} -> "ViewRemoteGitBranch"
  SyncLocalRootBranch {} -> "SyncLocalRootBranch"
  AppendToReflog {} -> "AppendToReflog"
  LoadReflog -> "LoadReflog"
  PutTerm {} -> "PutTerm"
  PutDecl {} -> "PutDecl"
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
