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
    RunInIO (..),
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
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.Codebase.Editor.Git as Git
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.RemoteRepo
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Runtime as Runtime
import Unison.Codebase.Type (GitError)
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import Unison.Result (Note, Result)
import Unison.Server.Backend (DefinitionResults, IncludeCycles)
import Unison.Server.QueryResult (QueryResult)
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
  GetDefinitionsBySuffixes ::
    Maybe Path ->
    Branch IO ->
    IncludeCycles ->
    [HQ.HashQualified Name] ->
    Command i v (DefinitionResults v)
  ConfigLookup :: Configured a => Text -> Command i v (Maybe a)
  Input :: Command i v i
  -- Presents some output to the user
  Notify :: Output v -> Command i v ()
  NotifyNumbered :: NumberedOutput v -> Command i v NumberedArgs
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
  -- Merge two branches, using the codebase for the LCA calculation where possible.
  Merge :: Branch.MergeMode -> Branch IO -> Branch IO -> Command i v (Branch IO)
  ViewRemoteGitBranch ::
    ReadGitRemoteNamespace ->
    Git.GitBranchBehavior ->
    (Branch IO -> (Free (Command i v) r)) ->
    Command i v (Either GitError r)
  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch IO -> Command i v ()
  -- IsDerivedTerm :: H.Hash -> Command m i v Bool
  -- IsDerivedType :: H.Hash -> Command m i v Bool

  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> [String] -> Command i v (Runtime.WatchResults v Ann)
  -- | Trigger an interactive fuzzy search over the provided options and return all
  -- selected results.
  -- | This allows us to implement MonadUnliftIO for (Free (Command i v)).
  -- Ideally we will eventually remove the Command type entirely and won't need
  -- this anymore.
  CmdUnliftIO :: Command i v (UnliftIO (Free (Command i v)))
  ResetAndUnlift :: Command i v (RunInIO i v)
  Abort :: Command i v a

instance MonadIO (Free (Command i v)) where
  liftIO io = Free.eval $ Eval io

instance MonadUnliftIO (Free (Command i v)) where
  withRunInIO f = do
    UnliftIO.UnliftIO toIO <- Free.eval CmdUnliftIO
    liftIO $ f toIO

newtype RunInIO i v
  = RunInIO (forall x. Free (Command i v) x -> IO x)

type UseCache = Bool

type EvalResult v =
  ( [(v, Term v ())],
    Map v (Ann, WK.WatchKind, Reference.Id, Term v (), Term v (), Runtime.IsCacheHit)
  )

lookupEvalResult :: Ord v => v -> EvalResult v -> Maybe (Term v ())
lookupEvalResult v (_, m) = view _5 <$> Map.lookup v m

commandName :: Command i v a -> String
commandName = \case
  Abort -> "Abort"
  ResetAndUnlift {} -> "ResetAndUnlift"
  Eval {} -> "Eval"
  API -> "API"
  UI -> "UI"
  DocsToHtml {} -> "DocsToHtml"
  ConfigLookup {} -> "ConfigLookup"
  Input -> "Input"
  Notify {} -> "Notify"
  NotifyNumbered {} -> "NotifyNumbered"
  LoadSource {} -> "LoadSource"
  Typecheck {} -> "Typecheck"
  TypecheckFile {} -> "TypecheckFile"
  Evaluate {} -> "Evaluate"
  Evaluate1 {} -> "Evaluate1"
  Merge {} -> "Merge"
  ViewRemoteGitBranch {} -> "ViewRemoteGitBranch"
  SyncLocalRootBranch {} -> "SyncLocalRootBranch"
  Execute {} -> "Execute"
  HQNameQuery {} -> "HQNameQuery"
  GetDefinitionsBySuffixes {} -> "GetDefinitionsBySuffixes"
  CmdUnliftIO {} -> "UnliftIO"
