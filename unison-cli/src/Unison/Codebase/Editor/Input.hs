module Unison.Codebase.Editor.Input
  ( Input (..),
    BranchSourceI (..),
    DiffBranchArg (..),
    PullSourceTarget (..),
    PushRemoteBranchInput (..),
    PushSourceTarget (..),
    PushSource (..),
    TestInput (..),
    Event (..),
    OutputLocation (..),
    RelativeToFold (..),
    BranchIdG (..),
    BranchId,
    BranchId2,
    AbsBranchId,
    UnresolvedProjectBranch,
    parseBranchId,
    parseBranchId2,
    parseShortCausalHash,
    Insistence (..),
    PullMode (..),
    FindScope (..),
    ShowDefinitionScope (..),
    IsGlobal,
    DeleteTarget (..),

    -- * Type aliases
    ErrorMessageOrName,
    RawQuery,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.These (These)
import U.Codebase.Config (ConfigKey)
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace)
import Unison.Codebase.Path (Path, Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.ProjectPath (ProjectPath)
import Unison.Codebase.PushBehavior (PushBehavior)
import Unison.Codebase.Runtime.Profile (ProfileSpec)
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.CommandLine.BranchRelativePath (BranchRelativePath, parseBranchRelativePath)
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectAndBranchNames, ProjectBranchName, ProjectBranchNameOrLatestRelease, ProjectName, Semver)
import Unison.Util.Pretty qualified as P

data Event
  = UnisonFileChanged SourceName Source
  deriving stock (Show)

type Source = Text -- "id x = x\nconst a b = a"

type SourceName = Text -- "foo.u" or "buffer 7"

type ErrorMessageOrValue a = Either (P.Pretty P.ColorText) a

type ErrorMessageOrName = ErrorMessageOrValue (HQ.HashQualified Name)

type RawQuery = String

data BranchIdG p
  = BranchAtSCH ShortCausalHash
  | BranchAtPath p
  | BranchAtProjectPath ProjectPath
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance (From p Text) => From (BranchIdG p) Text where
  from = \case
    BranchAtSCH h -> "#" <> SCH.toText h
    BranchAtPath p -> from p
    BranchAtProjectPath pp -> from pp

type BranchId = BranchIdG Path'

type BranchId2 = Either ShortCausalHash BranchRelativePath

type AbsBranchId = BranchIdG Path.Absolute

-- | An unambiguous project branch name, use the current project name if not provided.
type UnresolvedProjectBranch = ProjectAndBranch (Maybe ProjectName) ProjectBranchName

-- | Should we force the operation or not?
data Insistence = Force | Try
  deriving (Show, Eq)

parseBranchId :: String -> Either Text BranchId
parseBranchId ('#' : s) = case SCH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> pure $ BranchAtSCH h
parseBranchId s = BranchAtPath <$> Path.parsePath' s

parseBranchId2 :: String -> Either (P.Pretty P.ColorText) (Either ShortCausalHash BranchRelativePath)
parseBranchId2 ('#' : s) = case SCH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> Right (Left h)
parseBranchId2 s = Right <$> parseBranchRelativePath s

parseShortCausalHash :: String -> Either String ShortCausalHash
parseShortCausalHash ('#' : s) | Just sch <- SCH.fromText (Text.pack s) = Right sch
parseShortCausalHash _ = Left "Invalid hash, expected a base32hex string."

data PullMode
  = PullWithHistory
  | PullWithoutHistory
  deriving (Eq, Show)

type IsGlobal = Bool

data Input
  = AliasManyI [HQ'.HashQualified (Path.Split Path)] Path'
  | AliasTermI !Bool (HQ'.HashOrHQ (Path.Split Path')) (Path.Split Path') -- bool = force?
  | AliasTypeI !Bool (HQ'.HashOrHQ (Path.Split Path')) (Path.Split Path') -- bool = force?
  | ApiI
  | AuthLoginI
  | BranchI BranchSourceI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | BranchRenameI ProjectBranchName
  | BranchSquashI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName) (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | BranchesI (Maybe ProjectName)
  | CancelI
  | ClearI
  | CloneI ProjectAndBranchNames (Maybe ProjectAndBranchNames)
  | ConfigSetI ConfigKey Text
  | ConfigGetI ConfigKey
  | CreateAuthorI NameSegment {- identifier -} Text {- name -}
  | CreateMessage (P.Pretty P.ColorText)
  | DebugClearWatchI
  | DebugDependentsGraph
  | DebugDoctorI
  | DebugDumpNamespaceSimpleI
  | DebugDumpNamespacesI
  | DebugFormatI
  | DebugFuzzyOptionsI String [String] -- cmd and arguments
  | DebugLSPFoldRangesI
  | DebugLSPNameCompletionI Text -- The raw arguments provided
  | DebugNameDiffI ShortCausalHash ShortCausalHash
  | DebugNumberedArgsI
  | DebugSynhashTermI !Name
  | DebugTabCompletionI [String] -- The raw arguments provided
  | DebugTermI (Bool {- Verbose mode -}) (HQ.HashQualified Name)
  | DebugTypeI (HQ.HashQualified Name)
  | DebugTypecheckedUnisonFileI
  | DeleteBranchI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | DeleteI !Bool {- force? -} !DeleteTarget ![HQ'.HashQualified Name]
  | DeleteNamespaceI Insistence (Maybe (Path.Split Path.Relative))
  | DeleteProjectI ProjectName
  | DiffBranchI !DiffBranchArg !DiffBranchArg
  | DiffNamespaceI BranchId2 BranchId2 -- old new
  | DisplayI OutputLocation (NonEmpty (HQ.HashQualified Name))
  | DocToMarkdownI Name
  | DocsI (NonEmpty Name)
  | DocsToHtmlI BranchRelativePath FilePath
  | EditDependentsI !(HQ.HashQualified Name)
  | EditNamespaceI [Path.Path']
  | ExecuteI ProfileSpec (HQ.HashQualified Name) [String]
  | FindI Bool FindScope [String] -- isVerbose, findScope, query
  | FindShallowI Path'
  | ForkLocalBranchI (Either ShortCausalHash BranchRelativePath) BranchRelativePath
  | HistoryI (Maybe Int {- cap on number of results -}) (Maybe Int {- cap on diff elements shown -}) BranchId
  | -- An optional causal hash or branch to annotate.
    HistoryCommentI (Maybe BranchId2 {- causal to annotate -})
  | IOTestAllI
  | IOTestI (HQ.HashQualified Name)
  | LibInstallI
      !Bool -- Remind the user to use `lib.install` next time, not `pull`?
      !(ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease))
  | LibInstallLocalI
      -- The source local project and branch.
      !(ProjectAndBranch ProjectName ProjectBranchName)
      -- The destination lib name
      (Maybe NameSegment)
  | ListDependenciesI (HQ.HashQualified Name)
  | ListDependentsI (HQ.HashQualified Name)
  | LoadI (Maybe FilePath)
  | MakeStandaloneI String (HQ.HashQualified Name)
  | MergeBuiltinsI (Maybe Path.Relative)
  | MergeCommitI
  | MergeI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | MergeIOBuiltinsI (Maybe Path.Relative)
  | MoveAllI Path.Path' Path.Path'
  | MoveBranchI Path.Path' Path.Path'
  | MoveTermI (HQ'.HashQualified (Path.Split Path')) (Path.Split Path')
  | -- | Move sources into destination namespace
    MoveToI (NonEmpty Path.Path') Path.Path'
  | MoveTypeI (HQ'.HashQualified (Path.Split Path')) (Path.Split Path')
  | NamesI IsGlobal [(RawQuery, ErrorMessageOrName)]
  | -- | Rename (change final segment) of term, type, or namespace
    RenameI Path.Path' NameSegment
  | NamespaceDependenciesI (Maybe Path')
  | PopBranchI
  | ProjectCreateI Bool {- try downloading base? -} (Maybe ProjectName)
  | ProjectRenameI ProjectName
  | ProjectSwitchI ProjectAndBranchNames
  | ProjectsI
  | PullI !PullSourceTarget !PullMode
  | PushRemoteBranchI PushRemoteBranchInput
  | QuitI
  | ReleaseDraftI Semver
  | ResetI BranchId2 {- namespace to reset it to -} (Maybe UnresolvedProjectBranch {- ProjectBranch to reset -})
  | SaveExecuteResultI Name
  | ShowDefinitionI OutputLocation ShowDefinitionScope (NonEmpty (HQ.HashQualified Name))
  | ShowGlobalReflogI
  | ShowProjectBranchReflogI (Maybe (ProjectAndBranch (Maybe ProjectName) ProjectBranchName))
  | ShowProjectReflogI (Maybe ProjectName)
  | ShowRootReflogI {- Deprecated -}
  | StructuredFindI FindScope (HQ.HashQualified Name) -- sfind findScope query
  | StructuredFindReplaceI (HQ.HashQualified Name) -- sfind.replace rewriteQuery
  | SwitchBranchI Path'
  | -- | Sync from a codebase project branch to this codebase's project branch
    SyncFromCodebaseI FilePath (ProjectAndBranch ProjectName ProjectBranchName) UnresolvedProjectBranch
  | SyncFromFileI FilePath UnresolvedProjectBranch
  | SyncToFileI FilePath (ProjectAndBranch (Maybe ProjectName) (Maybe ProjectBranchName))
  | TestI TestInput
  | TextFindI Bool [String] -- TextFindI allowLib tokens
  | TodoI
  | UiI Path'
  | UndoI
  | UpI
  | Update2I
  | DiffUpdateI
  | UpdateBuiltinsI
  | UpgradeCommitI
  | UpgradeI ![NameSegment]
  | VersionI
  | -- | Watch an external file or directory for changes
    WatchI !FilePath
  | -- | Stop watching one or more external files or directories
    UnwatchI ![FilePath]
  | -- | List currently watched external paths
    WatchListI
  deriving (Eq, Show)

-- | The source of a `branch` command: what to make the new branch from.
data BranchSourceI
  = -- | Create a branch from the current context
    BranchSourceI'CurrentContext
  | -- | Create an empty branch
    BranchSourceI'Empty
  | -- | Create a branch from this other branch
    BranchSourceI'UnresolvedProjectBranch UnresolvedProjectBranch
  deriving stock (Eq, Show)

data DiffBranchArg
  = DiffBranchArg'Branch !(ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | DiffBranchArg'Hash !ShortCausalHash
  deriving stock (Eq, Show)

-- | Pull source and target: either neither is specified, or only a source, or both.
data PullSourceTarget
  = PullSourceTarget0
  | PullSourceTarget1 (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease))
  | PullSourceTarget2 (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease)) (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  deriving stock (Eq, Show)

data PushSource
  = ProjySource (These ProjectName ProjectBranchName)
  deriving stock (Eq, Show)

-- | Push source and target: either neither is specified, or only a target, or both.
data PushSourceTarget
  = PushSourceTarget0
  | PushSourceTarget1 (These ProjectName ProjectBranchName)
  | PushSourceTarget2 PushSource (These ProjectName ProjectBranchName)
  deriving stock (Eq, Show)

data PushRemoteBranchInput = PushRemoteBranchInput
  { sourceTarget :: PushSourceTarget,
    pushBehavior :: PushBehavior
  }
  deriving stock (Eq, Show)

data TestInput = TestInput
  { -- | Should we run tests in the `lib` namespace?
    includeLibNamespace :: Bool,
    -- | Relative path to run the tests in. Ignore if `includeLibNamespace` is True - that means test everything.
    path :: Path.Relative,
    showFailures :: Bool,
    showSuccesses :: Bool
  }
  deriving stock (Eq, Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation RelativeToFold
  | FileLocation FilePath RelativeToFold
  -- ClipboardLocation
  deriving (Eq, Show)

-- | Above a new fold, or within the topmost fold?
data RelativeToFold
  = AboveFold
  | WithinFold
  deriving stock (Eq, Show)

data FindScope
  = FindLocal Path'
  | FindLocalAndDeps Path'
  | FindGlobal
  deriving stock (Eq, Show)

data ShowDefinitionScope
  = ShowDefinitionLocal
  | ShowDefinitionGlobal
  deriving stock (Eq, Show)

data DeleteTarget
  = DeleteTarget'TermOrType
  | DeleteTarget'Term
  | DeleteTarget'Type
  deriving stock (Eq, Show)
