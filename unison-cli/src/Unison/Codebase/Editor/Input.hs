module Unison.Codebase.Editor.Input
  ( Input (..),
    BranchSourceI (..),
    DiffNamespaceToPatchInput (..),
    GistInput (..),
    PullSourceTarget (..),
    PushRemoteBranchInput (..),
    PushSourceTarget (..),
    PushSource (..),
    TestInput (..),
    Event (..),
    OutputLocation (..),
    PatchPath,
    BranchId,
    AbsBranchId,
    LooseCodeOrProject,
    parseBranchId,
    parseBranchId2,
    parseShortCausalHash,
    HashOrHQSplit',
    Insistence (..),
    PullMode (..),
    OptionalPatch (..),
    FindScope (..),
    ShowDefinitionScope (..),
    IsGlobal,
    DeleteOutput (..),
    DeleteTarget (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import Data.These (These)
import Unison.Codebase.Branch.Merge qualified as Branch
import Unison.Codebase.Editor.RemoteRepo (ReadRemoteNamespace, WriteGitRepo, WriteRemoteNamespace)
import Unison.Codebase.Path (Path, Path')
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Path.Parse qualified as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Codebase.ShortCausalHash qualified as SCH
import Unison.CommandLine.BranchRelativePath (BranchRelativePath, parseBranchRelativePath)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectAndBranchNames, ProjectBranchName, ProjectBranchNameOrLatestRelease, ProjectName, Semver)
import Unison.ShortHash (ShortHash)
import Unison.Util.Pretty qualified as P

data Event
  = UnisonFileChanged SourceName Source
  deriving stock (Show)

type Source = Text -- "id x = x\nconst a b = a"

type SourceName = Text -- "foo.u" or "buffer 7"

type PatchPath = Path.Split'

data OptionalPatch = NoPatch | DefaultPatch | UsePatch PatchPath
  deriving (Eq, Ord, Show)

type BranchId = Either ShortCausalHash Path'

-- | A lot of commands can take either a loose code path or a project branch in the same argument slot. Usually, those
-- have distinct syntaxes, but sometimes it's ambiguous, in which case we'd parse a `These`. The command itself can
-- decide what to do with the ambiguity.
type LooseCodeOrProject =
  These Path' (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)

type AbsBranchId = Either ShortCausalHash Path.Absolute

type HashOrHQSplit' = Either ShortHash Path.HQSplit'

-- | Should we force the operation or not?
data Insistence = Force | Try
  deriving (Show, Eq)

parseBranchId :: String -> Either Text BranchId
parseBranchId ('#' : s) = case SCH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> pure $ Left h
parseBranchId s = Right <$> Path.parsePath' s

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
  = -- names stuff:
    -- directory ops
    -- `Link` must describe a repo and a source path within that repo.
    -- clone w/o merge, error if would clobber
    ForkLocalBranchI (Either ShortCausalHash BranchRelativePath) BranchRelativePath
  | -- merge first causal into destination
    MergeLocalBranchI LooseCodeOrProject LooseCodeOrProject Branch.MergeMode
  | PreviewMergeLocalBranchI LooseCodeOrProject LooseCodeOrProject
  | DiffNamespaceI BranchId BranchId -- old new
  | PullI !PullSourceTarget !PullMode
  | PushRemoteBranchI PushRemoteBranchInput
  | ResetRootI (Either ShortCausalHash Path')
  | ResetI
      ( These
          (Either ShortCausalHash Path')
          (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
      )
      (Maybe LooseCodeOrProject)
  | -- todo: Q: Does it make sense to publish to not-the-root of a Github repo?
    --          Does it make sense to fork from not-the-root of a Github repo?
    -- used in Welcome module to give directions to user
    CreateMessage (P.Pretty P.ColorText)
  | -- Change directory.
    SwitchBranchI Path'
  | UpI
  | PopBranchI
  | -- > names foo
    -- > names foo.bar
    -- > names .foo.bar
    -- > names .foo.bar#asdflkjsdf
    -- > names #sdflkjsdfhsdf
    NamesI IsGlobal (HQ.HashQualified Name)
  | AliasTermI HashOrHQSplit' Path.Split'
  | AliasTypeI HashOrHQSplit' Path.Split'
  | AliasManyI [Path.HQSplit] Path'
  | MoveAllI Path.Path' Path.Path'
  | -- Move = Rename; It's an HQSplit' not an HQSplit', meaning the arg has to have a name.
    MoveTermI Path.HQSplit' Path.Split'
  | MoveTypeI Path.HQSplit' Path.Split'
  | MoveBranchI Path.Path' Path.Path'
  | MovePatchI Path.Split' Path.Split'
  | CopyPatchI Path.Split' Path.Split'
  | -- delete = unname
    DeleteI DeleteTarget
  | -- edits stuff:
    LoadI (Maybe FilePath)
  | ClearI
  | AddI (Set Name)
  | PreviewAddI (Set Name)
  | UpdateI OptionalPatch (Set Name)
  | Update2I
  | PreviewUpdateI (Set Name)
  | TodoI (Maybe PatchPath) Path'
  | PropagatePatchI PatchPath Path'
  | ListEditsI (Maybe PatchPath)
  | -- -- create and remove update directives
    DeprecateTermI PatchPath Path.HQSplit'
  | DeprecateTypeI PatchPath Path.HQSplit'
  | ReplaceI (HQ.HashQualified Name) (HQ.HashQualified Name) (Maybe PatchPath)
  | RemoveTermReplacementI (HQ.HashQualified Name) (Maybe PatchPath)
  | RemoveTypeReplacementI (HQ.HashQualified Name) (Maybe PatchPath)
  | UndoI
  | -- First `Maybe Int` is cap on number of results, if any
    -- Second `Maybe Int` is cap on diff elements shown, if any
    HistoryI (Maybe Int) (Maybe Int) BranchId
  | -- execute an IO thunk with args
    ExecuteI Text [String]
  | -- save the result of a previous Execute
    SaveExecuteResultI Name
  | -- execute an IO [Result]
    IOTestI (HQ.HashQualified Name)
  | -- execute all in-scope IO tests
    IOTestAllI
  | -- make a standalone binary file
    MakeStandaloneI String (HQ.HashQualified Name)
  | -- execute an IO thunk using scheme
    ExecuteSchemeI Text [String]
  | -- compile to a scheme file
    CompileSchemeI Text (HQ.HashQualified Name)
  | TestI TestInput
  | CreateAuthorI NameSegment {- identifier -} Text {- name -}
  | -- Display provided definitions.
    DisplayI OutputLocation (NonEmpty (HQ.HashQualified Name))
  | -- Display docs for provided terms.
    DocsI (NonEmpty Path.HQSplit')
  | -- other
    FindI Bool FindScope [String] -- FindI isVerbose findScope query
  | FindShallowI Path'
  | FindPatchI
  | StructuredFindI FindScope (HQ.HashQualified Name) -- sfind findScope query
  | StructuredFindReplaceI (HQ.HashQualified Name) -- sfind.replace rewriteQuery
  | -- Show provided definitions.
    ShowDefinitionI OutputLocation ShowDefinitionScope (NonEmpty (HQ.HashQualified Name))
  | ShowReflogI
  | UpdateBuiltinsI
  | MergeBuiltinsI (Maybe Path)
  | MergeIOBuiltinsI (Maybe Path)
  | ListDependenciesI (HQ.HashQualified Name)
  | ListDependentsI (HQ.HashQualified Name)
  | -- | List all external dependencies of a given namespace, or the current namespace if
    -- no path is provided.
    NamespaceDependenciesI (Maybe Path')
  | DebugTabCompletionI [String] -- The raw arguments provided
  | DebugFuzzyOptionsI String [String] -- cmd and arguments
  | DebugFormatI
  | DebugNumberedArgsI
  | DebugTypecheckedUnisonFileI
  | DebugDumpNamespacesI
  | DebugDumpNamespaceSimpleI
  | DebugTermI (Bool {- Verbose mode -}) (HQ.HashQualified Name)
  | DebugTypeI (HQ.HashQualified Name)
  | DebugLSPFoldRangesI
  | DebugClearWatchI
  | DebugDoctorI
  | DebugNameDiffI ShortCausalHash ShortCausalHash
  | QuitI
  | ApiI
  | UiI Path'
  | DocToMarkdownI Name
  | DocsToHtmlI Path' FilePath
  | GistI GistInput
  | AuthLoginI
  | VersionI
  | DiffNamespaceToPatchI DiffNamespaceToPatchInput
  | ProjectCreateI Bool {- try downloading base? -} (Maybe ProjectName)
  | ProjectRenameI ProjectName
  | ProjectSwitchI ProjectAndBranchNames
  | ProjectsI
  | BranchI BranchSourceI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | BranchRenameI ProjectBranchName
  | BranchesI (Maybe ProjectName)
  | CloneI ProjectAndBranchNames (Maybe ProjectAndBranchNames)
  | ReleaseDraftI Semver
  | UpgradeI !NameSegment !NameSegment
  | EditNamespaceI [Path.Path]
  | -- New merge algorithm: merge the given project branch into the current one.
    MergeI (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | LibInstallI !(ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease))
  deriving (Eq, Show)

-- | The source of a `branch` command: what to make the new branch from.
data BranchSourceI
  = -- | Create a branch from the current context
    BranchSourceI'CurrentContext
  | -- | Create an empty branch
    BranchSourceI'Empty
  | -- | Create a branch from this loose-code-or-project
    BranchSourceI'LooseCodeOrProject LooseCodeOrProject
  deriving stock (Eq, Show)

data DiffNamespaceToPatchInput = DiffNamespaceToPatchInput
  { -- The first/earlier namespace.
    branchId1 :: BranchId,
    -- The second/later namespace.
    branchId2 :: BranchId,
    -- Where to store the patch that corresponds to the diff between the namespaces.
    patch :: Path.Split'
  }
  deriving stock (Eq, Generic, Show)

-- | @"push.gist repo"@ pushes the contents of the current namespace to @repo@.
data GistInput = GistInput
  { repo :: WriteGitRepo
  }
  deriving stock (Eq, Show)

-- | Pull source and target: either neither is specified, or only a source, or both.
data PullSourceTarget
  = PullSourceTarget0
  | PullSourceTarget1 (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease))
  | PullSourceTarget2 (ReadRemoteNamespace (These ProjectName ProjectBranchNameOrLatestRelease)) (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  deriving stock (Eq, Show)

data PushSource
  = PathySource Path'
  | ProjySource (These ProjectName ProjectBranchName)
  deriving stock (Eq, Show)

-- | Push source and target: either neither is specified, or only a target, or both.
data PushSourceTarget
  = PushSourceTarget0
  | PushSourceTarget1 (WriteRemoteNamespace (These ProjectName ProjectBranchName))
  | PushSourceTarget2 PushSource (WriteRemoteNamespace (These ProjectName ProjectBranchName))
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
    path :: Path,
    showFailures :: Bool,
    showSuccesses :: Bool
  }
  deriving stock (Eq, Show)

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation
  | FileLocation FilePath
  -- ClipboardLocation
  deriving (Eq, Show)

data FindScope
  = FindLocal Path
  | FindLocalAndDeps Path
  | FindGlobal
  deriving stock (Eq, Show)

data ShowDefinitionScope
  = ShowDefinitionLocal
  | ShowDefinitionGlobal
  deriving stock (Eq, Show)

data DeleteOutput
  = DeleteOutput'Diff
  | DeleteOutput'NoDiff
  deriving stock (Eq, Show)

data DeleteTarget
  = DeleteTarget'TermOrType DeleteOutput [Path.HQSplit']
  | DeleteTarget'Term DeleteOutput [Path.HQSplit']
  | DeleteTarget'Type DeleteOutput [Path.HQSplit']
  | DeleteTarget'Namespace Insistence (Maybe Path.Split')
  | DeleteTarget'Patch Path.Split'
  | DeleteTarget'ProjectBranch (ProjectAndBranch (Maybe ProjectName) ProjectBranchName)
  | DeleteTarget'Project ProjectName
  deriving stock (Eq, Show)
