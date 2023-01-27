module Unison.Codebase.Editor.Input
  ( Input (..),
    DiffNamespaceToPatchInput (..),
    GistInput (..),
    PushRemoteBranchInput (..),
    TestInput (..),
    Event (..),
    OutputLocation (..),
    PatchPath,
    BranchId,
    AbsBranchId,
    parseBranchId,
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

import qualified Data.Text as Text
import U.Codebase.HashTags (CausalHash)
import qualified Unison.Codebase.Branch.Merge as Branch
import Unison.Codebase.Editor.RemoteRepo
import Unison.Codebase.Path (Path')
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Path.Parse as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import qualified Unison.Codebase.ShortCausalHash as SCH
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Codebase.Verbosity
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName)
import Unison.ShortHash (ShortHash)
import qualified Unison.Util.Pretty as P

data Event
  = UnisonFileChanged SourceName Source
  | IncomingRootBranch (Set CausalHash)

type Source = Text -- "id x = x\nconst a b = a"

type SourceName = Text -- "foo.u" or "buffer 7"

type PatchPath = Path.Split'

data OptionalPatch = NoPatch | DefaultPatch | UsePatch PatchPath
  deriving (Eq, Ord, Show)

type BranchId = Either ShortCausalHash Path'

type AbsBranchId = Either ShortCausalHash Path.Absolute

type HashOrHQSplit' = Either ShortHash Path.HQSplit'

-- | Should we force the operation or not?
data Insistence = Force | Try
  deriving (Show, Eq)

parseBranchId :: String -> Either String BranchId
parseBranchId ('#' : s) = case SCH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> pure $ Left h
parseBranchId s = Right <$> Path.parsePath' s

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
    ForkLocalBranchI (Either ShortCausalHash Path') Path'
  | -- merge first causal into destination
    MergeLocalBranchI Path' Path' Branch.MergeMode
  | PreviewMergeLocalBranchI Path' Path'
  | DiffNamespaceI BranchId BranchId -- old new
  | PullRemoteBranchI (Maybe ReadRemoteNamespace) Path' SyncMode PullMode Verbosity
  | PushRemoteBranchI PushRemoteBranchInput
  | CreatePullRequestI ReadRemoteNamespace ReadRemoteNamespace
  | LoadPullRequestI ReadRemoteNamespace ReadRemoteNamespace Path'
  | ResetRootI (Either ShortCausalHash Path')
  | -- todo: Q: Does it make sense to publish to not-the-root of a Github repo?
    --          Does it make sense to fork from not-the-root of a Github repo?
    -- used in Welcome module to give directions to user
    CreateMessage (P.Pretty P.ColorText)
  | -- Change directory. If Nothing is provided, prompt an interactive fuzzy search.
    SwitchBranchI (Maybe Path')
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
  | -- Move = Rename; It's an HQSplit' not an HQSplit', meaning the arg has to have a name.
    MoveTermI Path.HQSplit' Path.Split'
  | MoveTypeI Path.HQSplit' Path.Split'
  | MoveBranchI Path.Path' Path.Path'
  | MovePatchI Path.Split' Path.Split'
  | CopyPatchI Path.Split' Path.Split'
  | -- delete = unname
    DeleteI DeleteTarget
  | -- resolving naming conflicts within `branchpath`
    -- Add the specified name after deleting all others for a given reference
    -- within a given branch.
    ResolveTermNameI Path.HQSplit'
  | ResolveTypeNameI Path.HQSplit'
  | -- edits stuff:
    LoadI (Maybe FilePath)
  | ClearI
  | AddI (Set Name)
  | PreviewAddI (Set Name)
  | UpdateI OptionalPatch (Set Name)
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
    ExecuteI String [String]
  | -- save the result of a previous Execute
    SaveExecuteResultI Name
  | -- execute an IO [Result]
    IOTestI (HQ.HashQualified Name)
  | -- make a standalone binary file
    MakeStandaloneI String (HQ.HashQualified Name)
  | -- execute an IO thunk using scheme
    ExecuteSchemeI (HQ.HashQualified Name) [String]
  | -- compile to a scheme file
    CompileSchemeI String (HQ.HashQualified Name)
  | -- generate scheme libraries
    GenSchemeLibsI
  | -- fetch scheme compiler
    FetchSchemeCompilerI
  | TestI TestInput
  | -- metadata
    -- `link metadata definitions` (adds metadata to all of `definitions`)
    LinkI (HQ.HashQualified Name) [Path.HQSplit']
  | -- `unlink metadata definitions` (removes metadata from all of `definitions`)
    UnlinkI (HQ.HashQualified Name) [Path.HQSplit']
  | -- links from <type>
    LinksI Path.HQSplit' (Maybe String)
  | CreateAuthorI NameSegment {- identifier -} Text {- name -}
  | -- Display provided definitions. If list is empty, prompt a fuzzy search.
    DisplayI OutputLocation [HQ.HashQualified Name]
  | -- Display docs for provided terms. If list is empty, prompt a fuzzy search.
    DocsI [Path.HQSplit']
  | -- other
    FindI Bool FindScope [String] -- FindI isVerbose findScope query
  | FindShallowI Path'
  | FindPatchI
  | -- Show provided definitions. If list is empty, prompt a fuzzy search.
    ShowDefinitionI OutputLocation ShowDefinitionScope [HQ.HashQualified Name]
  | ShowDefinitionByPrefixI OutputLocation [HQ.HashQualified Name]
  | ShowReflogI
  | UpdateBuiltinsI
  | MergeBuiltinsI
  | MergeIOBuiltinsI
  | ListDependenciesI (HQ.HashQualified Name)
  | ListDependentsI (HQ.HashQualified Name)
  | -- | List all external dependencies of a given namespace, or the current namespace if
    -- no path is provided.
    NamespaceDependenciesI (Maybe Path')
  | DebugTabCompletionI [String] -- The raw arguments provided
  | DebugNumberedArgsI
  | DebugTypecheckedUnisonFileI
  | DebugDumpNamespacesI
  | DebugDumpNamespaceSimpleI
  | DebugClearWatchI
  | DebugDoctorI
  | DebugNameDiffI ShortCausalHash ShortCausalHash
  | QuitI
  | ApiI
  | UiI
  | DocsToHtmlI Path' FilePath
  | GistI GistInput
  | AuthLoginI
  | VersionI
  | DiffNamespaceToPatchI DiffNamespaceToPatchInput
  | ProjectCreateI ProjectName
  | ProjectCreateBranchI ProjectBranchName
  | ProjectPushI ProjectBranchName
  | -- | The project name and branch name to switch to.
    --
    -- If a local project does not exist with the given name, we will re-parse this name as a Share slug + project name
    -- (e.g. "@arya/lens").
    --
    -- If the branch is unspecified, we will pick a branch somehow. FIXME: this comment is imprecise because we haven't
    -- nailed down the default branch mechanism yet.
    ProjectSwitchI (ProjectAndBranch ProjectName (Maybe ProjectBranchName))
  deriving (Eq, Show)

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

data PushRemoteBranchInput = PushRemoteBranchInput
  { -- | The local path to push. If relative, it's resolved relative to the current path (`cd`).
    localPath :: Path',
    -- | The repo to push to. If missing, it is looked up in `.unisonConfig`.
    maybeRemoteRepo :: Maybe WriteRemotePath,
    -- | The push behavior (whether the remote branch is required to be empty or non-empty).
    pushBehavior :: PushBehavior,
    syncMode :: SyncMode
  }
  deriving stock (Eq, Show)

data TestInput = TestInput
  { -- | Should we run tests in the `lib` namespace?
    includeLibNamespace :: Bool,
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
  = FindLocal
  | FindLocalAndDeps
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
  = DeleteTarget'TermOrType DeleteOutput Path.HQSplit'
  | DeleteTarget'Term DeleteOutput Path.HQSplit'
  | DeleteTarget'Type DeleteOutput Path.HQSplit'
  | DeleteTarget'Branch Insistence (Maybe Path.Split')
  | DeleteTarget'Patch Path.Split'
  deriving stock (Eq, Show)
