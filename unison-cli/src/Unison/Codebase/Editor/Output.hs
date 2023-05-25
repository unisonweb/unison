module Unison.Codebase.Editor.Output
  ( Output (..),
    CreatedProjectBranchFrom (..),
    DisplayDefinitionsOutput (..),
    WhichBranchEmpty (..),
    NumberedOutput (..),
    NumberedArgs,
    ListDetailed,
    HistoryTail (..),
    TestReportStats (..),
    UndoFailureReason (..),
    ShareError (..),
    isFailure,
    isNumberedFailure,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import Data.Time (UTCTime)
import Network.URI (URI)
import qualified Servant.Client as Servant (ClientError)
import qualified System.Console.Haskeline as Completion
import U.Codebase.Branch.Diff (NameChanges)
import U.Codebase.HashTags (CausalHash)
import qualified U.Codebase.Sqlite.Project as Sqlite
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import Unison.Auth.Types (CredentialFailure)
import qualified Unison.Cli.Share.Projects.Types as Share
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.Output.BranchDiff (BranchDiffOutput)
import Unison.Codebase.Editor.Output.PushPull (PushPull)
import Unison.Codebase.Editor.RemoteRepo
import Unison.Codebase.Editor.SlurpResult (SlurpResult (..))
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.Codebase.Editor.TodoOutput as TO
import Unison.Codebase.IntegrityCheck (IntegrityResult (..))
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path')
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.PushBehavior (PushBehavior)
import qualified Unison.Codebase.Runtime as Runtime
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import qualified Unison.Codebase.ShortCausalHash as SCH
import Unison.Codebase.Type (GitError)
import qualified Unison.CommandLine.InputPattern as Input
import Unison.DataDeclaration (Decl)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified2 as HQ'
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.Names (Names)
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.NamesWithHistory as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import Unison.Project (ProjectAndBranch, ProjectBranchName, ProjectName, Semver)
import Unison.Reference (Reference, TermReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Server.Backend (ShallowListEntry (..))
import Unison.Server.SearchResult2 (SearchResult')
import qualified Unison.Share.Sync.Types as Sync
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
import qualified Unison.Sync.Types as Share (DownloadEntitiesError, UploadEntitiesError)
import qualified Unison.Syntax.Parser as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import Unison.Util.Relation (Relation)
import qualified Unison.WatchKind as WK

type ListDetailed = Bool

type SourceName = Text

type NumberedArgs = [String]

type HashLength = Int

data NumberedOutput
  = ShowDiffNamespace AbsBranchId AbsBranchId PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterUndo PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterDeleteDefinitions PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterDeleteBranch Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterModifyBranch Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterMerge (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName)) Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterMergePropagate (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName)) Path.Absolute Path.Path' PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterMergePreview (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName)) Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterPull Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | -- <authorIdentifier> <authorPath> <relativeBase>
    ShowDiffAfterCreateAuthor NameSegment Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | -- | Invariant: there's at least one conflict or edit in the TodoOutput.
    TodoOutput PPE.PrettyPrintEnvDecl (TO.TodoOutput Symbol Ann)
  | -- | CantDeleteDefinitions ppe couldntDelete becauseTheseStillReferenceThem
    CantDeleteDefinitions PPE.PrettyPrintEnvDecl (Map LabeledDependency (NESet LabeledDependency))
  | -- | CantDeleteNamespace ppe couldntDelete becauseTheseStillReferenceThem
    CantDeleteNamespace PPE.PrettyPrintEnvDecl (Map LabeledDependency (NESet LabeledDependency))
  | -- | DeletedDespiteDependents ppe deletedThings thingsWhichNowHaveUnnamedReferences
    DeletedDespiteDependents PPE.PrettyPrintEnvDecl (Map LabeledDependency (NESet LabeledDependency))
  | -- |    size limit, history                       , how the history ends
    History
      (Maybe Int) -- Amount of history to print
      HashLength
      [(CausalHash, Names.Diff)]
      HistoryTail -- 'origin point' of this view of history.
  | ListEdits Patch PPE.PrettyPrintEnv
  | ListProjects [Sqlite.Project]
  | ListBranches ProjectName [(ProjectBranchName, [(URI, ProjectName, ProjectBranchName)])]
  | AmbiguousSwitch ProjectName (ProjectAndBranch ProjectName ProjectBranchName)

--  | ShowDiff

data Output
  = -- Generic Success response; we might consider deleting this.
    Success
  | -- User did `add` or `update` before typechecking a file?
    NoUnisonFile
  | -- Used in Welcome module to instruct user
    PrintMessage (P.Pretty P.ColorText)
  | InvalidSourceName String
  | SourceLoadFailed String
  | -- No main function, the [Type v Ann] are the allowed types
    NoMainFunction String PPE.PrettyPrintEnv [Type Symbol Ann]
  | -- | Function found, but has improper type
    -- Note: the constructor name is misleading here; we weren't necessarily looking for a "main".
    BadMainFunction
      String
      -- ^ what we were trying to do (e.g. "run", "io.test")
      String
      -- ^ name of function
      (Type Symbol Ann)
      -- ^ bad type of function
      PPE.PrettyPrintEnv
      [Type Symbol Ann]
      -- ^ acceptable type(s) of function
  | BranchEmpty WhichBranchEmpty
  | LoadPullRequest (ReadRemoteNamespace Void) (ReadRemoteNamespace Void) Path' Path' Path' Path'
  | CreatedNewBranch Path.Absolute
  | BranchAlreadyExists Path'
  | FindNoLocalMatches
  | PatchAlreadyExists Path.Split'
  | NoExactTypeMatches
  | TypeAlreadyExists Path.Split' (Set Reference)
  | TypeParseError String (Parser.Err Symbol)
  | ParseResolutionFailures String [Names.ResolutionFailure Symbol Ann]
  | TypeHasFreeVars (Type Symbol Ann)
  | TermAlreadyExists Path.Split' (Set Referent)
  | LabeledReferenceAmbiguous Int (HQ.HashQualified Name) (Set LabeledDependency)
  | LabeledReferenceNotFound (HQ.HashQualified Name)
  | DeleteNameAmbiguous Int Path.HQSplit' (Set Referent) (Set Reference)
  | TermAmbiguous (HQ.HashQualified Name) (Set Referent)
  | HashAmbiguous ShortHash (Set Referent)
  | BranchHashAmbiguous ShortCausalHash (Set ShortCausalHash)
  | BadNamespace String String
  | BranchNotFound Path'
  | EmptyLooseCodePush Path'
  | EmptyProjectBranchPush (ProjectAndBranch ProjectName ProjectBranchName)
  | NameNotFound Path.HQSplit'
  | NamesNotFound [Name]
  | PatchNotFound Path.Split'
  | TypeNotFound Path.HQSplit'
  | TermNotFound Path.HQSplit'
  | TypeNotFound' ShortHash
  | TermNotFound' ShortHash
  | TypeTermMismatch (HQ.HashQualified Name) (HQ.HashQualified Name)
  | NoLastRunResult
  | SaveTermNameConflict Name
  | SearchTermsNotFound [HQ.HashQualified Name]
  | -- Like 'SearchTermsNotFound' but additionally contains term hits
    -- if we are searching for types or type hits if we are searching
    -- for terms. This additional info is used to provide an enhanced
    -- error message.
    SearchTermsNotFoundDetailed
      Bool
      -- ^ @True@ if we are searching for a term, @False@ if we are searching for a type
      [HQ.HashQualified Name]
      -- ^ Misses (search terms that returned no hits for terms or types)
      [HQ.HashQualified Name]
      -- ^ Hits for types if we are searching for terms or terms if we are searching for types
  | -- ask confirmation before deleting the last branch that contains some defns
    -- `Path` is one of the paths the user has requested to delete, and is paired
    -- with whatever named definitions would not have any remaining names if
    -- the path is deleted.
    DeleteBranchConfirmation
      [(Path', (Names, [SearchResult' Symbol Ann]))]
  | DeleteEverythingConfirmation
  | MoveRootBranchConfirmation
  | MovedOverExistingBranch Path'
  | DeletedEverything
  | ListNames
      IsGlobal
      Int -- hq length to print References
      [(Reference, [HQ'.HashQualified Name])] -- type match, type names
      [(Referent, [HQ'.HashQualified Name])] -- term match, term names
      -- list of all the definitions within this branch
  | ListOfDefinitions FindScope PPE.PrettyPrintEnv ListDetailed [SearchResult' Symbol Ann]
  | ListOfLinks PPE.PrettyPrintEnv [(HQ.HashQualified Name, Reference, Maybe (Type Symbol Ann))]
  | ListShallow (IO PPE.PrettyPrintEnv) [ShallowListEntry Symbol Ann]
  | ListOfPatches (Set Name)
  | -- show the result of add/update
    SlurpOutput Input PPE.PrettyPrintEnv SlurpResult
  | -- Original source, followed by the errors:
    ParseErrors Text [Parser.Err Symbol]
  | TypeErrors Path.Absolute Text PPE.PrettyPrintEnv [Context.ErrorNote Symbol Ann]
  | CompilerBugs Text PPE.PrettyPrintEnv [Context.CompilerBug Symbol Ann]
  | DisplayConflicts (Relation Name Referent) (Relation Name Reference)
  | EvaluationFailure Runtime.Error
  | Evaluated
      SourceFileContents
      PPE.PrettyPrintEnv
      [(Symbol, Term Symbol ())]
      (Map Symbol (Ann, WK.WatchKind, Term Symbol (), Runtime.IsCacheHit))
  | RunResult PPE.PrettyPrintEnv (Term Symbol ())
  | Typechecked SourceName PPE.PrettyPrintEnv SlurpResult (UF.TypecheckedUnisonFile Symbol Ann)
  | DisplayRendered (Maybe FilePath) (P.Pretty P.ColorText)
  | -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
    DisplayDefinitions DisplayDefinitionsOutput
  | TestIncrementalOutputStart PPE.PrettyPrintEnv (Int, Int) Reference (Term Symbol Ann)
  | TestIncrementalOutputEnd PPE.PrettyPrintEnv (Int, Int) Reference (Term Symbol Ann)
  | TestResults
      TestReportStats
      PPE.PrettyPrintEnv
      ShowSuccesses
      ShowFailures
      [(Reference, Text)] -- oks
      [(Reference, Text)] -- fails
  | CantUndo UndoFailureReason
  | -- new/unrepresented references followed by old/removed
    -- todo: eventually replace these sets with [SearchResult' v Ann]
    -- and a nicer render.
    BustedBuiltins (Set Reference) (Set Reference)
  | GitError GitError
  | ShareError ShareError
  | ViewOnShare (Either WriteShareRemoteNamespace (URI, ProjectName, ProjectBranchName))
  | ConfiguredMetadataParseError Path' String (P.Pretty P.ColorText)
  | NoConfiguredRemoteMapping PushPull Path.Absolute
  | ConfiguredRemoteMappingParseError PushPull Path.Absolute Text String
  | MetadataMissingType PPE.PrettyPrintEnv Referent
  | TermMissingType Reference
  | MetadataAmbiguous (HQ.HashQualified Name) PPE.PrettyPrintEnv [Referent]
  | AboutToPropagatePatch
  | -- todo: tell the user to run `todo` on the same patch they just used
    NothingToPatch PatchPath Path'
  | PatchNeedsToBeConflictFree
  | PatchInvolvesExternalDependents PPE.PrettyPrintEnv (Set Reference)
  | WarnIncomingRootBranch ShortCausalHash (Set ShortCausalHash)
  | StartOfCurrentPathHistory
  | ShowReflog [(Maybe UTCTime, SCH.ShortCausalHash, Text)]
  | PullAlreadyUpToDate
      (ReadRemoteNamespace Share.RemoteProjectBranch)
      (PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
  | PullSuccessful
      (ReadRemoteNamespace Share.RemoteProjectBranch)
      (PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
  | AboutToMerge
  | -- | Indicates a trivial merge where the destination was empty and was just replaced.
    MergeOverEmpty (PullTarget (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch))
  | MergeAlreadyUpToDate
      (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName))
      (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName))
  | PreviewMergeAlreadyUpToDate
      (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName))
      (Either Path.Path' (ProjectAndBranch ProjectName ProjectBranchName))
  | -- | No conflicts or edits remain for the current patch.
    NoConflictsOrEdits
  | NotImplemented
  | NoBranchWithHash ShortCausalHash
  | ListDependencies PPE.PrettyPrintEnv (Set LabeledDependency) [HQ.HashQualified Name] [HQ.HashQualified Name] -- types, terms
  | -- | List dependents of a type or term.
    ListDependents PPE.PrettyPrintEnv (Set LabeledDependency) [HQ.HashQualified Name] [HQ.HashQualified Name] -- types, terms
  | -- | List all direct dependencies which don't have any names in the current branch
    ListNamespaceDependencies
      PPE.PrettyPrintEnv -- PPE containing names for everything from the root namespace.
      Path.Absolute -- The namespace we're checking dependencies for.
      (Map LabeledDependency (Set Name)) -- Mapping of external dependencies to their local dependents.
  | DumpNumberedArgs NumberedArgs
  | DumpBitBooster CausalHash (Map CausalHash [CausalHash])
  | DumpUnisonFileHashes Int [(Name, Reference.Id)] [(Name, Reference.Id)] [(Name, Reference.Id)]
  | BadName String
  | DefaultMetadataNotification
  | CouldntLoadBranch CausalHash
  | HelpMessage Input.InputPattern
  | NamespaceEmpty (NonEmpty AbsBranchId)
  | NoOp
  | -- Refused to push, either because a `push` targeted an empty namespace, or a `push.create` targeted a non-empty namespace.
    RefusedToPush PushBehavior (WriteRemoteNamespace Void)
  | -- | @GistCreated repo@ means a causal was just published to @repo@.
    GistCreated (ReadRemoteNamespace Void)
  | -- | Directs the user to URI to begin an authorization flow.
    InitiateAuthFlow URI
  | UnknownCodeServer Text
  | CredentialFailureMsg CredentialFailure
  | PrintVersion Text
  | IntegrityCheck IntegrityResult
  | DisplayDebugNameDiff NameChanges
  | DisplayDebugCompletions [Completion.Completion]
  | ClearScreen
  | PulledEmptyBranch (ReadRemoteNamespace Share.RemoteProjectBranch)
  | CreatedProject ProjectName ProjectBranchName
  | CreatedProjectBranch CreatedProjectBranchFrom (ProjectAndBranch ProjectName ProjectBranchName)
  | CreatedRemoteProject URI (ProjectAndBranch ProjectName ProjectBranchName)
  | CreatedRemoteProjectBranch URI (ProjectAndBranch ProjectName ProjectBranchName)
  | -- We didn't push anything because the remote server is already in the state we want it to be
    RemoteProjectBranchIsUpToDate URI (ProjectAndBranch ProjectName ProjectBranchName)
  | InvalidProjectName Text
  | InvalidProjectBranchName Text
  | ProjectNameAlreadyExists ProjectName
  | ProjectNameRequiresUserSlug ProjectName -- invariant: this project name doesn't have a user slug :)
  | ProjectAndBranchNameAlreadyExists (ProjectAndBranch ProjectName ProjectBranchName)
  | -- ran a command that only makes sense if on a project branch
    NotOnProjectBranch
  | -- there's no remote project associated with branch, nor any of its parent branches
    NoAssociatedRemoteProject URI (ProjectAndBranch ProjectName ProjectBranchName)
  | -- there's no remote branch associated with branch
    NoAssociatedRemoteProjectBranch URI (ProjectAndBranch ProjectName ProjectBranchName)
  | LocalProjectDoesntExist ProjectName
  | LocalProjectBranchDoesntExist (ProjectAndBranch ProjectName ProjectBranchName)
  | LocalProjectNorProjectBranchExist ProjectName ProjectBranchName
  | RemoteProjectDoesntExist URI ProjectName
  | RemoteProjectBranchDoesntExist URI (ProjectAndBranch ProjectName ProjectBranchName)
  | RemoteProjectReleaseIsDeprecated URI (ProjectAndBranch ProjectName ProjectBranchName)
  | RemoteProjectPublishedReleaseCannotBeChanged URI (ProjectAndBranch ProjectName ProjectBranchName)
  | -- A remote project branch head wasn't in the expected state
    RemoteProjectBranchHeadMismatch URI (ProjectAndBranch ProjectName ProjectBranchName)
  | Unauthorized Text
  | ServantClientError Servant.ClientError
  | MarkdownOut Text
  | DownloadedEntities Int
  | UploadedEntities Int
  | -- A generic "not implemented" message, for WIP code that's nonetheless been merged into trunk
    NotImplementedYet Text
  | DraftingRelease ProjectBranchName Semver
  | CannotCreateReleaseBranchWithBranchCommand ProjectBranchName Semver
  | CalculatingDiff
  | -- | The `local` in a `clone remote local` is ambiguous
    AmbiguousCloneLocal
      (ProjectAndBranch ProjectName ProjectBranchName)
      -- ^ Treating `local` as a project. We may know the branch name, if it was provided in `remote`.
      (ProjectAndBranch ProjectName ProjectBranchName)
  | -- | The `remote` in a `clone remote local` is ambiguous
    AmbiguousCloneRemote ProjectName (ProjectAndBranch ProjectName ProjectBranchName)
  | ClonedProjectBranch
      (ProjectAndBranch ProjectName ProjectBranchName)
      (ProjectAndBranch ProjectName ProjectBranchName)

-- | What did we create a project branch from?
--
--   * Loose code
--   * Nothingness (we made an empty branch)
--   * Other branch (in another project)
--   * Parent branch (in this project)
data CreatedProjectBranchFrom
  = CreatedProjectBranchFrom'LooseCode Path.Absolute
  | CreatedProjectBranchFrom'Nothingness
  | CreatedProjectBranchFrom'OtherBranch (ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch)
  | CreatedProjectBranchFrom'ParentBranch ProjectBranchName

data DisplayDefinitionsOutput = DisplayDefinitionsOutput
  { isTest :: TermReference -> Bool,
    outputFile :: Maybe FilePath,
    prettyPrintEnv :: PPE.PrettyPrintEnvDecl,
    terms :: Map Reference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)),
    types :: Map Reference (DisplayObject () (Decl Symbol Ann))
  }

-- | A branch was empty. But how do we refer to that branch?
data WhichBranchEmpty
  = WhichBranchEmptyHash ShortCausalHash
  | WhichBranchEmptyPath Path'

data ShareError
  = ShareErrorCheckAndSetPush Sync.CheckAndSetPushError
  | ShareErrorDownloadEntities Share.DownloadEntitiesError
  | ShareErrorFastForwardPush Sync.FastForwardPushError
  | ShareErrorGetCausalHashByPath Sync.GetCausalHashByPathError
  | ShareErrorPull Sync.PullError
  | ShareErrorTransport Sync.CodeserverTransportError
  | ShareErrorUploadEntities Share.UploadEntitiesError

data HistoryTail
  = EndOfLog CausalHash
  | MergeTail CausalHash [CausalHash]
  | PageEnd CausalHash Int -- PageEnd nextHash nextIndex
  deriving (Show)

data TestReportStats
  = CachedTests TotalCount CachedCount
  | NewlyComputed
  deriving (Show)

type TotalCount = Int -- total number of tests

type CachedCount = Int -- number of tests found in the cache

type ShowSuccesses = Bool -- whether to list results or just summarize

type ShowFailures = Bool -- whether to list results or just summarize

data UndoFailureReason = CantUndoPastStart | CantUndoPastMerge deriving (Show)

type SourceFileContents = Text

isFailure :: Output -> Bool
isFailure o = case o of
  AmbiguousCloneLocal {} -> True
  AmbiguousCloneRemote {} -> True
  ClonedProjectBranch {} -> False
  NoLastRunResult {} -> True
  SaveTermNameConflict {} -> True
  RunResult {} -> False
  Success {} -> False
  PrintMessage {} -> False
  CouldntLoadBranch {} -> True
  NoUnisonFile {} -> True
  InvalidSourceName {} -> True
  SourceLoadFailed {} -> True
  NoMainFunction {} -> True
  BadMainFunction {} -> True
  CreatedNewBranch {} -> False
  BranchAlreadyExists {} -> True
  -- we do a global search after finding no local matches, so let's not call this a failure yet
  FindNoLocalMatches {} -> False
  PatchAlreadyExists {} -> True
  NoExactTypeMatches -> True
  BranchEmpty {} -> True
  EmptyLooseCodePush {} -> True
  EmptyProjectBranchPush {} -> True
  TypeAlreadyExists {} -> True
  TypeParseError {} -> True
  ParseResolutionFailures {} -> True
  TypeHasFreeVars {} -> True
  TermAlreadyExists {} -> True
  LabeledReferenceAmbiguous {} -> True
  LabeledReferenceNotFound {} -> True
  DeleteNameAmbiguous {} -> True
  TermAmbiguous {} -> True
  BranchHashAmbiguous {} -> True
  BadName {} -> True
  BadNamespace {} -> True
  BranchNotFound {} -> True
  NameNotFound {} -> True
  NamesNotFound _ -> True
  PatchNotFound {} -> True
  TypeNotFound {} -> True
  TypeNotFound' {} -> True
  TermNotFound {} -> True
  TermNotFound' {} -> True
  TypeTermMismatch {} -> True
  SearchTermsNotFound ts -> not (null ts)
  SearchTermsNotFoundDetailed _ misses otherHits -> not (null misses && null otherHits)
  DeleteBranchConfirmation {} -> False
  DeleteEverythingConfirmation -> False
  MoveRootBranchConfirmation -> False
  MovedOverExistingBranch {} -> False
  DeletedEverything -> False
  ListNames _ _ tys tms -> null tms && null tys
  ListOfLinks _ ds -> null ds
  ListOfDefinitions _ _ _ ds -> null ds
  ListOfPatches s -> Set.null s
  SlurpOutput _ _ sr -> not $ SR.isOk sr
  ParseErrors {} -> True
  TypeErrors {} -> True
  CompilerBugs {} -> True
  DisplayConflicts {} -> False
  EvaluationFailure {} -> True
  Evaluated {} -> False
  Typechecked {} -> False
  DisplayDefinitions DisplayDefinitionsOutput {terms, types} -> null terms && null types
  DisplayRendered {} -> False
  TestIncrementalOutputStart {} -> False
  TestIncrementalOutputEnd {} -> False
  TestResults _ _ _ _ _ fails -> not (null fails)
  CantUndo {} -> True
  GitError {} -> True
  BustedBuiltins {} -> True
  ConfiguredMetadataParseError {} -> True
  NoConfiguredRemoteMapping {} -> True
  ConfiguredRemoteMappingParseError {} -> True
  MetadataMissingType {} -> True
  MetadataAmbiguous {} -> True
  PatchNeedsToBeConflictFree {} -> True
  PatchInvolvesExternalDependents {} -> True
  AboutToPropagatePatch {} -> False
  NothingToPatch {} -> False
  WarnIncomingRootBranch {} -> False
  StartOfCurrentPathHistory -> True
  NotImplemented -> True
  DumpNumberedArgs {} -> False
  DumpBitBooster {} -> False
  NoBranchWithHash {} -> True
  PullAlreadyUpToDate {} -> False
  PullSuccessful {} -> False
  AboutToMerge {} -> False
  MergeOverEmpty {} -> False
  MergeAlreadyUpToDate {} -> False
  PreviewMergeAlreadyUpToDate {} -> False
  NoConflictsOrEdits {} -> False
  ListShallow _ es -> null es
  HashAmbiguous {} -> True
  ShowReflog {} -> False
  LoadPullRequest {} -> False
  DefaultMetadataNotification -> False
  HelpMessage {} -> True
  NoOp -> False
  ListDependencies {} -> False
  ListDependents {} -> False
  ListNamespaceDependencies {} -> False
  TermMissingType {} -> True
  DumpUnisonFileHashes _ x y z -> x == mempty && y == mempty && z == mempty
  NamespaceEmpty {} -> True
  RefusedToPush {} -> True
  GistCreated {} -> False
  InitiateAuthFlow {} -> False
  UnknownCodeServer {} -> True
  CredentialFailureMsg {} -> True
  PrintVersion {} -> False
  IntegrityCheck r ->
    case r of
      NoIntegrityErrors -> False
      IntegrityErrorDetected {} -> True
  ShareError {} -> True
  ViewOnShare {} -> False
  DisplayDebugCompletions {} -> False
  DisplayDebugNameDiff {} -> False
  ClearScreen -> False
  PulledEmptyBranch {} -> False
  CreatedProject {} -> False
  CreatedProjectBranch {} -> False
  CreatedRemoteProject {} -> False
  CreatedRemoteProjectBranch {} -> False
  InvalidProjectName {} -> True
  InvalidProjectBranchName {} -> True
  ProjectNameAlreadyExists {} -> True
  ProjectNameRequiresUserSlug {} -> True
  NotOnProjectBranch {} -> True
  NoAssociatedRemoteProject {} -> True
  NoAssociatedRemoteProjectBranch {} -> True
  ProjectAndBranchNameAlreadyExists {} -> True
  LocalProjectDoesntExist {} -> True
  LocalProjectBranchDoesntExist {} -> True
  LocalProjectNorProjectBranchExist {} -> True
  RemoteProjectDoesntExist {} -> True
  RemoteProjectBranchDoesntExist {} -> True
  RemoteProjectReleaseIsDeprecated {} -> True
  RemoteProjectPublishedReleaseCannotBeChanged {} -> True
  RemoteProjectBranchHeadMismatch {} -> True
  Unauthorized {} -> True
  ServantClientError {} -> True
  MarkdownOut {} -> False
  NotImplementedYet {} -> True
  RemoteProjectBranchIsUpToDate {} -> False
  DownloadedEntities {} -> False
  UploadedEntities {} -> False
  DraftingRelease {} -> False
  CannotCreateReleaseBranchWithBranchCommand {} -> True
  CalculatingDiff {} -> False

isNumberedFailure :: NumberedOutput -> Bool
isNumberedFailure = \case
  AmbiguousSwitch {} -> True
  CantDeleteDefinitions {} -> True
  CantDeleteNamespace {} -> True
  DeletedDespiteDependents {} -> False
  History {} -> False
  ListBranches {} -> False
  ListEdits {} -> False
  ListProjects {} -> False
  ShowDiffAfterCreateAuthor {} -> False
  ShowDiffAfterDeleteBranch {} -> False
  ShowDiffAfterDeleteDefinitions {} -> False
  ShowDiffAfterMerge {} -> False
  ShowDiffAfterMergePreview {} -> False
  ShowDiffAfterMergePropagate {} -> False
  ShowDiffAfterModifyBranch {} -> False
  ShowDiffAfterPull {} -> False
  ShowDiffAfterUndo {} -> False
  ShowDiffNamespace {} -> False
  TodoOutput _ todo -> TO.todoScore todo > 0 || not (TO.noConflicts todo)
