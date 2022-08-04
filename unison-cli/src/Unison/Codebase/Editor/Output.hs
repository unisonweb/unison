{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.Output
  ( Output (..),
    NumberedOutput (..),
    NumberedArgs,
    ListDetailed,
    HistoryTail (..),
    TestReportStats (..),
    UndoFailureReason (..),
    ReflogEntry (..),
    ShareError (..),
    isFailure,
    isNumberedFailure,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import Network.URI (URI)
import Unison.Auth.Types (CredentialFailure)
import qualified Unison.Codebase.Branch as Branch
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
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.Type (GitError)
import qualified Unison.CommandLine.InputPattern as Input
import Unison.DataDeclaration (Decl)
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
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
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import Unison.Server.Backend (ShallowListEntry (..))
import Unison.Server.SearchResult' (SearchResult')
import qualified Unison.Share.Sync.Types as Sync
import Unison.ShortHash (ShortHash)
import Unison.Symbol (Symbol)
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
  | ShowDiffAfterMerge Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterMergePropagate Path.Path' Path.Absolute Path.Path' PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterMergePreview Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterPull Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
  | ShowDiffAfterCreatePR ReadRemoteNamespace ReadRemoteNamespace PPE.PrettyPrintEnv (BranchDiffOutput Symbol Ann)
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
      [(Branch.CausalHash, Names.Diff)]
      HistoryTail -- 'origin point' of this view of history.
  | ListEdits Patch PPE.PrettyPrintEnv

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
  | -- Main function found, but has improper type
    BadMainFunction String (Type Symbol Ann) PPE.PrettyPrintEnv [Type Symbol Ann]
  | BranchEmpty (Either ShortBranchHash Path')
  | BranchNotEmpty Path'
  | LoadPullRequest ReadRemoteNamespace ReadRemoteNamespace Path' Path' Path' Path'
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
  | BranchHashAmbiguous ShortBranchHash (Set ShortBranchHash)
  | BadNamespace String String
  | BranchNotFound Path'
  | EmptyPush Path'
  | NameNotFound Path.HQSplit'
  | PatchNotFound Path.Split'
  | TypeNotFound Path.HQSplit'
  | TermNotFound Path.HQSplit'
  | TypeNotFound' ShortHash
  | TermNotFound' ShortHash
  | TypeTermMismatch (HQ.HashQualified Name) (HQ.HashQualified Name)
  | SearchTermsNotFound [HQ.HashQualified Name]
  | -- ask confirmation before deleting the last branch that contains some defns
    -- `Path` is one of the paths the user has requested to delete, and is paired
    -- with whatever named definitions would not have any remaining names if
    -- the path is deleted.
    DeleteBranchConfirmation
      [(Path', (Names, [SearchResult' Symbol Ann]))]
  | DeleteEverythingConfirmation
  | DeletedEverything
  | ListNames
      IsGlobal
      Int -- hq length to print References
      [(Reference, Set (HQ'.HashQualified Name))] -- type match, type names
      [(Referent, Set (HQ'.HashQualified Name))] -- term match, term names
      -- list of all the definitions within this branch
  | ListOfDefinitions PPE.PrettyPrintEnv ListDetailed [SearchResult' Symbol Ann]
  | ListOfLinks PPE.PrettyPrintEnv [(HQ.HashQualified Name, Reference, Maybe (Type Symbol Ann))]
  | ListShallow PPE.PrettyPrintEnv [ShallowListEntry Symbol Ann]
  | ListOfPatches (Set Name)
  | -- show the result of add/update
    SlurpOutput Input PPE.PrettyPrintEnv (SlurpResult Symbol)
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
  | Typechecked SourceName PPE.PrettyPrintEnv (SlurpResult Symbol) (UF.TypecheckedUnisonFile Symbol Ann)
  | DisplayRendered (Maybe FilePath) (P.Pretty P.ColorText)
  | -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
    DisplayDefinitions
      (Maybe FilePath)
      PPE.PrettyPrintEnvDecl
      (Map Reference (DisplayObject () (Decl Symbol Ann)))
      (Map Reference (DisplayObject (Type Symbol Ann) (Term Symbol Ann)))
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
  | ViewOnShare WriteShareRemotePath
  | ConfiguredMetadataParseError Path' String (P.Pretty P.ColorText)
  | NoConfiguredRemoteMapping PushPull Path.Absolute
  | ConfiguredRemoteMappingParseError PushPull Path.Absolute Text String
  | MetadataMissingType PPE.PrettyPrintEnv Referent
  | TermMissingType Reference
  | MetadataAmbiguous (HQ.HashQualified Name) PPE.PrettyPrintEnv [Referent]
  | -- todo: tell the user to run `todo` on the same patch they just used
    NothingToPatch PatchPath Path'
  | PatchNeedsToBeConflictFree
  | PatchInvolvesExternalDependents PPE.PrettyPrintEnv (Set Reference)
  | WarnIncomingRootBranch ShortBranchHash (Set ShortBranchHash)
  | StartOfCurrentPathHistory
  | ShowReflog [ReflogEntry]
  | PullAlreadyUpToDate ReadRemoteNamespace Path'
  | PullSuccessful ReadRemoteNamespace Path'
  | -- | Indicates a trivial merge where the destination was empty and was just replaced.
    MergeOverEmpty Path'
  | MergeAlreadyUpToDate Path' Path'
  | PreviewMergeAlreadyUpToDate Path' Path'
  | -- | No conflicts or edits remain for the current patch.
    NoConflictsOrEdits
  | NotImplemented
  | NoBranchWithHash ShortBranchHash
  | ListDependencies Int LabeledDependency [(Name, Reference)] (Set Reference)
  | -- | List dependents of a type or term.
    ListDependents Int LabeledDependency [(Reference, Maybe Name)]
  | -- | List all direct dependencies which don't have any names in the current branch
    ListNamespaceDependencies
      PPE.PrettyPrintEnv -- PPE containing names for everything from the root namespace.
      Path.Absolute -- The namespace we're checking dependencies for.
      (Map LabeledDependency (Set Name)) -- Mapping of external dependencies to their local dependents.
  | DumpNumberedArgs NumberedArgs
  | DumpBitBooster Branch.CausalHash (Map Branch.CausalHash [Branch.CausalHash])
  | DumpUnisonFileHashes Int [(Name, Reference.Id)] [(Name, Reference.Id)] [(Name, Reference.Id)]
  | BadName String
  | DefaultMetadataNotification
  | CouldntLoadBranch Branch.CausalHash
  | HelpMessage Input.InputPattern
  | NamespaceEmpty (NonEmpty AbsBranchId)
  | NoOp
  | -- Refused to push, either because a `push` targeted an empty namespace, or a `push.create` targeted a non-empty namespace.
    RefusedToPush PushBehavior WriteRemotePath
  | -- | @GistCreated repo@ means a causal was just published to @repo@.
    GistCreated ReadRemoteNamespace
  | -- | Directs the user to URI to begin an authorization flow.
    InitiateAuthFlow URI
  | UnknownCodeServer Text
  | CredentialFailureMsg CredentialFailure
  | PrintVersion Text
  | IntegrityCheck IntegrityResult

data ShareError
  = ShareErrorCheckAndSetPush Sync.CheckAndSetPushError
  | ShareErrorFastForwardPush Sync.FastForwardPushError
  | ShareErrorPull Sync.PullError
  | ShareErrorGetCausalHashByPath Sync.GetCausalHashByPathError
  | ShareErrorTransport Sync.CodeserverTransportError

data ReflogEntry = ReflogEntry {hash :: ShortBranchHash, reason :: Text}
  deriving (Show)

data HistoryTail
  = EndOfLog Branch.CausalHash
  | MergeTail Branch.CausalHash [Branch.CausalHash]
  | PageEnd Branch.CausalHash Int -- PageEnd nextHash nextIndex
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
  FindNoLocalMatches {} -> True
  PatchAlreadyExists {} -> True
  NoExactTypeMatches -> True
  BranchEmpty {} -> True
  EmptyPush {} -> True
  BranchNotEmpty {} -> True
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
  PatchNotFound {} -> True
  TypeNotFound {} -> True
  TypeNotFound' {} -> True
  TermNotFound {} -> True
  TermNotFound' {} -> True
  TypeTermMismatch {} -> True
  SearchTermsNotFound ts -> not (null ts)
  DeleteBranchConfirmation {} -> False
  DeleteEverythingConfirmation -> False
  DeletedEverything -> False
  ListNames _ _ tys tms -> null tms && null tys
  ListOfLinks _ ds -> null ds
  ListOfDefinitions _ _ ds -> null ds
  ListOfPatches s -> Set.null s
  SlurpOutput _ _ sr -> not $ SR.isOk sr
  ParseErrors {} -> True
  TypeErrors {} -> True
  CompilerBugs {} -> True
  DisplayConflicts {} -> False
  EvaluationFailure {} -> True
  Evaluated {} -> False
  Typechecked {} -> False
  DisplayDefinitions _ _ m1 m2 -> null m1 && null m2
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
  NothingToPatch {} -> False
  WarnIncomingRootBranch {} -> False
  StartOfCurrentPathHistory -> True
  NotImplemented -> True
  DumpNumberedArgs {} -> False
  DumpBitBooster {} -> False
  NoBranchWithHash {} -> True
  PullAlreadyUpToDate {} -> False
  PullSuccessful {} -> False
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

isNumberedFailure :: NumberedOutput -> Bool
isNumberedFailure = \case
  ShowDiffNamespace {} -> False
  ShowDiffAfterDeleteDefinitions {} -> False
  ShowDiffAfterDeleteBranch {} -> False
  ShowDiffAfterModifyBranch {} -> False
  ShowDiffAfterMerge {} -> False
  ShowDiffAfterMergePropagate {} -> False
  ShowDiffAfterMergePreview {} -> False
  ShowDiffAfterUndo {} -> False
  ShowDiffAfterPull {} -> False
  ShowDiffAfterCreatePR {} -> False
  ShowDiffAfterCreateAuthor {} -> False
  TodoOutput _ todo -> TO.todoScore todo > 0 || not (TO.noConflicts todo)
  CantDeleteDefinitions {} -> True
  CantDeleteNamespace {} -> True
  History {} -> False
  DeletedDespiteDependents {} -> False
  ListEdits {} -> False
