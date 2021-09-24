
{-# LANGUAGE StandaloneDeriving #-} -- RLM: Not sure exactly


module Unison.Codebase.Editor.InputOutput
where 

import Unison.Prelude

import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Merge as Branch
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import           Unison.Codebase.Path           ( Path' )
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Path.Parse as Path
import           Unison.Codebase.Editor.RemoteRepo
import           Unison.ShortHash (ShortHash)
import           Unison.Codebase.ShortBranchHash (ShortBranchHash)
import qualified Unison.Codebase.ShortBranchHash as SBH
import           Unison.Codebase.SyncMode       ( SyncMode )
import           Unison.Name                    ( Name )
import           Unison.NameSegment             ( NameSegment )

import qualified Data.Text as Text


---

import Unison.Server.Backend (ShallowListEntry(..))
import Unison.Codebase (GetRootBranchError)
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Type (GitError)
import Unison.Names2 ( Names )
import Unison.Parser.Ann (Ann)
import qualified Unison.Reference as Reference
import Unison.Reference ( Reference )
import Unison.Referent  ( Referent )
import Unison.DataDeclaration ( Decl )
import Unison.Util.Relation (Relation)
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Parser as Parser
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import Unison.Codebase.Editor.DisplayObject (DisplayObject)
import qualified Unison.Codebase.Editor.TodoOutput as TO
import Unison.Server.SearchResult' (SearchResult')
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Names3 as Names
import qualified Data.Set as Set
import Unison.Codebase.Editor.Output.BranchDiff (BranchDiffOutput)
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.WatchKind as WK

-- EVERYTHING FROM INPUT 
data Event
  = UnisonFileChanged SourceName Source
  | IncomingRootBranch (Set Branch.Hash)

type Source = Text -- "id x = x\nconst a b = a"
type SourceName = Text -- "foo.u" or "buffer 7"
type BranchId = Either ShortBranchHash Path'
type HashOrHQSplit' = Either ShortHash Path.HQSplit'
type PatchPath = Path.Split'

parseBranchId :: String -> Either String BranchId
parseBranchId ('#':s) = case SBH.fromText (Text.pack s) of
  Nothing -> Left "Invalid hash, expected a base32hex string."
  Just h -> pure $ Left h
parseBranchId s = Right <$> Path.parsePath' s

data Input 
  -- names stuff:
    -- directory ops
    -- `Link` must describe a repo and a source path within that repo.
    -- clone w/o merge, error if would clobber
    = ForkLocalBranchI (Either ShortBranchHash Path') Path'
    -- pairs onboarding input with desired output response 
    -- | RespondToInput Input (Output String) -- RLM note: cycle happens here, also what is this type param? I chose an arbitrary type here and I know it shouldnt be a string. 
    | RespondToInput Input OutputSimple -- RLM note: cycle happens here, also what is this type param? I chose an arbitrary type here and I know it shouldnt be a string. 
    -- merge first causal into destination
    | MergeLocalBranchI Path' Path' Branch.MergeMode
    | PreviewMergeLocalBranchI Path' Path'
    | DiffNamespaceI Path' Path' -- old new
    | PullRemoteBranchI (Maybe ReadRemoteNamespace) Path' SyncMode
    | PushRemoteBranchI (Maybe WriteRemotePath) Path' SyncMode
    | CreatePullRequestI ReadRemoteNamespace ReadRemoteNamespace
    | LoadPullRequestI ReadRemoteNamespace ReadRemoteNamespace Path'
    | ResetRootI (Either ShortBranchHash Path')
    -- todo: Q: Does it make sense to publish to not-the-root of a Github repo?
    --          Does it make sense to fork from not-the-root of a Github repo?
    -- change directory
    | SwitchBranchI Path'
    | UpI
    | PopBranchI
    -- > names foo
    -- > names foo.bar
    -- > names .foo.bar
    -- > names .foo.bar#asdflkjsdf
    -- > names #sdflkjsdfhsdf
    | NamesI (HQ.HashQualified Name)
    | AliasTermI HashOrHQSplit' Path.Split'
    | AliasTypeI HashOrHQSplit' Path.Split'
    | AliasManyI [Path.HQSplit] Path'
    -- Move = Rename; It's an HQSplit' not an HQSplit', meaning the arg has to have a name.
    | MoveTermI Path.HQSplit' Path.Split'
    | MoveTypeI Path.HQSplit' Path.Split'
    | MoveBranchI (Maybe Path.Split') Path.Split'
    | MovePatchI Path.Split' Path.Split'
    | CopyPatchI Path.Split' Path.Split'
    -- delete = unname
    | DeleteI Path.HQSplit'
    | DeleteTermI Path.HQSplit'
    | DeleteTypeI Path.HQSplit'
    | DeleteBranchI (Maybe Path.Split')
    | DeletePatchI Path.Split'
    -- resolving naming conflicts within `branchpath`
      -- Add the specified name after deleting all others for a given reference
      -- within a given branch.
    | ResolveTermNameI Path.HQSplit'
    | ResolveTypeNameI Path.HQSplit'
  -- edits stuff:
    | LoadI (Maybe FilePath)
    | AddI [HQ'.HashQualified Name]
    | PreviewAddI [HQ'.HashQualified Name]
    | UpdateI (Maybe PatchPath) [HQ'.HashQualified Name]
    | PreviewUpdateI [HQ'.HashQualified Name]
    | TodoI (Maybe PatchPath) Path'
    | PropagatePatchI PatchPath Path'
    | ListEditsI (Maybe PatchPath)
    -- -- create and remove update directives
    | DeprecateTermI PatchPath Path.HQSplit'
    | DeprecateTypeI PatchPath Path.HQSplit'
    | ReplaceI (HQ.HashQualified Name) (HQ.HashQualified Name) (Maybe PatchPath)
    | RemoveTermReplacementI (HQ.HashQualified Name) (Maybe PatchPath)
    | RemoveTypeReplacementI (HQ.HashQualified Name) (Maybe PatchPath)
  | UndoI
  -- First `Maybe Int` is cap on number of results, if any
  -- Second `Maybe Int` is cap on diff elements shown, if any
  | HistoryI (Maybe Int) (Maybe Int) BranchId
  -- execute an IO thunk
  | ExecuteI String
  -- execute an IO [Result]
  | IOTestI (HQ.HashQualified Name)
  | TestI Bool Bool -- TestI showSuccesses showFailures
  -- metadata
  -- `link metadata definitions` (adds metadata to all of `definitions`)
  | LinkI (HQ.HashQualified Name) [Path.HQSplit']
  -- `unlink metadata definitions` (removes metadata from all of `definitions`)
  | UnlinkI (HQ.HashQualified Name) [Path.HQSplit']
  -- links from <type>
  | LinksI Path.HQSplit' (Maybe String)
  | CreateAuthorI NameSegment {- identifier -} Text {- name -}
  | DisplayI OutputLocation (HQ.HashQualified Name)
  | DocsI Path.HQSplit'
  -- other
  | SearchByNameI Bool Bool [String] -- SearchByName isVerbose showAll query
  | FindShallowI Path'
  | FindPatchI
  | ShowDefinitionI OutputLocation [HQ.HashQualified Name]
  | ShowDefinitionByPrefixI OutputLocation [HQ.HashQualified Name]
  | ShowReflogI
  | UpdateBuiltinsI
  | MergeBuiltinsI
  | MergeIOBuiltinsI
  | ListDependenciesI (HQ.HashQualified Name)
  | ListDependentsI (HQ.HashQualified Name)
  | DebugNumberedArgsI
  | DebugTypecheckedUnisonFileI
  | DebugDumpNamespacesI
  | DebugDumpNamespaceSimpleI
  | DebugClearWatchI
  | QuitI
  | UiI
  deriving (Eq, Show) -- <<< RLM: Need to figure this one out 

-- Some commands, like `view`, can dump output to either console or a file.
data OutputLocation
  = ConsoleLocation
  | LatestFileLocation
  | FileLocation FilePath
  -- ClipboardLocation
  deriving (Eq, Show)

-- OUTPUT STUFF BELOW  
type ListDetailed = Bool
type NumberedArgs = [String]

data PushPull = Push | Pull deriving (Eq, Ord, Show)

pushPull :: a -> a -> PushPull -> a
pushPull push pull p = case p of
  Push -> push
  Pull -> pull

data NumberedOutput v
  = ShowDiffNamespace Path.Absolute Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterUndo PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterDeleteDefinitions PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterDeleteBranch Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterModifyBranch Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterMerge Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterMergePropagate Path.Path' Path.Absolute Path.Path' PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterMergePreview Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterPull Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  | ShowDiffAfterCreatePR ReadRemoteNamespace ReadRemoteNamespace PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  -- <authorIdentifier> <authorPath> <relativeBase>
  | ShowDiffAfterCreateAuthor NameSegment Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)

--  | ShowDiff

-- RLM: SPIKE - Ok a creative but potentially bad idea that would solve many of my woes is to break out Output v into OutputWithNoType Parameter. 
data OutputSimple 
  = Onboarding String 
  | Success 
  deriving (Eq, Show)

data Output v
  -- Generic Success response; we might consider deleting this.
  = Simple OutputSimple -- RLM: Test here 
  -- User did `add` or `update` before typechecking a file?
  | NoUnisonFile
  | InvalidSourceName String
  | SourceLoadFailed String
  -- No main function, the [Type v Ann] are the allowed types
  | NoMainFunction String PPE.PrettyPrintEnv [Type v Ann]
  -- Main function found, but has improper type
  | BadMainFunction String (Type v Ann) PPE.PrettyPrintEnv [Type v Ann]
  | BranchEmpty (Either ShortBranchHash Path')
  | BranchNotEmpty Path'
  | LoadPullRequest ReadRemoteNamespace ReadRemoteNamespace Path' Path' Path' Path'
  | CreatedNewBranch Path.Absolute
  | BranchAlreadyExists Path'
  | PatchAlreadyExists Path.Split'
  | NoExactTypeMatches
  | TypeAlreadyExists Path.Split' (Set Reference)
  | TypeParseError String (Parser.Err v)
  | ParseResolutionFailures String [Names.ResolutionFailure v Ann]
  | TypeHasFreeVars (Type v Ann)
  | TermAlreadyExists Path.Split' (Set Referent)
  | LabeledReferenceAmbiguous Int (HQ.HashQualified Name) (Set LabeledDependency)
  | LabeledReferenceNotFound (HQ.HashQualified Name)
  | DeleteNameAmbiguous Int Path.HQSplit' (Set Referent) (Set Reference)
  | TermAmbiguous (HQ.HashQualified Name) (Set Referent)
  | HashAmbiguous ShortHash (Set Referent)
  | BranchHashAmbiguous ShortBranchHash (Set ShortBranchHash)
  | BranchNotFound Path'
  | NameNotFound Path.HQSplit'
  | PatchNotFound Path.Split'
  | TypeNotFound Path.HQSplit'
  | TermNotFound Path.HQSplit'
  | TypeNotFound' ShortHash
  | TermNotFound' ShortHash
  | TypeTermMismatch (HQ.HashQualified Name) (HQ.HashQualified Name)
  | SearchTermsNotFound [HQ.HashQualified Name]
  -- ask confirmation before deleting the last branch that contains some defns
  -- `Path` is one of the paths the user has requested to delete, and is paired
  -- with whatever named definitions would not have any remaining names if
  -- the path is deleted.
  | DeleteBranchConfirmation
      [(Path', (Names, [SearchResult' v Ann]))]
  -- CantDelete input couldntDelete becauseTheseStillReferenceThem
  | CantDelete PPE.PrettyPrintEnv [SearchResult' v Ann] [SearchResult' v Ann]
  | DeleteEverythingConfirmation
  | DeletedEverything
  | ListNames Int -- hq length to print References
              [(Reference, Set (HQ'.HashQualified Name))] -- type match, type names
              [(Referent, Set (HQ'.HashQualified Name))] -- term match, term names
  -- list of all the definitions within this branch
  | ListOfDefinitions PPE.PrettyPrintEnv ListDetailed [SearchResult' v Ann]
  | ListOfLinks PPE.PrettyPrintEnv [(HQ.HashQualified Name, Reference, Maybe (Type v Ann))]
  | ListShallow PPE.PrettyPrintEnv [ShallowListEntry v Ann]
  | ListOfPatches (Set Name)
  -- show the result of add/update
  | SlurpOutput Input PPE.PrettyPrintEnv (SlurpResult v)
  -- Original source, followed by the errors:
  | ParseErrors Text [Parser.Err v]
  | TypeErrors Text PPE.PrettyPrintEnv [Context.ErrorNote v Ann]
  | CompilerBugs Text PPE.PrettyPrintEnv [Context.CompilerBug v Ann]
  | DisplayConflicts (Relation Name Referent) (Relation Name Reference)
  | EvaluationFailure Runtime.Error
  | Evaluated SourceFileContents
              PPE.PrettyPrintEnv
              [(v, Term v ())]
              (Map v (Ann, WK.WatchKind, Term v (), Runtime.IsCacheHit))
  | Typechecked SourceName PPE.PrettyPrintEnv (SlurpResult v) (UF.TypecheckedUnisonFile v Ann)
  | DisplayRendered (Maybe FilePath) (P.Pretty P.ColorText)
  -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
  | DisplayDefinitions (Maybe FilePath)
                       PPE.PrettyPrintEnvDecl
                       (Map Reference (DisplayObject () (Decl v Ann)))
                       (Map Reference (DisplayObject (Type v Ann) (Term v Ann)))
  -- | Invariant: there's at least one conflict or edit in the TodoOutput.
  | TodoOutput PPE.PrettyPrintEnvDecl (TO.TodoOutput v Ann)
  | TestIncrementalOutputStart PPE.PrettyPrintEnv (Int,Int) Reference (Term v Ann)
  | TestIncrementalOutputEnd PPE.PrettyPrintEnv (Int,Int) Reference (Term v Ann)
  | TestResults TestReportStats
      PPE.PrettyPrintEnv ShowSuccesses ShowFailures
                [(Reference, Text)] -- oks
                [(Reference, Text)] -- fails
  | CantUndo UndoFailureReason
  | ListEdits Patch PPE.PrettyPrintEnv

  -- new/unrepresented references followed by old/removed
  -- todo: eventually replace these sets with [SearchResult' v Ann]
  -- and a nicer render.
  | BustedBuiltins (Set Reference) (Set Reference)
  | GitError Input GitError
  | ConfiguredMetadataParseError Path' String (P.Pretty P.ColorText)
  | NoConfiguredGitUrl PushPull Path'
  | ConfiguredGitUrlParseError PushPull Path' Text String
  | DisplayLinks PPE.PrettyPrintEnvDecl Metadata.Metadata
               (Map Reference (DisplayObject () (Decl v Ann)))
               (Map Reference (DisplayObject (Type v Ann) (Term v Ann)))
  | MetadataMissingType PPE.PrettyPrintEnv Referent
  | TermMissingType Reference
  | MetadataAmbiguous (HQ.HashQualified Name) PPE.PrettyPrintEnv [Referent]
  -- todo: tell the user to run `todo` on the same patch they just used
  | NothingToPatch PatchPath Path'
  | PatchNeedsToBeConflictFree
  | PatchInvolvesExternalDependents PPE.PrettyPrintEnv (Set Reference)
  | WarnIncomingRootBranch ShortBranchHash (Set ShortBranchHash)
  | StartOfCurrentPathHistory
  | History (Maybe Int) [(ShortBranchHash, Names.Diff)] HistoryTail
  | ShowReflog [ReflogEntry]
  | PullAlreadyUpToDate ReadRemoteNamespace Path'
  | MergeAlreadyUpToDate Path' Path'
  | PreviewMergeAlreadyUpToDate Path' Path'
  -- | No conflicts or edits remain for the current patch.
  | NoConflictsOrEdits
  | NotImplemented
  | NoBranchWithHash ShortBranchHash
  | ListDependencies Int LabeledDependency [(Name, Reference)] (Set Reference)
  | ListDependents Int LabeledDependency [(Name, Reference)] (Set Reference)
  | DumpNumberedArgs NumberedArgs
  | DumpBitBooster Branch.Hash (Map Branch.Hash [Branch.Hash])
  | DumpUnisonFileHashes Int [(Name, Reference.Id)] [(Name, Reference.Id)] [(Name, Reference.Id)]
  | BadName String
  | DefaultMetadataNotification
  | BadRootBranch GetRootBranchError
  | CouldntLoadBranch Branch.Hash
  | NoOp
  deriving (Show)

data ReflogEntry =
  ReflogEntry { hash :: ShortBranchHash, reason :: Text }
  deriving (Show)

data HistoryTail =
  EndOfLog ShortBranchHash |
  MergeTail ShortBranchHash [ShortBranchHash] |
  PageEnd ShortBranchHash Int -- PageEnd nextHash nextIndex
  deriving (Show)

data TestReportStats
  = CachedTests TotalCount CachedCount
  | NewlyComputed deriving Show

type TotalCount = Int -- total number of tests
type CachedCount = Int -- number of tests found in the cache
type ShowSuccesses = Bool -- whether to list results or just summarize
type ShowFailures = Bool  -- whether to list results or just summarize

data UndoFailureReason = CantUndoPastStart | CantUndoPastMerge deriving Show

type SourceFileContents = Text

isFailure :: Ord v => Output v -> Bool
isFailure o = case o of
  Simple Success{} -> False
  Simple Onboarding{} -> False 
  BadRootBranch{} -> True
  CouldntLoadBranch{} -> True
  NoUnisonFile{} -> True
  InvalidSourceName{} -> True
  SourceLoadFailed{} -> True
  NoMainFunction{} -> True
  BadMainFunction{} -> True
  CreatedNewBranch{} -> False
  BranchAlreadyExists{} -> True
  PatchAlreadyExists{} -> True
  NoExactTypeMatches -> True
  BranchEmpty{} -> True
  BranchNotEmpty{} -> True
  TypeAlreadyExists{} -> True
  TypeParseError{} -> True
  ParseResolutionFailures{} -> True
  TypeHasFreeVars{} -> True
  TermAlreadyExists{} -> True
  LabeledReferenceAmbiguous{} -> True
  LabeledReferenceNotFound{} -> True
  DeleteNameAmbiguous{} -> True
  TermAmbiguous{} -> True
  BranchHashAmbiguous{} -> True
  BadName{} -> True
  BranchNotFound{} -> True
  NameNotFound{} -> True
  PatchNotFound{} -> True
  TypeNotFound{} -> True
  TypeNotFound'{} -> True
  TermNotFound{} -> True
  TermNotFound'{} -> True
  TypeTermMismatch{} -> True
  SearchTermsNotFound ts -> not (null ts)
  DeleteBranchConfirmation{} -> False
  CantDelete{} -> True
  DeleteEverythingConfirmation -> False
  DeletedEverything -> False
  ListNames _ tys tms -> null tms && null tys
  ListOfLinks _ ds -> null ds
  ListOfDefinitions _ _ ds -> null ds
  ListOfPatches s -> Set.null s
  SlurpOutput _ _ sr -> not $ SR.isOk sr
  ParseErrors{} -> True
  TypeErrors{} -> True
  CompilerBugs{} -> True
  DisplayConflicts{} -> False
  EvaluationFailure{} -> True
  Evaluated{} -> False
  Typechecked{} -> False
  DisplayDefinitions _ _ m1 m2 -> null m1 && null m2
  DisplayRendered{} -> False
  TodoOutput _ todo -> TO.todoScore todo > 0 || not (TO.noConflicts todo)
  TestIncrementalOutputStart{} -> False
  TestIncrementalOutputEnd{} -> False
  TestResults _ _ _ _ _ fails -> not (null fails)
  CantUndo{} -> True
  ListEdits{} -> False
  GitError{} -> True
  BustedBuiltins{} -> True
  ConfiguredMetadataParseError{} -> True
  NoConfiguredGitUrl{} -> True
  ConfiguredGitUrlParseError{} -> True
  DisplayLinks{} -> False
  MetadataMissingType{} -> True
  MetadataAmbiguous{} -> True
  PatchNeedsToBeConflictFree{} -> True
  PatchInvolvesExternalDependents{} -> True
  NothingToPatch{} -> False
  WarnIncomingRootBranch{} -> False
  History{} -> False
  StartOfCurrentPathHistory -> True
  NotImplemented -> True
  DumpNumberedArgs{} -> False
  DumpBitBooster{} -> False
  NoBranchWithHash{} -> True
  PullAlreadyUpToDate{} -> False
  MergeAlreadyUpToDate{} -> False
  PreviewMergeAlreadyUpToDate{} -> False
  NoConflictsOrEdits{} -> False
  ListShallow _ es -> null es
  HashAmbiguous{} -> True
  ShowReflog{} -> False
  LoadPullRequest{} -> False
  DefaultMetadataNotification -> False
  NoOp -> False
  ListDependencies{} -> False
  ListDependents{} -> False
  TermMissingType{} -> True
  DumpUnisonFileHashes _ x y z -> x == mempty && y == mempty && z == mempty

isNumberedFailure :: NumberedOutput v -> Bool
isNumberedFailure = \case
  ShowDiffNamespace{} -> False
  ShowDiffAfterDeleteDefinitions{} -> False
  ShowDiffAfterDeleteBranch{} -> False
  ShowDiffAfterModifyBranch{} -> False
  ShowDiffAfterMerge{} -> False
  ShowDiffAfterMergePropagate{} -> False
  ShowDiffAfterMergePreview{} -> False
  ShowDiffAfterUndo{} -> False
  ShowDiffAfterPull{} -> False
  ShowDiffAfterCreatePR{} -> False
  ShowDiffAfterCreateAuthor{} -> False


