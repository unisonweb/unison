{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.Output
  ( Output(..)
  , NumberedOutput(..)
  , NumberedArgs
  , ListDetailed
  , ShallowListEntry(..)
  , HistoryTail(..)
  , TestReportStats(..)
  , UndoFailureReason(..)
  , PushPull(..)
  , ReflogEntry(..)
  , pushPull
  , isFailure
  , isNumberedFailure
  ) where

import Unison.Prelude

import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import Unison.Codebase.GitError
import Unison.Codebase.Path (Path', Path)
import Unison.Codebase.Patch (Patch)
import Unison.Name ( Name )
import Unison.Names2 ( Names )
import Unison.Parser ( Ann )
import qualified Unison.Reference as Reference
import Unison.Reference ( Reference )
import Unison.Referent  ( Referent )
import Unison.DataDeclaration ( Decl )
import Unison.Util.Relation (Relation)
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.SlurpResult as SR
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Parser as Parser
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import Unison.Codebase.Editor.DisplayThing (DisplayThing)
import qualified Unison.Codebase.Editor.TodoOutput as TO
import Unison.Codebase.Editor.SearchResult' (SearchResult')
import Unison.Term (Term)
import Unison.Type (Type)
import qualified Unison.Names3 as Names
import qualified Data.Set as Set
import Unison.NameSegment (NameSegment)
import Unison.ShortHash (ShortHash)
import Unison.Var (Var)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import Unison.Codebase.Editor.Output.BranchDiff (BranchDiffOutput)
import Unison.LabeledDependency (LabeledDependency)

type ListDetailed = Bool
type SourceName = Text
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
  | ShowDiffAfterCreatePR RemoteNamespace RemoteNamespace PPE.PrettyPrintEnv (BranchDiffOutput v Ann)
  -- <authorIdentifier> <authorPath> <relativeBase>
  | ShowDiffAfterCreateAuthor NameSegment Path.Path' Path.Absolute PPE.PrettyPrintEnv (BranchDiffOutput v Ann)

--  | ShowDiff

data Output v
  -- Generic Success response; we might consider deleting this.
  = Success
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
  | LoadPullRequest RemoteNamespace RemoteNamespace Path' Path' Path' Path'
  | CreatedNewBranch Path.Absolute
  | BranchAlreadyExists Path'
  | PatchAlreadyExists Path.Split'
  | NoExactTypeMatches
  | TypeAlreadyExists Path.Split' (Set Reference)
  | TypeParseError String (Parser.Err v)
  | ParseResolutionFailures String [Names.ResolutionFailure v Ann]
  | TypeHasFreeVars (Type v Ann)
  | TermAlreadyExists Path.Split' (Set Referent)
  | LabeledReferenceAmbiguous Int HQ.HashQualified (Set LabeledDependency)
  | LabeledReferenceNotFound HQ.HashQualified
  | DeleteNameAmbiguous Int Path.HQSplit' (Set Referent) (Set Reference)
  | TermAmbiguous HQ.HashQualified (Set Referent)
  | HashAmbiguous ShortHash (Set Referent)
  | BranchHashAmbiguous ShortBranchHash (Set ShortBranchHash)
  | BranchNotFound Path'
  | NameNotFound Path.HQSplit'
  | PatchNotFound Path.Split'
  | TypeNotFound Path.HQSplit'
  | TermNotFound Path.HQSplit'
  | TypeNotFound' ShortHash
  | TermNotFound' ShortHash
  | SearchTermsNotFound [HQ.HashQualified]
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
              [(Reference, Set HQ'.HashQualified)] -- type match, type names
              [(Referent, Set HQ'.HashQualified)] -- term match, term names
  -- list of all the definitions within this branch
  | ListOfDefinitions PPE.PrettyPrintEnv ListDetailed [SearchResult' v Ann]
  | ListOfLinks PPE.PrettyPrintEnv [(HQ.HashQualified, Reference, Maybe (Type v Ann))]
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
              (Map v (Ann, UF.WatchKind, Term v (), Runtime.IsCacheHit))
  | Typechecked SourceName PPE.PrettyPrintEnv (SlurpResult v) (UF.TypecheckedUnisonFile v Ann)
  | DisplayRendered (Maybe FilePath) (P.Pretty P.ColorText)
  -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
  | DisplayDefinitions (Maybe FilePath)
                       PPE.PrettyPrintEnvDecl
                       (Map Reference (DisplayThing (Decl v Ann)))
                       (Map Reference (DisplayThing (Term v Ann)))
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
  | ConfiguredGitUrlIncludesShortBranchHash PushPull RemoteRepo ShortBranchHash Path
  | DisplayLinks PPE.PrettyPrintEnvDecl Metadata.Metadata
               (Map Reference (DisplayThing (Decl v Ann)))
               (Map Reference (DisplayThing (Term v Ann)))
  | MetadataMissingType PPE.PrettyPrintEnv Referent
  | MetadataAmbiguous HQ.HashQualified PPE.PrettyPrintEnv [Referent]
  -- todo: tell the user to run `todo` on the same patch they just used
  | NothingToPatch PatchPath Path'
  | PatchNeedsToBeConflictFree
  | PatchInvolvesExternalDependents PPE.PrettyPrintEnv (Set Reference)
  | WarnIncomingRootBranch ShortBranchHash (Set ShortBranchHash)
  | StartOfCurrentPathHistory
  | History (Maybe Int) [(ShortBranchHash, Names.Diff)] HistoryTail
  | ShowReflog [ReflogEntry]
  | PullAlreadyUpToDate RemoteNamespace Path'
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
  | NoOp
  deriving (Show)

data ReflogEntry =
  ReflogEntry { hash :: ShortBranchHash, reason :: Text }
  deriving (Show)

data ShallowListEntry v a
  = ShallowTermEntry Referent HQ'.HQSegment (Maybe (Type v a))
  | ShallowTypeEntry Reference HQ'.HQSegment
  | ShallowBranchEntry NameSegment Int -- number of child definitions
  | ShallowPatchEntry NameSegment
  deriving (Eq, Show)

-- requires Var v to derive Eq, which is required by Ord though not by `compare`
instance Var v => Ord (ShallowListEntry v a) where
   compare x y = case compare (toNS x) (toNS y) of
     EQ -> compare (toHash x) (toHash y)
     c  -> c
     where
     toNS = \case
       ShallowTermEntry _ hq _ -> HQ'.toName hq
       ShallowTypeEntry _ hq   -> HQ'.toName hq
       ShallowBranchEntry ns _ -> ns
       ShallowPatchEntry  ns   -> ns
     toHash :: ShallowListEntry v a -> Maybe ShortHash
     toHash = \case
       ShallowTermEntry _ hq _ -> HQ'.toHash hq
       ShallowTypeEntry _ hq   -> HQ'.toHash hq
       ShallowBranchEntry _  _ -> Nothing
       ShallowPatchEntry _     -> Nothing

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
  Success{} -> False
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
  TodoOutput _ todo -> TO.todoScore todo /= 0
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
  ConfiguredGitUrlIncludesShortBranchHash{} -> True
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


