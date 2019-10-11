{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.Output
  ( Output(..)
  , ListDetailed
  , ShallowListEntry(..)
  , HistoryTail(..)
  , TestReportStats(..)
  , UndoFailureReason(..)
  , PushPull(..)
  , ReflogEntry(..)
  , pushPull
  , isFailure
  ) where

import Unison.Prelude

import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import Unison.Codebase.GitError
import Unison.Codebase.Path (Path')
import Unison.Codebase.Patch (Patch)
import Unison.Name ( Name )
import Unison.Names2 ( Names )
import Unison.Parser ( Ann )
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
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile as UF
import Unison.Codebase.Editor.DisplayThing (DisplayThing)
import qualified Unison.Codebase.Editor.TodoOutput as TO
import Unison.Codebase.Editor.SearchResult' (SearchResult')
import Unison.Type (Type)
import qualified Unison.Names3 as Names
import qualified Data.Set as Set
import Unison.Codebase.NameSegment (NameSegment, HQSegment)
import Unison.ShortHash (ShortHash)
import Unison.Var (Var)
import Unison.Codebase.ShortBranchHash (ShortBranchHash)

type Term v a = Term.AnnotatedTerm v a
type ListDetailed = Bool
type SourceName = Text

data PushPull = Push | Pull deriving (Eq, Ord, Show)

pushPull :: a -> a -> PushPull -> a
pushPull push pull p = case p of
  Push -> push
  Pull -> pull

data Output v
  -- Generic Success response; we might consider deleting this.
  -- I had put the `Input` field here in case we wanted the success message
  -- to vary based on the command the user submitted.
  = Success Input
  -- User did `add` or `update` before typechecking a file?
  | NoUnisonFile Input
  -- No main function, the [Type v Ann] are the allowed types
  | NoMainFunction Input String PPE.PrettyPrintEnv [Type v Ann]
  | CreatedNewBranch Path.Absolute
  | BranchAlreadyExists Input Path'
  | PatchAlreadyExists Input Path.Split'
  | NoExactTypeMatches
  | TypeAlreadyExists Input Path.Split' (Set Reference)
  | TypeParseError Input String (Parser.Err v)
  | ParseResolutionFailures Input String [Names.ResolutionFailure v Ann]
  | TypeHasFreeVars Input (Type v Ann)
  | TermAlreadyExists Input Path.Split' (Set Referent)
  | TypeAmbiguous Input Path.HQSplit' (Set Reference)
  | TermAmbiguous Input Path.HQSplit' (Set Referent)
  | HashAmbiguous Input ShortHash (Set Referent)
  | BranchHashAmbiguous Input ShortBranchHash (Set ShortBranchHash)
  | BadDestinationBranch Input Path'
  | BranchNotFound Input Path'
  | PatchNotFound Input Path.Split'
  | TypeNotFound Input Path.HQSplit'
  | TermNotFound Input Path.HQSplit'
  | TermNotFound' Input Reference.Id
  | SearchTermsNotFound [HQ.HashQualified]
  -- ask confirmation before deleting the last branch that contains some defns
  -- `Path` is one of the paths the user has requested to delete, and is paired
  -- with whatever named definitions would not have any remaining names if
  -- the path is deleted.
  | DeleteBranchConfirmation
      [(Path', (Names, [SearchResult' v Ann]))]
  -- CantDelete input couldntDelete becauseTheseStillReferenceThem
  | CantDelete Input PPE.PrettyPrintEnv [SearchResult' v Ann] [SearchResult' v Ann]
  | DeleteEverythingConfirmation
  | DeletedEverything
  | ListNames [(Referent, Set HQ'.HashQualified)] -- term match, term names
              [(Reference, Set HQ'.HashQualified)] -- type match, type names
  -- list of all the definitions within this branch
  | ListOfDefinitions PPE.PrettyPrintEnv ListDetailed [SearchResult' v Ann]
  | ListShallow PPE.PrettyPrintEnv [ShallowListEntry v Ann]
  | ListOfPatches (Set Name)
  -- show the result of add/update
  | SlurpOutput Input PPE.PrettyPrintEnv (SlurpResult v)
  -- Original source, followed by the errors:
  | ParseErrors Text [Parser.Err v]
  | TypeErrors Text PPE.PrettyPrintEnv [Context.ErrorNote v Ann]
  | DisplayConflicts (Relation Name Referent) (Relation Name Reference)
  | EvaluationFailure Runtime.Error
  | Evaluated SourceFileContents
              PPE.PrettyPrintEnv
              [(v, Term v ())]
              (Map v (Ann, UF.WatchKind, Term v (), Runtime.IsCacheHit))
  | Typechecked SourceName PPE.PrettyPrintEnv (SlurpResult v) (UF.TypecheckedUnisonFile v Ann)
  -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
  | DisplayDefinitions (Maybe FilePath)
                       PPE.PrettyPrintEnv
                       (Map Reference (DisplayThing (Decl v Ann)))
                       (Map Reference (DisplayThing (Term v Ann)))
  | TodoOutput PPE.PrettyPrintEnv (TO.TodoOutput v Ann)
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
  | BranchDiff Names Names
  | GitError Input GitError
  | NoConfiguredGitUrl PushPull Path'
  | DisplayLinks PPE.PrettyPrintEnv Metadata.Metadata
               (Map Reference (DisplayThing (Decl v Ann)))
               (Map Reference (DisplayThing (Term v Ann)))
  | LinkFailure Input
  -- todo: tell the user to run `todo` on the same patch they just used
  | NothingToPatch PatchPath Path'
  | PatchNeedsToBeConflictFree
  | PatchInvolvesExternalDependents PPE.PrettyPrintEnv (Set Reference)
  | WarnIncomingRootBranch (Set ShortBranchHash)
  | ShowDiff Input Names.Diff
  | History (Maybe Int) [(ShortBranchHash, Names.Diff)] HistoryTail
  | ShowReflog [ReflogEntry]
  | NothingTodo Input
  | NotImplemented
  | NoBranchWithHash Input ShortBranchHash
  | DumpBitBooster Branch.Hash (Map Branch.Hash [Branch.Hash])
--  deriving (Show)

data ReflogEntry =
  ReflogEntry { old :: ShortBranchHash, new :: ShortBranchHash, reason :: Text }

data ShallowListEntry v a
  = ShallowTermEntry Referent HQSegment (Maybe (Type v a))
  | ShallowTypeEntry Reference HQSegment
  | ShallowBranchEntry NameSegment Int -- number of child definitions
  | ShallowPatchEntry NameSegment
  deriving Eq

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
  NoMainFunction{} -> True
  CreatedNewBranch{} -> False
  BranchAlreadyExists{} -> True
  PatchAlreadyExists{} -> True
  NoExactTypeMatches -> True
  TypeAlreadyExists{} -> True
  TypeParseError{} -> True
  ParseResolutionFailures{} -> True
  TypeHasFreeVars{} -> True
  TermAlreadyExists{} -> True
  TypeAmbiguous{} -> True
  TermAmbiguous{} -> True
  BranchHashAmbiguous{} -> True
  BadDestinationBranch{} -> True
  BranchNotFound{} -> True
  PatchNotFound{} -> True
  TypeNotFound{} -> True
  TermNotFound{} -> True
  TermNotFound'{} -> True
  SearchTermsNotFound ts -> not (null ts)
  DeleteBranchConfirmation{} -> False
  CantDelete{} -> True
  DeleteEverythingConfirmation -> False
  DeletedEverything -> False
  ListNames tms tys -> null tms && null tys
  ListOfDefinitions _ _ ds -> null ds
  ListOfPatches s -> Set.null s
  SlurpOutput _ _ sr -> not $ SR.isOk sr
  ParseErrors{} -> True
  TypeErrors{} -> True
  DisplayConflicts{} -> False
  EvaluationFailure{} -> True
  Evaluated{} -> False
  Typechecked{} -> False
  DisplayDefinitions _ _ m1 m2 -> null m1 && null m2
  TodoOutput _ todo -> TO.todoScore todo /= 0
  TestIncrementalOutputStart{} -> False
  TestIncrementalOutputEnd{} -> False
  TestResults _ _ _ _ _ fails -> not (null fails)
  CantUndo{} -> True
  ListEdits{} -> False
  GitError{} -> True
  BustedBuiltins{} -> True
  NoConfiguredGitUrl{} -> True
  DisplayLinks{} -> False
  LinkFailure{} -> True
  PatchNeedsToBeConflictFree{} -> True
  PatchInvolvesExternalDependents{} -> True
  NothingToPatch{} -> False
  WarnIncomingRootBranch{} -> False
  History{} -> False
  ShowDiff{} -> False
  BranchDiff{} -> False
  NotImplemented -> True
  DumpBitBooster{} -> False
  NoBranchWithHash{} -> True
  NothingTodo{} -> False
  ListShallow _ es -> null es
  HashAmbiguous{} -> True
  ShowReflog{} -> False



