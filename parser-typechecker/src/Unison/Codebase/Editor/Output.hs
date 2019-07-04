{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.Output
  ( Output(..)
  , ListDetailed
  , SearchResult'(..)
  , TermResult'(..)
  , TestReportStats(..)
  , TodoOutput(..)
  , TypeResult'(..)
  , UndoFailureReason(..)
  , pattern Tm
  , pattern Tp
  , foldResult'
  , tmReferent
  , tpReference
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Unison.Codebase.Editor.Input
import Unison.Codebase.Editor.SlurpResult (SlurpResult(..))
import Unison.Codebase.GitError
import Unison.Codebase.Path (Path')
import Unison.Codebase.Patch (Patch)
import Unison.Name ( Name )
import Unison.Names2 ( Names, Names0 )
import Unison.Parser ( Ann )
import Unison.Reference ( Reference )
import Unison.Referent  ( Referent )
import Unison.DataDeclaration ( Decl )
import Unison.Util.Relation (Relation)
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Parser as Parser
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Typechecker.Context as Context
import qualified Unison.UnisonFile as UF
import Unison.Codebase.Editor.DisplayThing (DisplayThing)

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type ListDetailed = Bool
type SourceName = Text

data Output v
  -- Generic Success response; we might consider deleting this.
  -- I had put the `Input` field here in case we wanted the success message
  -- to vary based on the command the user submitted.
  = Success Input
  -- User did `add` or `update` before typechecking a file?
  | NoUnisonFile
  | CreatedNewBranch Path.Absolute
  | BranchAlreadyExists Input Path'
  | PatchAlreadyExists Input Path.Split'
  | NoExactTypeMatches
  | TypeAlreadyExists Input Path.Split' (Set Reference)
  | TypeParseError Input String (Parser.Err v)
  | TypeHasFreeVars Input (Type v Ann)
  | TermAlreadyExists Input Path.Split' (Set Referent)
  | TypeAmbiguous Input Path.HQSplit' (Set Reference)
  | TermAmbiguous Input Path.HQSplit' (Set Referent)
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
  | CantDelete Input Names0 [SearchResult' v Ann] [SearchResult' v Ann]
  | ListNames [(Referent, Set Name)] -- term match, term names
              [(Reference, Set Name)] -- type match, type names
  -- list of all the definitions within this branch
  | ListOfDefinitions Names0 ListDetailed [SearchResult' v Ann]
  | ListOfPatches (Set Name)
  | TermHistory Path.Split' [Referent]
  | TypeHistory Path.Split' [Reference]
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
  | FileChangeEvent SourceName Text
  -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
  | DisplayDefinitions (Maybe FilePath)
                       PPE.PrettyPrintEnv
                       (Map Reference (DisplayThing (Decl v Ann)))
                       (Map Reference (DisplayThing (Term v Ann)))
  | TodoOutput Names0 (TodoOutput v Ann)
  | TestIncrementalOutputStart PPE.PrettyPrintEnv (Int,Int) Reference (Term v Ann)
  | TestIncrementalOutputEnd PPE.PrettyPrintEnv (Int,Int) Reference (Term v Ann)
  | TestResults TestReportStats
      PPE.PrettyPrintEnv ShowSuccesses ShowFailures
                [(Reference, Text)] [(Reference, Text)]
  | CantUndo UndoFailureReason
  | ListEdits Patch Names0

  -- new/unrepresented references followed by old/removed
  -- todo: eventually replace these sets with [SearchResult' v Ann]
  -- and a nicer render.
  | BustedBuiltins (Set Reference) (Set Reference)
  | BranchDiff Names Names
  | GitError GitError
  | DisplayLinks PPE.PrettyPrintEnv Metadata.Metadata
               (Map Reference (DisplayThing (Decl v Ann)))
               (Map Reference (DisplayThing (Term v Ann)))
  | LinkFailure Input
  -- todo: tell the user to run `todo` on the same patch they just used
  | NothingToPatch PatchPath Path'
  | PatchNeedsToBeConflictFree
  | PatchInvolvesExternalDependents PPE.PrettyPrintEnv (Set Reference)
  deriving (Show)

data TestReportStats
  = CachedTests TotalCount CachedCount
  | NewlyComputed deriving Show

type TotalCount = Int -- total number of tests
type CachedCount = Int -- number of tests found in the cache
type ShowSuccesses = Bool -- whether to list results or just summarize
type ShowFailures = Bool  -- whether to list results or just summarize

data UndoFailureReason = CantUndoPastStart | CantUndoPastMerge deriving Show

data SearchResult' v a
  = Tm' (TermResult' v a)
  | Tp' (TypeResult' v a)
  deriving (Eq, Show)
data TermResult' v a =
  TermResult' HQ'.HashQualified (Maybe (Type v a)) Referent (Set HQ'.HashQualified)
  deriving (Eq, Show)
data TypeResult' v a =
  TypeResult' HQ'.HashQualified (DisplayThing (Decl v a)) Reference (Set HQ'.HashQualified)
  deriving (Eq, Show)
pattern Tm n t r as = Tm' (TermResult' n t r as)
pattern Tp n t r as = Tp' (TypeResult' n t r as)

tmReferent :: SearchResult' v a -> Maybe Referent
tmReferent = \case; Tm _ _ r _ -> Just r; _ -> Nothing
tpReference :: SearchResult' v a -> Maybe Reference
tpReference = \case; Tp _ _ r _ -> Just r; _ -> Nothing

foldResult' :: (TermResult' v a -> b) -> (TypeResult' v a -> b) -> SearchResult' v a -> b
foldResult' f g = \case
  Tm' tm -> f tm
  Tp' tp -> g tp

type SourceFileContents = Text

type Score = Int

data TodoOutput v a = TodoOutput_
  { todoScore :: Int
  , todoFrontier ::
        ( [(HQ'.HashQualified, Reference, Maybe (Type v a))]
        , [(HQ'.HashQualified, Reference, DisplayThing (Decl v a))])
  , todoFrontierDependents ::
        ( [(Score, HQ'.HashQualified, Reference, Maybe (Type v a))]
        , [(Score, HQ'.HashQualified, Reference, DisplayThing (Decl v a))])
  , nameConflicts :: Names0
  , editConflicts :: Patch
  } deriving (Show)
