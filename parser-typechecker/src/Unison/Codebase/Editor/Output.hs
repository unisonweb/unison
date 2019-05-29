{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Codebase.Editor.Output
  ( Output(..)
  , DisplayThing(..)
  , TodoOutput(..)
  , ListDetailed
  , SlurpResult(..)
  , SearchResult'(..)
  , TermResult'(..)
  , TypeResult'(..)
  , pattern Tm
  , pattern Tp
  , foldResult'
  , isNonemptySlurp
  , disallowUpdates
  , tmReferent
  , tpReference
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

import Unison.Codebase.Path (Path')
import Unison.Codebase.Editor.Input

import           Unison.Name                    ( Name )
import Unison.Parser ( Ann )
import Unison.Reference ( Reference )
import Unison.Referent  ( Referent )
import Unison.Names2 ( Names )
import           Unison.HashQualified           ( HashQualified )

import           Unison.Util.Relation          (Relation)
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Runtime       as Runtime
import qualified Unison.Parser                 as Parser
import qualified Unison.Reference              as Reference
import qualified Unison.UnisonFile             as UF
import qualified Unison.Typechecker.Context    as Context
import           Unison.Typechecker.TypeLookup  ( Decl )
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.Util.Monoid            as Monoid

type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a
type ListDetailed = Bool
type SourceName = Text

-- data DisplayThing a = BuiltinThing | MissingThing Reference.Id | RegularThing a
--   deriving (Eq, Ord, Show)

data Output v
  -- Generic Success response; we might consider deleting this.
  -- I had put the `Input` field here in case we wanted the success message
  -- to vary based on the command the user submitted.
  = Success Input
  -- User did `add` or `update` before typechecking a file?
  | NoUnisonFile
  | CreatedNewBranch Path.Absolute
  | BranchAlreadyExists Input Path.Split'
  | TypeAlreadyExists Input Path.Split' (Set Reference)
  | TermAlreadyExists Input Path.Split' (Set Referent)
  | TypeAmbiguous Input Path.HQSplit' (Set Reference)
  | TermAmbiguous Input Path.HQSplit' (Set Referent)
  | BranchNotFound Input Path.Split'
  | TypeNotFound Input Path.HQSplit'
  | TermNotFound Input Path.HQSplit'
  -- ask confirmation before deleting the last branch that contains some defns
  -- `Path` is one of the paths the user has requested to delete, and is paired
  -- with whatever named definitions would not have any remaining names if
  -- the path is deleted.
  | DeleteBranchConfirmation
      [(Path', (Names, [SearchResult' v Ann]))]
  -- CantDelete input couldntDelete becauseTheseStillReferenceThem
  | CantDelete Input [SearchResult' v Ann] [SearchResult' v Ann]
  -- list of all the definitions within this branch
  | ListOfDefinitions Names ListDetailed [SearchResult' v Ann]
  -- show the result of add/update
  | SlurpOutput Input (SlurpResult v)
  -- Original source, followed by the errors:
  | ParseErrors Text [Parser.Err v]
  | TypeErrors Text Names [Context.ErrorNote v Ann]
  | DisplayConflicts (Relation Name Referent) (Relation Name Reference)
  | Evaluated SourceFileContents
              Names
              [(v, Term v ())]
              (Map v (Ann, Term v (), Runtime.IsCacheHit))
  | Typechecked SourceName Names (UF.TypecheckedUnisonFile v Ann)
  | FileChangeEvent SourceName Text
  -- "display" definitions, possibly to a FilePath on disk (e.g. editing)
  | DisplayDefinitions (Maybe FilePath)
                       Names
                       (Map Reference (DisplayThing (Decl v Ann)))
                       (Map Reference (DisplayThing (Term v Ann)))
  | TodoOutput Names (TodoOutput v Ann)
  -- | ListEdits Edits Names

  -- new/unrepresented references followed by old/removed
  -- todo: eventually replace these sets with [SearchResult' v Ann]
  -- and a nicer render.
  | BustedBuiltins (Set Reference) (Set Reference)
  | BranchDiff Names Names
  deriving (Show)

data DisplayThing a = BuiltinThing | MissingThing Reference.Id | RegularThing a
  deriving (Eq, Ord, Show)

data SearchResult' v a
  = Tm' (TermResult' v a)
  | Tp' (TypeResult' v a)
  deriving (Eq, Show)
data TermResult' v a =
  TermResult' HashQualified (Maybe (Type v a)) Referent (Set HashQualified)
  deriving (Eq, Show)
data TypeResult' v a =
  TypeResult' HashQualified (DisplayThing (Decl v a)) Reference (Set HashQualified)
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

data TodoOutput v a
  = TodoOutput_ {
      todoScore :: Int,
      todoFrontier ::
        ( [(HashQualified, Reference, Maybe (Type v a))]
        , [(HashQualified, Reference, DisplayThing (Decl v a))]),
      todoFrontierDependents ::
        ( [(Score, HashQualified, Reference, Maybe (Type v a))]
        , [(Score, HashQualified, Reference, DisplayThing (Decl v a))])--,
      -- todoConflicts :: OldBranch.Branch0
    } deriving (Show)

-- -- todo: do we want something here for nonexistent old name?
-- data NameChangeResult = NameChangeResult
--   { _oldNameConflicted :: Set DefnTarget
--   , _newNameAlreadyExists :: Set DefnTarget
--   , _changedSuccessfully :: Set DefnTarget
--   } deriving (Eq, Ord, Show)

data SlurpComponent v =
  SlurpComponent { implicatedTypes :: Set v, implicatedTerms :: Set v }
  deriving (Eq,Ord,Show)

instance Ord v => Semigroup (SlurpComponent v) where
  (<>) = mappend
instance Ord v => Monoid (SlurpComponent v) where
  mempty = SlurpComponent mempty mempty
  c1 `mappend` c2 = SlurpComponent (implicatedTypes c1 <> implicatedTypes c2)
                                   (implicatedTerms c1 <> implicatedTerms c2)

-- foo = bar + 1  -- new definition
-- bar = 7        -- updated definition
--
-- > add
-- suppose bar already exists.
-- SlurpResult:
-- adds = {foo}
-- updates = {bar}


data SlurpResult v = SlurpResult {
  -- The file that we tried to add from
    originalFile :: UF.TypecheckedUnisonFile v Ann
  -- the transitive closure of the user-specified HQs
  , finalFile :: UF.TypecheckedUnisonFile v Ann
  -- Extra definitions that were added to satisfy transitive closure,
  -- beyond what the user specified.
  , extraDefinitions :: SlurpComponent v
  -- Previously existed only in the file; now added to the codebase.
  , adds :: SlurpComponent v
  -- Exists in the branch and the file, with the same name and contents.
  , duplicates :: SlurpComponent v
  -- Not added to codebase due to the name already existing
  -- in the branch with a different definition.
  , collisions :: SlurpComponent v
  -- Not added to codebase due to the name existing
  -- in the branch with a conflict (two or more definitions).
  , conflicts :: SlurpComponent v
  -- Names that already exist in the branch, but whose definitions
  -- in `originalFile` are treated as updates.
  , updates :: SlurpComponent v
  -- Names of terms in `originalFile` that couldn't be updated because
  -- they refer to existing constructors. (User should instead do a find/replace,
  -- a constructor rename, or refactor the type that the name comes from).
  , termExistingConstructorCollisions :: Map v Referent
  , constructorExistingTermCollisions :: Map v [Referent]
  -- -- Already defined in the branch, but with a different name.
  , termAlias :: Map v (Set Name)
  , typeAlias :: Map v (Set Name)
  , termsWithBlockedDependencies :: Map v (Set Reference)
  , typesWithBlockedDependencies :: Map v (Set Reference)
  } deriving (Show)

disallowUpdates :: SlurpResult v -> SlurpResult v
disallowUpdates = error "todo"

isNonemptySlurp :: Ord v => SlurpResult v -> Bool
isNonemptySlurp s = Monoid.nonEmpty (adds s) || Monoid.nonEmpty (updates s)
