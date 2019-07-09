{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor.Command (
  Command(..),
  AmbientAbilities,
  LexedSource,
  Source,
  SourceName,
  TypecheckingResult
  ) where

import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import Data.Sequence (Seq)
import Data.Text (Text)


import           Unison.Codebase.Editor.Output
import           Unison.Codebase.Editor.RemoteRepo

import           Unison.Codebase.Branch         ( Branch )
import           Unison.Codebase.GitError
import           Unison.Names3                  ( Names )
import           Unison.Parser                  ( Ann )
import           Unison.Referent                ( Referent )
import           Unison.Reference               ( Reference )
import           Unison.Result                  ( Note
                                                , Result)
import           Unison.DataDeclaration         ( Decl )
import qualified Unison.Codebase.Runtime       as Runtime
import qualified Unison.ConstructorType        as CT
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Reference              as Reference
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.UnisonFile             as UF
import qualified Unison.Lexer                  as L
import qualified Unison.Parser                 as Parser


type AmbientAbilities v = [Type.AnnotatedType v Ann]
type SourceName = Text
type Source = Text
type LexedSource = (Text, [L.Token L.Lexeme])
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

type TypecheckingResult v =
  Result (Seq (Note v Ann)) (Maybe (UF.TypecheckedUnisonFile v Ann))

data Command m i v a where
  Eval :: m a -> Command m i v a

  Input :: Command m i v i

  -- Presents some output to the user
  Notify :: Output v -> Command m i v ()

  -- literally just write some terms and types .unison/{terms,types}
  AddDefsToCodebase :: UF.TypecheckedUnisonFile v Ann -> Command m i v ()

  -- the hash length needed to disambiguate any definition in the codebase
  CodebaseHashLength :: Command m i v Int

  ParseType :: Names -> LexedSource -> Command m i v (Either (Parser.Error v) (Type v Ann))

  -- Typecheck a unison file relative to a particular link.
  -- If we want to be able to resolve relative names (seems unnecessary,
  -- at least in M1), we can keep a map from Link to parent in memory.
  Typecheck :: AmbientAbilities v
            -> Names
            -> (Reference -> CT.ConstructorType)
            -> SourceName
            -> LexedSource
            -> Command m i v (TypecheckingResult v)

  -- Evaluate all watched expressions in a UnisonFile and return
  -- their results, keyed by the name of the watch variable. The tuple returned
  -- has the form:
  --   (hash, (ann, sourceTerm, evaluatedTerm, isCacheHit))
  --
  -- where
  --   `hash` is the hash of the original watch expression definition
  --   `ann` gives the location of the watch expression
  --   `sourceTerm` is a closed term (no free vars) for the watch expression
  --   `evaluatedTerm` is the result of evaluating that `sourceTerm`
  --   `isCacheHit` is True if the result was computed by just looking up
  --   in a cache
  --
  -- It's expected that the user of this action might add the
  -- `(hash, evaluatedTerm)` mapping to a cache to make future evaluations
  -- of the same watches instantaneous.

  Evaluate :: PPE.PrettyPrintEnv
           -> UF.TypecheckedUnisonFile v Ann
           -> Command m i v (Either Runtime.Error
                ([(v, Term v ())], Map v
                (Ann, UF.WatchKind, Reference, Term v (), Term v (), Runtime.IsCacheHit)))

  -- Evaluate a single closed definition
  Evaluate1 :: PPE.PrettyPrintEnv -> Term v Ann -> Command m i v (Either Runtime.Error (Term v Ann))

  -- Add a cached watch to the codebase
  PutWatch :: UF.WatchKind -> Reference.Id -> Term v Ann -> Command m i v ()

  -- Loads any cached watches of the given kind
  LoadWatches :: UF.WatchKind -> Set Reference -> Command m i v [(Reference, Term v Ann)]

  -- Loads a root branch from some codebase, returning `Nothing` if not found.
  -- Any definitions in the head of the requested root that aren't in the local
  -- codebase are copied there.
  LoadLocalRootBranch :: Command m i v (Branch m)

  LoadRemoteRootBranch ::
    RemoteRepo -> Command m i v (Either GitError (Branch m))

  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch m -> Command m i v ()

  SyncRemoteRootBranch ::
    RemoteRepo -> Branch m -> Command m i v (Either GitError ())
  -- e.g.
  --   /Lib/Arya/Public/SuperML> push github:aryairani/superML
  --   SynchRootBranch (Github "aryairani" "superML" "master")
  --                   (Branch at /Lib/Arya/Public/SuperML)

  LoadTerm :: Reference.Id -> Command m i v (Maybe (Term v Ann))

  LoadType :: Reference.Id -> Command m i v (Maybe (Decl v Ann))

  LoadTypeOfTerm :: Reference -> Command m i v (Maybe (Type v Ann))

  PutTerm :: Reference.Id -> Term v Ann -> Type v Ann -> Command m i v ()

  -- todo: eliminate these hopefully (why, again? because we can know from the Reference?)
  IsTerm :: Reference -> Command m i v Bool
  IsType :: Reference -> Command m i v Bool

  -- Get the immediate (not transitive) dependents of the given reference
  -- This might include historical definitions not in any current path; these
  -- should be filtered by the caller of this command if that's not desired.
  GetDependents :: Reference -> Command m i v (Set Reference)

  GetTermsOfType :: Type v Ann -> Command m i v (Set Referent)
  GetTermsMentioningType :: Type v Ann -> Command m i v (Set Referent)

  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: PPE.PrettyPrintEnv -> UF.TypecheckedUnisonFile v Ann -> Command m i v ()
