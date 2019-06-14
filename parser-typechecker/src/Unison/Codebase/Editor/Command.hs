{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor.Command (Command(..), AmbientAbilities, SourceName, TypecheckingResult) where

import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import Data.Sequence (Seq)
import Data.Text (Text)


import           Unison.Codebase.Editor.Output
import           Unison.Codebase.Editor.RemoteRepo

import           Unison.Codebase.Branch2        ( Branch )
import           Unison.Codebase.GitError
import           Unison.Names2                  ( Names )
import           Unison.Parser                  ( Ann )
import           Unison.Reference               ( Reference )
import           Unison.Result                  ( Note
                                                , Result)
import           Unison.Typechecker.TypeLookup  ( Decl )

import qualified Unison.Codebase.Runtime       as Runtime
import qualified Unison.Reference              as Reference
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import qualified Unison.UnisonFile             as UF
import qualified Unison.PrettyPrintEnv         as PPE


type AmbientAbilities v = [Type.AnnotatedType v Ann]
type SourceName = Text
type Source = Text
type Term v a = Term.AnnotatedTerm v a
type Type v a = Type.AnnotatedType v a

type TypecheckingResult v =
  Result (Seq (Note v Ann))
         (PPE.PrettyPrintEnv, Maybe (UF.TypecheckedUnisonFile v Ann))

data Command m i v a where
  Eval :: m a -> Command m i v a

  Input :: Command m i v i

  -- Presents some output to the user
  Notify :: Output v -> Command m i v ()

  -- This will load the namespace from the provided link, and
  -- give warnings about name conflicts and the like.
  -- If there are no warnings, or if the `CollisionHandler` specifies to ignore
  -- them, then this also writes the supplied definitions to `terms/`, `types/`.
  -- It does not write any namespace stuff.  (Maybe it should?)
  -- It may complain if you are trying to write definitions into a remote link,
  -- and suggest that you can convert the link to a fork if you want.

  -- literally just write some terms and types .unison/{terms,types}
  AddDefsToCodebase :: UF.TypecheckedUnisonFile v Ann -> Command m i v ()
    -- want it to literally add terms, types to disk

    --  AddDefsToCodebase
    --    :: -- CollisionHandler -> (todo)
    --       Path
    --    -> UF.TypecheckedUnisonFile v Ann
    --    -> Command m i v (Branch (Command m i v), SlurpResult v)

  -- Arya: Do we need this?
  -- -- Load one level of a namespace.  It may involve reading from disk,
  -- -- or from http into a cache.
  -- GetBranch :: RepoLink Path -> Command m i v Branch

  -- Typecheck a unison file relative to a particular link.
  -- If we want to be able to resolve relative names (seems unnecessary,
  -- at least in M1), we can keep a map from Link to parent in memory.
  Typecheck :: AmbientAbilities v
            -> Names
            -> SourceName
            -> Source
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

  Evaluate :: UF.TypecheckedUnisonFile v Ann
           -> Command m i v ([(v, Term v ())], Map v
                (Ann, UF.WatchKind, Reference, Term v (), Term v (), Runtime.IsCacheHit))


  -- Loads a root branch from some codebase, returning `Nothing` if not found.
  -- Any definitions in the head of the requested root that aren't in the local
  -- codebase are copied there.
  LoadLocalRootBranch :: Command m i v (Branch m)

  LoadRemoteRootBranch ::
    RemoteRepo -> Command m i v (Either GitError (Branch m))

  -- RetrieveHashes repo types terms
  RetrieveHashes ::
    RemoteRepo -> Set Reference -> Set Reference -> Command m i v ()

  -- Syncs the Branch to some codebase and updates the head to the head of this causal.
  -- Any definitions in the head of the supplied branch that aren't in the target
  -- codebase are copied there.
  SyncLocalRootBranch :: Branch m -> Command m i v ()
  SyncRemoteRootBranch :: RemoteRepo -> Branch m -> Command m i v ()
  -- e.g.
  --   /Lib/Arya/Public/SuperML> push github:aryairani/superML
  --   SynchRootBranch (Github "aryairani" "superML" "master")
  --                   (Branch at /Lib/Arya/Public/SuperML)

  LoadTerm :: Reference.Id -> Command m i v (Maybe (Term v Ann))

  LoadType :: Reference.Id -> Command m i v (Maybe (Decl v Ann))

  LoadTypeOfTerm :: Reference -> Command m i v (Maybe (Type v Ann))

  -- Get the immediate (not transitive) dependents of the given reference
  -- This might include historical definitions not in any current path; these
  -- should be filtered by the caller of this command if that's not desired.
  GetDependents :: Reference -> Command m i v (Set Reference)

  -- Execute a UnisonFile for its IO effects
  -- todo: Execute should do some evaluation?
  Execute :: UF.TypecheckedUnisonFile v Ann -> Command m i v ()
