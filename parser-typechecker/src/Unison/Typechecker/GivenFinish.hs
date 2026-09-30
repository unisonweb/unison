-- | Accept elaboration only after explicit dictionary passing checks as core.
module Unison.Typechecker.GivenFinish
  ( VerifiedBindings,
    components,
    FinishError (..),
    finish,
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation)
import Unison.Parser.Ann (Ann, SiteId)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenApply qualified as Apply
import Unison.Typechecker.GivenCore qualified as Core
import Unison.Typechecker.GivenPlan qualified as Plan
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.GivenSites qualified as Sites
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Var (Var)

newtype VerifiedBindings v = VerifiedBindings [[(v, Term v Ann, Type v Ann)]]

-- | Dependency groups recomputed after insertion, with qualified signatures.
components :: VerifiedBindings v -> [[(v, Term v Ann, Type v Ann)]]
components (VerifiedBindings result) = result

data FinishError v
  = PlanFailure (Plan.PlanError v)
  | InsertionFailure Apply.ApplyError
  | ExpectedFileBindings
  | DuplicateBinding v
  | DuplicateSignature v
  | SignatureMismatch
  | CoreTypeErrors [Context.ErrorNote v Ann]
  | CoreCompilerBug (Context.CompilerBug v Ann) [Context.ErrorNote v Ann]
  | CoreComponentMismatch
  deriving stock (Show)

finish ::
  (Var v, BuiltinAnnotation Ann) =>
  PrettyPrintEnv ->
  [Type v Ann] ->
  TL.TypeLookup v Ann ->
  Sites.PreparedTerm v ->
  Set SiteId ->
  [Given.Given v Ann] ->
  [Context.InfoNote v Ann] ->
  Either (FinishError v) (VerifiedBindings v)
finish ppe abilities lookup prepared marked ambient notes = do
  edits <- first PlanFailure (Plan.planWithVisibleGivens (Sites.visibleGivens prepared marked) ambient notes)
  rewritten <- first InsertionFailure (Apply.apply edits (Sites.renameLocals prepared))
  bindings <- case rewritten of
    Term.LetRecNamedAnnotatedTop' True _ bindings _ -> pure bindings
    _ -> Left ExpectedFileBindings
  bodies <- foldM (\m ((loc, v), body) -> insertUnique DuplicateBinding v (loc, body) m) Map.empty bindings
  signatures <- foldM (\m (v, typ, _) -> insertUnique DuplicateSignature v typ m) Map.empty [entry | Context.TopLevelComponent entries <- notes, entry <- entries]
  unless (Map.keysSet bodies == Map.keysSet signatures) (Left SignatureMismatch)
  let checked = [(v, loc, body, signatures Map.! v) | (v, (loc, body)) <- Map.toList bodies]
  case Core.verifyBindings ppe abilities lookup checked of
    Context.TypeError errors _ -> Left (CoreTypeErrors (toList errors))
    Context.CompilerBug bug errors _ -> Left (CoreCompilerBug bug (toList errors))
    Context.Success checkedNotes () -> do
      let groups = [[v | (v, _, _) <- entries] | Context.TopLevelComponent entries <- toList checkedNotes]
          names = concat groups
      unless (Set.fromList names == Map.keysSet bodies && length names == Map.size bodies) (Left CoreComponentMismatch)
      pure (VerifiedBindings [[(v, snd (bodies Map.! v), signatures Map.! v) | v <- group] | group <- groups])
  where
    insertUnique failure key value m
      | Map.member key m = Left (failure key)
      | otherwise = Right (Map.insert key value m)
