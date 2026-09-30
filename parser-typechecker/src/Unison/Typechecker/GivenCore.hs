-- | Validate explicit dictionary passing before accepting elaborated bindings.
module Unison.Typechecker.GivenCore (lowerType, verifyBindings) where

import Unison.ABT qualified as ABT
import Unison.Codebase.BuiltinAnnotation (BuiltinAnnotation)
import Unison.DataDeclaration qualified as DD
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Typechecker.Variance qualified as Variance
import Unison.Var (Var)

-- | At runtime every dictionary is an ordinary positional argument.
lowerType :: (Ord v) => Type v loc -> Type v loc
lowerType = ABT.transform \case
  Type.ImplicitArrow i o -> Type.Arrow i o
  f -> f

-- | Check the rewritten bodies against their claimed stored signatures.
-- Checking the whole file recomputes components after inserted dictionary
-- references have changed its dependency graph. Returned info notes contain
-- these components; callers must not reuse the pre-elaboration components.
verifyBindings ::
  (Var v, BuiltinAnnotation loc, Ord loc, Show loc, Monoid loc) =>
  PrettyPrintEnv ->
  [Type v loc] ->
  TL.TypeLookup v loc ->
  [(v, loc, Term v loc, Type v loc)] ->
  Context.Result v loc ()
verifyBindings ppe abilities lookup bindings =
  ()
    <$ Context.synthesizeClosed
      ppe
      Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled
      (Variance.fromTypeLookup loweredLookup)
      (TypeVar.liftType . lowerType <$> abilities)
      loweredLookup
      (TypeVar.liftTerm (Term.letRec' True checkedBindings (Term.nat mempty 0)))
  where
    checkedBindings = [(v, loc, Term.ann loc (Term.typeMap lowerType body) (lowerType typ)) | (v, loc, body, typ) <- bindings]
    loweredLookup =
      TL.TypeLookup
        (lowerType <$> TL.typeOfTerms lookup)
        (lowerDecl <$> TL.dataDecls lookup)
        (DD.EffectDeclaration . lowerDecl . DD.toDataDecl <$> TL.effectDecls lookup)
    lowerDecl declaration = declaration {DD.constructors' = [(loc, v, lowerType typ) | (loc, v, typ) <- DD.constructors' declaration]}
