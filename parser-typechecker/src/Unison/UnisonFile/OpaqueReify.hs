-- | Validation of @reify@ body fns in opaque type declarations.
--
-- An opaque type @T α₁..αₙ@ may declare a body fn named @reify@ whose role is
-- to reconstruct a renderable form of a value of type @T@. If present, its
-- type must match the canonical shape
--
-- > forall α₁..αₙ. T α₁..αₙ ->{} '(T α₁..αₙ)
--
-- This module exposes:
--
-- * 'expectedReifyType' — build the canonical type for an opaque decl.
-- * 'validateOpaqueReifySignatures' — scan a typechecked file's opaque decls
--   for body fns named @reify@ and verify their inferred types match.
module Unison.UnisonFile.OpaqueReify
  ( expectedReifyType,
    validateOpaqueReifySignatures,
  )
where

import Data.Map qualified as Map
import Unison.Builtin.Decls qualified as DD
import Unison.Name qualified as Name
import Unison.Syntax.Name qualified as Name (toVar, unsafeParseVar)
import Unison.OpaqueDeclaration (OpaqueDeclaration)
import Unison.OpaqueDeclaration qualified as OpaqueDeclaration
import Unison.Reference (TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile.Type (TypecheckedUnisonFile (..))
import Unison.Var (Var)
import Unison.Var qualified as Var

-- | The canonical type for an opaque @T@'s @reify@ body fn:
--
-- > forall α₁..αₙ. T α₁..αₙ ->{} '(T α₁..αₙ)
--
-- All type variables are quantified at the outside, and the function arrow
-- carries the empty effect set @{}@; the result is a thunk @'(T α*)@ which
-- desugars to @() ->{} T α*@.
expectedReifyType ::
  forall v a.
  (Var v, Monoid a) =>
  a ->
  -- | The opaque type's own reference (the LHS).
  TypeReferenceId ->
  OpaqueDeclaration v a ->
  Type v a
expectedReifyType loc refId od =
  Type.foralls loc params $
    Type.arrow loc tApplied $
      Type.effect1 loc (Type.effects loc []) thunk
  where
    params :: [v]
    params = OpaqueDeclaration.paramNames od
    tRef :: Type v a
    tRef = Type.ref loc (Reference.DerivedId refId)
    tApplied :: Type v a
    tApplied = Type.apps tRef [(loc, Type.var loc p) | p <- params]
    -- '(T α*) desugars to () ->{} (T α*)
    thunk :: Type v a
    thunk =
      Type.arrow loc (DD.thunkArgType loc) $
        Type.effect1 loc (Type.effects loc []) tApplied

-- | For each opaque declaration in the typechecked file, look up any body fn
-- whose unqualified name is exactly @reify@. If found and its inferred type
-- does not match 'expectedReifyType', return a list of mismatches: tuples of
-- the body-fn name, its location, the inferred type, and the expected type.
--
-- An absent @reify@ is fine (rendering falls back to abstract). The check
-- is structural-equivalence up to alpha-equivalence as implemented by
-- 'Typechecker.isEqual'.
validateOpaqueReifySignatures ::
  forall v a.
  (Var v, Monoid a) =>
  TypecheckedUnisonFile v a ->
  [(v, a, Type v a, Type v a)]
validateOpaqueReifySignatures
  TypecheckedUnisonFileId
    { opaqueDeclarationsId',
      hashTermsId
    } =
    do
      (opaqueName, (refId, od)) <- Map.toList opaqueDeclarationsId'
      let reifyFqn :: v
          reifyFqn = qualified opaqueName (Var.named "reify")
          expected = expectedReifyType (OpaqueDeclaration.annotation od) refId od
      case Map.lookup reifyFqn hashTermsId of
        Nothing -> []
        Just (loc, _refId, _wk, _tm, found) ->
          if Typechecker.isEqual found expected
            then []
            else [(reifyFqn, loc, found, expected)]
    where
      -- Build the qualified body-fn name @T.name@, mirroring how the file
      -- parser builds these via 'Unison.Syntax.Var.namespaced2'. We do it
      -- inline here to avoid pulling in 'Unison.Syntax.Var' at the
      -- typechecking layer.
      qualified :: v -> v -> v
      qualified parent child =
        Name.toVar $
          Name.joinDot
            (Name.unsafeParseVar parent)
            (Name.unsafeParseVar child)
