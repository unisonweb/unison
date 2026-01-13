{-# LANGUAGE PartialTypeSignatures #-}

-- | Find a computation of type '{IO} () in the codebase.
module Unison.Codebase.MainTerm
  ( MainTerm (..),
    getMainTerm,
    builtinIOTestTypes,
    builtinMain,
    builtinMainWithResultType,
  )
where

import Control.Lens (mapped, _1)
import Data.List.NonEmpty qualified as NEList
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Unison.Builtin.Decls qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Parser.Ann
import Unison.Prelude
import Unison.Reference (Reference, TermReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Util.Relation qualified as Relation
import Unison.Var (Var)
import Unison.Var qualified as Var

data MainTerm v
  = -- No terms were found with this name
    NotFound
  | -- 1 or more terms were found with this name, but none of them have the right type
    -- Invariant: list is not empty
    BadType [(HQ.HashQualified Name, TermReference, Type v Ann)]
  | -- 2 or more terms were found with this name, and of those, 2 or more have the right type
    -- Invariant: length of list is >= 2
    Ambiguous [(HQ.HashQualified Name, TermReference, Type v Ann)]
  | -- 1 or more terms were found with this name, and exactly 1 has the right type
    Success (HQ.HashQualified Name) TermReference (Term v Ann) (Type v Ann)

getMainTerm ::
  (Monad m, Var v) =>
  (Reference -> m (Maybe (Type v Ann))) ->
  Names.Names ->
  HQ.HashQualified Name ->
  Type.Type v Ann ->
  m (MainTerm v)
getMainTerm loadTypeOfTerm parseNames mainName mainType = do
  -- Get all terms and constructors referred to by that name
  let allReferents :: [(Name, Referent)]
      allReferents =
        Relation.toList $
          HQ.filterBySuffix
            Referent.toShortHash
            mainName
            (Names.terms parseNames)

  -- Keep only the terms (throwing away constructors)
  allTermReferences :: [(Name, TermReference, Type v Ann)] <-
    allReferents & mapMaybeM \case
      (name, Referent.Ref ref) -> do
        loadTypeOfTerm ref <&> \case
          Just ty -> Just (name, ref, ty)
          -- this shouldn't really happen
          Nothing -> Nothing
      _ -> pure Nothing

  -- Keep only the terms that are of the right 'main' type
  let allTermReferencesThatCouldBeRun :: [(Name, TermReference, Type v Ann)]
      allTermReferencesThatCouldBeRun =
        filter
          (\(_, _, ty) -> Typechecker.fitsScheme ty mainType)
          allTermReferences

  pure case allTermReferencesThatCouldBeRun of
    [(name, ref, ty)] ->
      let a = Parser.Ann.External
          tm = DD.forceTerm a a (Term.ref a ref)
       in Success (mainName $> name) ref tm ty
    [] ->
      case allTermReferences of
        [] -> NotFound
        _ -> BadType (over (mapped . _1) (mainName $>) allTermReferences)
    _ -> Ambiguous (over (mapped . _1) (mainName $>) allTermReferencesThatCouldBeRun)

-- forall x. '{ io2.IO, Exception } x
builtinMain :: (Var v) => a -> Type.Type v a
builtinMain a =
  let result = Var.named "result"
   in Type.forAll a result (builtinMainWithResultType a (Type.var a result))

-- '{io2.IO, Exception} res
builtinMainWithResultType :: (Var v) => a -> Type.Type v a -> Type.Type v a
builtinMainWithResultType a res = Type.arrow a (Type.ref a DD.unitRef) io
  where
    io = Type.effect a [Type.builtinIO a, DD.exceptionType a] res

-- | All possible IO'ish test types, e.g.
-- '{IO, Exception} [Result]
-- '{IO} [Result]
builtinIOTestTypes :: forall v a. (Ord v, Var v) => a -> NESet (Type.Type v a)
builtinIOTestTypes a =
  NESet.fromList
    ( delayedResultWithEffects ([Type.builtinIO a, DD.exceptionType a])
        NEList.:| [delayedResultWithEffects ([Type.builtinIO a])]
    )
  where
    delayed = Type.arrow a (Type.ref a DD.unitRef)
    delayedResultWithEffects es = delayed (Type.effect a es (DD.testResultListType a))
