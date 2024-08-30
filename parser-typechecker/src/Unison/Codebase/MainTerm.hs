{-# LANGUAGE PartialTypeSignatures #-}

-- | Find a computation of type '{IO} () in the codebase.
module Unison.Codebase.MainTerm where

import Data.List.NonEmpty qualified as NEList
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Unison.Builtin.Decls qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Parser.Ann
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent qualified as Referent
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Var (Var)
import Unison.Var qualified as Var

data MainTerm v
  = NotFound (HQ.HashQualified Name)
  | BadType (HQ.HashQualified Name) (Maybe (Type v Ann))
  | Success (HQ.HashQualified Name) (Term v Ann) (Type v Ann)

getMainTerm ::
  (Monad m, Var v) =>
  (Reference -> m (Maybe (Type v Ann))) ->
  Names.Names ->
  HQ.HashQualified Name ->
  Type.Type v Ann ->
  m (MainTerm v)
getMainTerm loadTypeOfTerm parseNames mainName mainType = do
  let refs = Names.lookupHQTerm Names.IncludeSuffixes mainName parseNames
  let a = Parser.Ann.External
  case toList refs of
    [] -> pure (NotFound mainName)
    [Referent.Ref ref] -> do
      typ <- loadTypeOfTerm ref
      case typ of
        Just typ ->
          if Typechecker.fitsScheme typ mainType
            then do
              let tm = DD.forceTerm a a (Term.ref a ref)
              return (Success mainName tm typ)
            else pure (BadType mainName $ Just typ)
        _ -> pure (BadType mainName Nothing)
    _ -> pure (error "multiple matching refs") -- TODO: make a real exception

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
