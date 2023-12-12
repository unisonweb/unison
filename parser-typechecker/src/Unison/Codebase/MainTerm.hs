{-# LANGUAGE PartialTypeSignatures #-}

-- | Find a computation of type '{IO} () in the codebase.
module Unison.Codebase.MainTerm where

import Unison.Builtin.Decls qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Parser.Ann qualified as Parser.Ann
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HQ (fromString)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Var (Var)
import Unison.Var qualified as Var

data MainTerm v
  = NotAFunctionName String
  | NotFound String
  | BadType String (Maybe (Type v Ann))
  | Success (HQ.HashQualified Name) (Term v Ann) (Type v Ann)

getMainTerm ::
  (Monad m, Var v) =>
  (Reference -> m (Maybe (Type v Ann))) ->
  Names.Names ->
  String ->
  Type.Type v Ann ->
  m (MainTerm v)
getMainTerm loadTypeOfTerm parseNames mainName mainType =
  case HQ.fromString mainName of
    Nothing -> pure (NotAFunctionName mainName)
    Just hq -> do
      let refs = NamesWithHistory.lookupHQTerm NamesWithHistory.IncludeSuffixes hq (NamesWithHistory.NamesWithHistory parseNames mempty)
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
                  return (Success hq tm typ)
                else pure (BadType mainName $ Just typ)
            _ -> pure (BadType mainName Nothing)
        _ -> pure (error "multiple matching refs") -- TODO: make a real exception

-- forall x. '{ io2.IO, Exception } x
builtinMain :: (Var v) => a -> Type.Type v a
builtinMain a =
  let result = Var.named "result"
   in Type.forall a result (builtinMainWithResultType a (Type.var a result))

-- '{io2.IO, Exception} res
builtinMainWithResultType :: (Var v) => a -> Type.Type v a -> Type.Type v a
builtinMainWithResultType a res = Type.arrow a (Type.ref a DD.unitRef) io
  where
    io = Type.effect a [Type.builtinIO a, DD.exceptionType a] res

builtinResultArr :: (Ord v) => a -> Type.Type v a
builtinResultArr a = Type.effect a [Type.builtinIO a, DD.exceptionType a] (DD.testResultType a)

-- '{io2.IO} [Result]
builtinTest :: (Ord v) => a -> Type.Type v a
builtinTest a =
  Type.arrow a (Type.ref a DD.unitRef) (builtinResultArr a)
