{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Find a computation of type '{IO} () in the codebase.
module Unison.Codebase.MainTerm where

import qualified Unison.Builtin.Decls as DD
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Names as Names
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Parser.Ann
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Referent as Referent
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker as Typechecker
import Unison.Var (Var)
import qualified Unison.Var as Var

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
      let refs = NamesWithHistory.lookupHQTerm hq (NamesWithHistory.NamesWithHistory parseNames mempty)
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
builtinMain :: Var v => a -> Type.Type v a
builtinMain a =
  let x = Var.named "x"
   in Type.forall a x (builtinMain' a (Type.var a x))

-- '{io2.IO, Exception} res
builtinMain' :: Var v => a -> Type.Type v a -> Type.Type v a
builtinMain' a res = Type.arrow a (Type.ref a DD.unitRef) io
  where
    io = Type.effect a [Type.builtinIO a, DD.exceptionType a] res

-- [Result]
resultArr :: Ord v => a -> Type.Type v a
resultArr a = Type.app a (Type.ref a Type.listRef) (Type.ref a DD.testResultRef)

builtinResultArr :: Ord v => a -> Type.Type v a
builtinResultArr a = Type.effect a [Type.builtinIO a, DD.exceptionType a] (resultArr a)

-- '{io2.IO} [Result]
builtinTest :: Ord v => a -> Type.Type v a
builtinTest a =
  Type.arrow a (Type.ref a DD.unitRef) (builtinResultArr a)
