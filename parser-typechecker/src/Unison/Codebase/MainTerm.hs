{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Find a computation of type '{IO} () in the codebase.
module Unison.Codebase.MainTerm where

import Unison.Prelude

import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import qualified Unison.Term                   as Term
import           Unison.Term                    ( Term )
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )
import qualified Unison.Builtin.Decls          as DD
import qualified Unison.HashQualified          as HQ
import qualified Unison.Referent               as Referent
import qualified Unison.Names3                 as Names3
import           Unison.Reference               ( Reference )
import qualified Unison.Type                   as Type
import           Unison.Type                    ( Type )
import qualified Unison.Typechecker as Typechecker
import           Unison.Runtime.IOSource        ( ioReference )

data MainTerm v
  = NotAFunctionName String
  | NotFound String
  | BadType String (Maybe (Type v Ann))
  | Success HQ.HashQualified (Term v Ann) (Type v Ann)

getMainTerm
  :: (Monad m, Var v)
  => (Reference -> m (Maybe (Type v Ann)))
  -> Names3.Names0
  -> String
  -> Type.Type v Ann
  -> m (MainTerm v)
getMainTerm loadTypeOfTerm parseNames0 mainName mainType =
  case HQ.fromString mainName of
    Nothing -> pure (NotAFunctionName mainName)
    Just hq -> do
      traceShowM hq
      let refs = Names3.lookupHQTerm hq (Names3.Names parseNames0 mempty)
      let a = Parser.External
      case toList refs of
        [Referent.Ref ref] -> do
          typ <- loadTypeOfTerm ref
          traceShowM typ
          case typ of
            Just typ ->
              if Typechecker.isSubtype mainType typ then do
                let tm = DD.forceTerm a a (Term.ref a ref)
                return (Success hq tm typ)
              else pure (BadType mainName $ Just typ)
            _ -> pure (BadType mainName Nothing)
        _ -> pure (NotFound mainName)

-- forall a. '{IO} a
nullaryMain :: Var v => a -> Type.Type v a
nullaryMain a =
  Type.forall a v $ Type.arrow a (Type.ref a DD.unitRef) (io a)
  where io a = Type.effect a [Type.ref a ioReference] (Type.var a v)
        v = Var.named "a"


-- forall a. '{io2.IO} a
builtinMain :: Var v => a -> Type.Type v a
builtinMain a =
  Type.forall a v $ Type.arrow a (Type.ref a DD.unitRef) (io a)
  where io a = Type.effect1 a (Type.builtinIO a) (Type.var a v)
        v = Var.named "a"

-- [Result]
resultArr :: Ord v => a -> Type.Type v a
resultArr a = Type.app a (Type.ref a Type.vectorRef) (Type.ref a DD.testResultRef)

-- {IO} [Result]
ioResultArr :: Ord v => a -> Type.Type v a
ioResultArr a = Type.effect1 a (Type.ref a ioReference) (resultArr a)

builtinResultArr :: Ord v => a -> Type.Type v a
builtinResultArr a = Type.effect1 a (Type.builtinIO a) (resultArr a)

-- '{IO} [Result]
nullaryTest :: Ord v => a -> Type.Type v a
nullaryTest a
  = Type.arrow a (Type.ref a DD.unitRef) (ioResultArr a)

-- '{io2.IO} [Result]
builtinTest :: Ord v => a -> Type.Type v a
builtinTest a
  = Type.arrow a (Type.ref a DD.unitRef) (builtinResultArr a)

mainTypes :: Var v => a -> [Type v a]
mainTypes a = [nullaryMain a]
