{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Find a computation of type '{IO} () in the codebase.
module Unison.Codebase.MainTerm where

import Unison.Prelude

import           Unison.Parser                  ( Ann )
import qualified Unison.Parser                 as Parser
import qualified Unison.Term                   as Term
import           Unison.Term                    ( Term )
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
  | BadType String
  | Success HQ.HashQualified (Term v Ann) (Type v Ann)

getMainTerm
  :: (Monad m, Var v)
  => (Reference -> m (Maybe (Type v Ann)))
  -> Names3.Names0
  -> String
  -> m (MainTerm v)
getMainTerm loadTypeOfTerm parseNames0 mainName =
  case HQ.fromString mainName of
    Nothing -> pure (NotAFunctionName mainName)
    Just hq -> do
      let refs = Names3.lookupHQTerm hq (Names3.Names parseNames0 mempty)
      let a = Parser.External
      case toList refs of
        [Referent.Ref ref] -> do
          typ <- loadTypeOfTerm ref
          case typ of
            Just typ | Typechecker.isSubtype typ (nullaryMain a) -> do
              let tm = DD.forceTerm a a (Term.ref a ref)
              return (Success hq tm typ)
            _ -> pure (BadType mainName)
        _ -> pure (NotFound mainName)

-- {IO} ()
ioUnit :: Ord v => a -> Type.Type v a
ioUnit a = Type.effect a [Type.ref a ioReference] (Type.ref a DD.unitRef)

-- '{IO} ()
nullaryMain :: Ord v => a -> Type.Type v a
nullaryMain a = Type.arrow a (Type.ref a DD.unitRef) (ioUnit a)

mainTypes :: Ord v => a -> [Type v a]
mainTypes a = [nullaryMain a]
