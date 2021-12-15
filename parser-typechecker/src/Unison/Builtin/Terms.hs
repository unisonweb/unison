{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Builtin.Terms where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Unison.Builtin.Decls as Decls
import Unison.ConstructorReference (GConstructorReference(..))
import qualified Unison.Hashing.V2.Convert as H
import qualified Unison.Reference as Reference
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Var (Var)
import qualified Unison.Var as Var

builtinTermsSrc :: Var v => a -> [(v, Term v a, Type v a)]
builtinTermsSrc a =
  [ ( v "metadata.isPropagated",
      Term.constructor a (ConstructorReference Decls.isPropagatedRef Decls.isPropagatedConstructorId),
      Type.ref a Decls.isPropagatedRef
    ),
    ( v "metadata.isTest",
      Term.constructor a (ConstructorReference Decls.isTestRef Decls.isTestConstructorId),
      Type.ref a Decls.isTestRef
    )
  ]

v :: Var v => Text -> v
v = Var.named

builtinTermsRef :: Var v => a -> Map v Reference.Id
builtinTermsRef a =
  fmap fst
    . H.hashTermComponents
    . Map.fromList
    . fmap (\(v, tm, _tp) -> (v, tm))
    $ builtinTermsSrc a
