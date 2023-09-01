{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Builtin.Terms
  ( builtinTermsRef,
    builtinTermsSrc,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Unison.Builtin.Decls qualified as Decls
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Hashing.V2.Convert qualified as H
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var

-- This exists presumably only because constructors can't be metadata
builtinTermsSrc :: a -> [(Symbol, a, Term Symbol a, Type Symbol a)]
builtinTermsSrc ann =
  [ ( v "metadata.isPropagated",
      ann,
      Term.constructor ann (ConstructorReference Decls.isPropagatedRefId Decls.isPropagatedConstructorId),
      Type.refId ann Decls.isPropagatedRefId
    ),
    ( v "metadata.isTest",
      ann,
      Term.constructor ann (ConstructorReference Decls.isTestRefId Decls.isTestConstructorId),
      Type.refId ann Decls.isTestRefId
    )
  ]

v :: (Var v) => Text -> v
v = Var.named

builtinTermsRef :: Map Symbol Reference.Id
builtinTermsRef =
  fmap (\(refId, _, _, _) -> refId)
    . H.hashTermComponents
    . Map.fromList
    . fmap (\(v, _a, tm, tp) -> (v, (tm, tp, ())))
    $ builtinTermsSrc ()
