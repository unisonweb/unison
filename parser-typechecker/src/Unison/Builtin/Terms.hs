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

builtinTermsSrc :: a -> [(Symbol, a, Term Symbol a, Type Symbol a)]
builtinTermsSrc ann =
  [ ( v "metadata.isPropagated",
      ann,
      Term.constructor ann (ConstructorReference Decls.isPropagatedRef Decls.isPropagatedConstructorId),
      Type.ref ann Decls.isPropagatedRef
    ),
    ( v "metadata.isTest",
      ann,
      Term.constructor ann (ConstructorReference Decls.isTestRef Decls.isTestConstructorId),
      Type.ref ann Decls.isTestRef
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
