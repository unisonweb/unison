module Unison.PatternMatchCoverage.EffectHandler where

import Unison.ConstructorReference (ConstructorReference)
import Unison.PatternMatchCoverage.Pretty
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Util.Pretty

data EffectHandler
  = NoEffect
  | Effect ConstructorReference
  deriving stock (Show, Eq, Ord, Generic)

prettyEffectHandler :: PrettyPrintEnv -> EffectHandler -> Pretty ColorText
prettyEffectHandler ppe = \case
  NoEffect -> "pure"
  Effect cr -> prettyConstructorReference ppe cr
