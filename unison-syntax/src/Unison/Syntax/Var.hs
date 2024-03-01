module Unison.Syntax.Var
  ( namespaced,
  )
where

import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Unison.Name qualified as Name
import Unison.Prelude
import Unison.Syntax.Name qualified as Name
import Unison.Var (Var)

namespaced :: (Var v) => List.NonEmpty v -> v
namespaced (v :| vs) =
  Name.toVar (foldl' Name.joinDot (Name.unsafeParseVar v) (map Name.unsafeParseVar vs))
