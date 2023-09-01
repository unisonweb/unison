module Unison.Hashing.V2.Referent
  ( Referent (..),
  )
where

import Unison.Hashing.V2.ConstructorId (ConstructorId)
import Unison.Hashing.V2.Reference (Reference (ReferenceDerivedId), ReferenceId)
import Unison.Hashing.V2.Tokenizable (Tokenizable)
import Unison.Hashing.V2.Tokenizable qualified as H

data Referent
  = ReferentRef Reference
  | ReferentCon ReferenceId ConstructorId
  deriving stock (Show, Ord, Eq)

instance Tokenizable Referent where
  tokens (ReferentRef r) = [H.Tag 0] ++ H.tokens r
  tokens (ReferentCon r i) = [H.Tag 2] ++ H.tokens (ReferenceDerivedId r) ++ H.tokens i
