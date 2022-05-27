module Unison.Share.Types
  (CodeserverHost(..)) where

import Data.Aeson
import Unison.Prelude

-- | The hostname of a server we may authenticate with,
-- e.g. @CodeserverHost "enlil.unison-lang.org"@
newtype CodeserverHost = CodeserverHost Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
