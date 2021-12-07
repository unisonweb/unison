module Unison.Sqlite.Sql
  ( Sql (..),
  )
where

import Unison.Prelude

-- | A SQL snippet.
newtype Sql
  = Sql Text
  deriving newtype (IsString, Show)
