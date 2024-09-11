{-# LANGUAGE QuasiQuotes #-}

module Unison.Sqlite.DataVersion
  ( DataVersion (..),
  )
where

import Unison.Prelude

newtype DataVersion
  = DataVersion Int64
  deriving stock (Eq)
  deriving newtype (Show)
