{-# LANGUAGE TemplateHaskell #-}

module Unison.Sqlite.Sql
  ( Sql (..),
    sql,
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter (quoteExp))
import qualified Language.Haskell.TH.Syntax as TH
import qualified NeatInterpolation
import Unison.Prelude

-- | A SQL snippet.
newtype Sql
  = Sql Text
  deriving newtype (IsString, Monoid, Semigroup, Show)

-- | A quasi-quoter that produces expressions of type 'Sql'.
sql :: QuasiQuoter
sql =
  NeatInterpolation.trimming
    { quoteExp =
        \string -> do
          text <- quoteExp NeatInterpolation.trimming string
          pure (TH.AppE (TH.ConE 'Sql) text)
    }
