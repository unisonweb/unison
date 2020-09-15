{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.V1.Symbol where

import Data.Word (Word64)
import Data.Text (Text)

data Symbol = Symbol !Word64 !Text deriving (Eq, Ord)

-- instance Show Symbol where
--   show (Symbol 0 n) = show n
--   show (Symbol id n) = show n ++ "-" ++ show id
