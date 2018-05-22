{-# Language DeriveGeneric #-}

module Unison.Pattern where

import Data.Text (Text)
import Data.Int (Int64)
import Data.Word (Word64)
import GHC.Generics
import Unison.Reference (Reference)
import qualified Unison.Hashable as H

data Pattern
  = Unbound
  | Var
  | Boolean !Bool
  | Int64 !Int64
  | UInt64 !Word64
  | Float !Double
  | Constructor !Reference !Int [Pattern] deriving (Generic,Eq,Show)

instance H.Hashable Pattern where
  tokens Unbound = [H.Tag 0]
  tokens Var = [H.Tag 1]
  tokens (Boolean b) = H.Tag 2 : error "need to figure out hashable"
  tokens (Int64 i) = H.Tag 3 : error "need to figure out hashable"
  tokens (UInt64 u) = H.Tag 4 : error "need to figure out hashable"
  tokens (Float f) = H.Tag 5 : H.tokens f
  tokens (Constructor r n args) = [H.Tag 6, H.accumulateToken r, H.VarInt n, H.accumulateToken args]
