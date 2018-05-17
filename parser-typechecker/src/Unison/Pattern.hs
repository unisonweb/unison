{-# Language DeriveGeneric #-}

module Unison.Pattern where

import Data.Text (Text)
import GHC.Generics
import Unison.Literal (Literal)
import Unison.Reference (Reference)
import qualified Unison.Hashable as H

data Pattern
  = Wildcard !Text -- added Text just for comment / annotation
  | Var
  | Literal !Literal
  | Constructor !Reference !Int [Pattern] deriving (Generic,Eq,Show)

instance H.Hashable Pattern where
  tokens (Wildcard _) = [H.Tag 0]
  tokens Var = [H.Tag 1]
  tokens (Literal l) = H.Tag 2 : H.tokens l
  tokens (Constructor r n args) = [H.Tag 3, H.accumulateToken r, H.VarInt n, H.accumulateToken args]

