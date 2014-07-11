module Unison.Term where

import Unison.Hash (..)
import Unison.Var (I)
import Unison.Type as T

data Literal
  = N Float
  | S String 
  | V [Term]

data Term
  = Var I
  | Lit Literal
  | Con Hash
  | Ref Hash
  | App Term Term
  | Type Term T.Type
  | Lam I Term
