{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.A_TermEdit where

import GHC.Generics
import Data.Aeson.TH
import Data.Bytes.Serial
import Unison.A_Eval (Eval)
import qualified Unison.A_Eval as Eval
import qualified Unison.A_Term as Term
import qualified Unison.A_Hash as Hash
import qualified Unison.ABT as ABT

data Action
  = Abstract -- Turn target into function parameter
  | Step -- Link + beta reduce the target
  | Eta -- Eta reduce the target
  | LetFloat -- Float the target out to a let binding, as far as possible
  | WHNF -- Simplify target to weak head normal form
  | Noop -- Do nothing to the target
  deriving Generic

-- abstract :: Term.Term

instance Serial Action
deriveJSON defaultOptions ''Action
