{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.A_TermEdit where

import Control.Applicative
import GHC.Generics
import Data.Aeson.TH
import Data.Bytes.Serial
import Unison.A_Eval (Eval)
import Unison.A_Hash (Hash)
import Unison.Note (Noted)
import qualified Unison.A_Eval as Eval
import qualified Unison.A_Term as Term
import qualified Unison.A_Hash as Hash
import qualified Unison.ABT as ABT

-- f {42} x ==> f {(y -> y) 42} x
-- f {(y -> y) 42} x ==> {(y -> f y x) 42 }
-- f {42} x ==> f {let y = 42 in y} x
-- f {let y = 42 in y} x ==> {let y = 42 in f y x}
--
data Action
  = Abstract -- Turn target into function parameter
  | AbstractLet -- Turn target into let bound expression
  | MergeLet -- Merge a let block into its parent let block
  | SwapDown -- Swap the target let binding with the subsequent binding
  | SwapUp -- Swap the target let binding with the previous binding
  | Inline -- Delete a let binding by inlining its definition into usage sites
  | FloatOut -- Float the target binding out one level
  | Step -- Link + beta reduce the target
  | Eta -- Eta reduce the target
  | WHNF -- Simplify target to weak head normal form
  | Noop -- Do nothing to the target
  deriving Generic

-- | Interpret the given 'Action'
interpret :: (Applicative f, Monad f)
          => Eval (Noted f)
          -> (Hash -> Noted f Term.Term)
          -> Term.Path -> Action -> Term.Term -> Noted f (Maybe (Term.Path, Term.Term))
interpret eval link path action t = fail "TermEdit.interpret todo"

abstract :: Term.Path -> Term.Term -> Maybe (Term.Path, Term.Term)
abstract path t = Nothing -- case Term.at path t of

instance Serial Action
deriveJSON defaultOptions ''Action
