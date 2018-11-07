module Unison.Runtime.IR where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Unison.Reference as R
import Unison.Symbol (Symbol)
import Unison.Term (AnnotatedTerm)
import Data.Vector (Vector)

type Pos = Int
type Arity = Int
type ConstructorId = Int
type Term v = AnnotatedTerm v ()

-- Values, in normal form
data V
  = I Int64 | F Double | N Word64 | B Bool | T Text
  | Lam Arity (Either R.Reference (Term Symbol)) IR
  | Data R.Reference ConstructorId [V]
  | Sequence (Vector V)
  | Requested Req
  deriving (Eq,Show)

data Pattern
  = PatternI Int64 | PatternF Double | PatternN Word64 | PatternB Bool | PatternT Text
  | PatternData R.Reference ConstructorId [Pattern]
  | PatternSequence (Vector Pattern)
  | PatternBind R.Reference ConstructorId [Pattern] Pattern
  | PatternIgnore
  | PatternVar deriving (Eq,Show)

-- Computations, need to be reduced to values
data IR
  = Var Pos
  | AddI Pos Pos | SubI Pos Pos | MultI Pos Pos | DivI Pos Pos
  | AddN Pos Pos | SubN Pos Pos | MultN Pos Pos | DivN Pos Pos
  | AddF Pos Pos | SubF Pos Pos | MultF Pos Pos | DivF Pos Pos
  | Let IR IR
  | LetRec [IR] IR
  | MakeSequence [Pos]
  | V V
  | DynamicApply Pos [Pos] -- call to unknown function
  | Construct R.Reference ConstructorId [Pos]
  | Request R.Reference ConstructorId [Pos]
  | Handle Pos IR
  | If Pos IR IR
  | And Pos IR
  | Or Pos IR
  | Match Pos [(Pattern, Maybe IR, IR)] -- pattern, optional guard, rhs
  deriving (Eq,Show)

-- Contains the effect ref and ctor id, the args, and the continuation
-- which expects the result at the top of the stack
data Req
  = Req R.Reference ConstructorId [V] IR
  deriving (Eq,Show)

