module Unison.Codebase.Branch where

import Data.Map (Map)
import Unison.Codebase.Causal (Causal)
import Unison.Codebase.Conflicted (Conflicted)
import Unison.Codebase.Name (Name)
import Unison.Codebase.NameEdit (NameEdit)
import Unison.Codebase.TermEdit (TermEdit)
import Unison.Codebase.TypeEdit (TypeEdit)
import Unison.Reference (Reference)

data Branch v a =
  Branch { namespace     :: Map Name (Causal NameEdit)
         , edited        :: Map Reference (Causal (Conflicted TermEdit))
         , editedDatas   :: Map Reference (Causal (Conflicted TypeEdit))
         , editedEffects :: Map Reference (Causal (Conflicted TypeEdit))
         }

