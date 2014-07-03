module Unison.Node.Store.Memory where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Map as M
import Data.Set as S
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)
import Unison.Node.Metadata (Metadata)
import Unison.Node.Store
import Unison.Note (Noted)

data StoreState = StoreState {
  terms :: Map Hash Term,
  types :: Map Hash Type,
  metadata :: Map Hash (Metadata Hash)
}

{-
store :: Store (State StoreState)
store =
  let
    hashes limit =
      liftA2 S.union (gets (limitf . M.keysSet . terms))
                     (gets (limitf . M.keysSet . types))
      where limitf = maybe id S.intersection limit
    readTerm = undefined
    writeTerm = undefined
    readType = undefined
    writeType = undefined
    readMetadata = undefined
    writeMetadata = undefined
  in Store hashes readTerm writeTerm readType writeType readMetadata writeMetadata
 -}
