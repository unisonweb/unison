module Unison.Node.Server where

import Control.Monad.IO.Class
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Type as T
import Unison.Syntax.Hash (Hash)
import Unison.Syntax.Term (Term)
import Unison.Syntax.Type (Type)
import Unison.Node (Node)
import Web.Scotty

server :: MonadIO f => Int -> Node f Hash Term Type -> f ()
server port node = undefined
