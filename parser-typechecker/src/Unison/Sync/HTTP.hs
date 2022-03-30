module Unison.Sync.HTTP where

import Control.Monad.Reader
import qualified Network.HTTP.Client as HTTP
import Unison.Sync
import Unison.Sync.Types

newtype Sync a = Sync (ReaderT HTTP.Manager IO a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadSync Sync where
  getCausalHashForPath = _
  pushForce = _
  uploadToRepo = _
  downloadEntities = _
