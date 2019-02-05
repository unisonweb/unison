{-#LANGUAGE RankNTypes#-}
module Unison.Codebase.Runtime where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Text                      ( Text )
import           Unison.Codebase                ( Codebase )
import           Unison.UnisonFile              ( UnisonFile )
import           Unison.Term                    ( Term )

data Runtime v = Runtime
  { terminate :: forall m. MonadIO m => m ()
  , evaluate
      :: forall a b m
      .  MonadIO m
      => UnisonFile v a
      -> Codebase m v b
      -> m (Term v)
  }
