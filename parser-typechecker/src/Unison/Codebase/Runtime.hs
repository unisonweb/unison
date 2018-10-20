{-#LANGUAGE RankNTypes#-}
module Unison.Codebase.Runtime where

import Unison.Codebase   (Codebase)
import Unison.UnisonFile (UnisonFile)

data Runtime v = Runtime
  { terminate :: IO ()
  , evaluate  :: forall a b . UnisonFile v a -> Codebase IO v b -> IO () -- but eventually IO (Term v)
  }
