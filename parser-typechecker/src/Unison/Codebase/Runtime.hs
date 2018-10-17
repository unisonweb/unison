module Unison.Codebase.Runtime where

import qualified Unison.Codebase   as Codebase
import           Unison.Parser     (Ann)
import qualified Unison.UnisonFile as UF

type Codebase v = Codebase.Codebase IO v Ann
type UnisonFile v = UF.UnisonFile v Ann

data Runtime v = Runtime
  { terminate :: IO ()
  , evaluate  :: UnisonFile v -> Codebase v -> IO () -- but eventually IO (Term v)
  }
