{-#LANGUAGE RankNTypes#-}
module Unison.Codebase.Runtime where

import           Data.Text                      ( Text )
import           Unison.Codebase                ( Codebase )
import           Unison.UnisonFile              ( UnisonFile )
import           Unison.Term                    ( Term )

data Runtime v = Runtime
  { terminate :: IO ()
  , evaluate
      :: forall a b
       . UnisonFile v a
      -> Codebase IO v b
      -> IO ([(Text, Term v)], Term v)
  }
