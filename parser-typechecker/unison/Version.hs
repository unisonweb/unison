{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Data.Text
import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Exp (LitE), Lit (StringL))
import Shellmet

gitDescribe :: String
gitDescribe =
  $( fmap (LitE . StringL . unpack) . runIO $
       "git" $| ["describe", "--tags", "--always", "--dirty='"]
         $? pure "unknown"
   )
