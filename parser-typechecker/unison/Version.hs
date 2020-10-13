{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Language.Haskell.TH (runIO)
import Language.Haskell.TH.Syntax (Exp(LitE), Lit(StringL))
import Shellmet
import Data.Text

gitDescribe :: String
gitDescribe = $( fmap (LitE . StringL . unpack) . runIO $
  "git" $| ["describe", "--tags", "--always", "--dirty='"]
        $? pure "unknown"
  )

