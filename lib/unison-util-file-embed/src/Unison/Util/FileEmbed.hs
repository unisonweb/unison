module Unison.Util.FileEmbed (embedProjectStringFile) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Language.Haskell.TH.Syntax (Exp, Q)

embedProjectStringFile :: FilePath -> Q Exp
embedProjectStringFile fp = makeRelativeToProject fp >>= embedStringFile
