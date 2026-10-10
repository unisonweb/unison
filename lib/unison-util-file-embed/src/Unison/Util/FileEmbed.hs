{-# LANGUAGE TemplateHaskell #-}

module Unison.Util.FileEmbed (embedProjectStringFile, embedProjectTextFile) where

import Data.FileEmbed (embedFile, embedStringFile, makeRelativeToProject)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH.Syntax (Exp, Q)

embedProjectStringFile :: FilePath -> Q Exp
embedProjectStringFile fp = makeRelativeToProject fp >>= embedStringFile

-- | The file's contents as a 'Data.Text.Text'. The bytes are a literal in
-- the object code ('embedFile'); the decode runs once, when the value is
-- first forced.
embedProjectTextFile :: FilePath -> Q Exp
embedProjectTextFile fp = do
  path <- makeRelativeToProject fp
  [|decodeUtf8 $(embedFile path)|]
