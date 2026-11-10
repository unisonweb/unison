{-# LANGUAGE TemplateHaskell #-}

module Unison.Util.FileEmbed (embedProjectStringFile, embedProjectTextFile, embedProjectTextDir) where

import Data.FileEmbed (embedDir, embedFile, embedStringFile, makeRelativeToProject)
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

-- | The files in a directory (and its subdirectories), as pairs of their paths (relative to the directory) and their
-- contents as 'Data.Text.Text'. A file that is changed makes the module recompile, but a file that is added doesn't, until
-- something else does.
embedProjectTextDir :: FilePath -> Q Exp
embedProjectTextDir fp = do
  path <- makeRelativeToProject fp
  [|[(name, decodeUtf8 bytes) | (name, bytes) <- $(embedDir path)]|]
