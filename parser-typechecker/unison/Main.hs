{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( when )
import           Safe                           ( headMay )
import           System.Environment             ( getArgs )
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import qualified Unison.Codebase.Serialization as S
import           Unison.Codebase.Serialization.V0
                                                ( formatSymbol )
import qualified Unison.CommandLine.Main       as CommandLine
import           Unison.Parser                  ( Ann(External) )
import qualified Unison.Runtime.Rt1IO          as Rt1
import qualified Unison.Codebase.Editor        as Editor

main :: IO ()
main = do
  args <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool
  let codebasePath  = ".unison"
      initialBranchName = "master"
      scratchFilePath   = "."
      theCodebase =
        FileCodebase.codebase1 External formatSymbol formatAnn codebasePath
      launch baseBranch = CommandLine.main
        scratchFilePath
        initialBranchName
        baseBranch
        (headMay args)
        (pure Rt1.runtime)
        theCodebase
  exists <- FileCodebase.exists codebasePath
  when (not exists) $ do
    putStrLn $ "☝️  No codebase exists here so I'm initializing one in: " <> codebasePath
    FileCodebase.initialize codebasePath
  baseBranch <- Editor.initializeCodebase theCodebase
  launch baseBranch

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())
