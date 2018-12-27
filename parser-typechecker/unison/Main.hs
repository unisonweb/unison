{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( when )
import           Safe                           ( headMay )
import           System.Environment             ( getArgs )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.CommandLine2  as CommandLine
import qualified Unison.Codebase.FileCodebase  as FileCodebase
import           Unison.Codebase.Runtime.JVM    ( javaRuntime )
import qualified Unison.Codebase.Serialization as S
import           Unison.Codebase.Serialization.V0
                                                ( formatSymbol
                                                , getSymbol
                                                )
import           Unison.Parser                  ( Ann(External) )

main :: IO ()
main = do
  args <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool
  let codebasePath  = ".unison"
      initialBranchName = "master"
      scratchFilePath   = "."
      theCodebase =
        FileCodebase.codebase1 External formatSymbol formatAnn codebasePath
      launch = CommandLine.main
        scratchFilePath
        initialBranchName
        (headMay args)
        (javaRuntime getSymbol 42441)
        theCodebase
  exists <- FileCodebase.exists codebasePath
  when (not exists) $ do
    putStrLn $ "☝️  No codebase exists here so I'm initializing one in: " <> codebasePath
    FileCodebase.initialize codebasePath
    Codebase.initialize theCodebase
  launch

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())
