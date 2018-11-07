{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Data.Char                      ( toLower )
import           Safe                           ( headMay )
import           System.Environment             ( getArgs )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.CommandLine   as CommandLine
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
  hSetBuffering stdout NoBuffering -- cool
  let codebasePath      = ".unison"
      initialBranchName = "master"
      scratchFilePath   = "."
      theCodebase =
        FileCodebase.codebase1 External formatSymbol formatAnn codebasePath
      launch = CommandLine.main scratchFilePath
                                initialBranchName
                                (headMay args)
                                (javaRuntime getSymbol 42441)
                                theCodebase
  exists <- FileCodebase.exists codebasePath
  case exists of
    True  -> launch
    False -> do
      putStr
        "I can't find a Unison codebase here, would you like to create one? [y/n] "
      line <- getLine
      case words (map toLower line) of
        ('y' : _) : _ -> do
          FileCodebase.initialize codebasePath
          Codebase.initialize theCodebase
          launch
        _ -> pure ()

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())
