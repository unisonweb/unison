{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( unless )
import Safe ( headMay )
import System.Environment ( getArgs )
import System.Directory ( getCurrentDirectory )
import Unison.Codebase.Serialization.V1 ( formatSymbol )
import Unison.Parser ( Ann(External) )
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileCodebase as FileCodebase
import qualified Unison.Codebase.Serialization as S
import qualified Unison.CommandLine.Main as CommandLine
import qualified Unison.Runtime.Rt1IO as Rt1
import qualified Unison.Codebase.Path as Path
import qualified Unison.Util.Pretty as P
import qualified Unison.PrettyTerminal as PT

main :: IO ()
main = do
  args <- getArgs
  -- hSetBuffering stdout NoBuffering -- cool
  dir  <- getCurrentDirectory
  let
    codebasePath = ".unison/v1"
    initialPath  = Path.absoluteEmpty
    theCodebase = FileCodebase.codebase1 formatSymbol formatAnn codebasePath
    launch = CommandLine.main dir
                              initialPath
                              (headMay args)
                              (pure Rt1.runtime)
                              theCodebase
  exists <- FileCodebase.exists codebasePath
  unless exists $ do
    PT.putPrettyLn' . P.callout "☝️" . P.wrap $
      "No codebase exists here so I'm initializing one in: " <> P.string codebasePath
    FileCodebase.initialize codebasePath
    Codebase.initializeCodebase theCodebase
  launch

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())
