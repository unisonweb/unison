{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( when )
import           Data.Monoid                    ((<>))
import           Options.Applicative
import           Safe                           ( headMay )
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
import qualified Unison.Runtime.Rt0            as Rt0


data Args = Args
  { initialFile :: [String]
  , scratchPath :: String
  , haskell :: Bool}

args :: Parser Args
args = Args
     <$> many (argument str
          ( metavar "INITIALFILE"
         <> help "Scratch file (currently not used)" ))
     <*> strOption
          ( long "scratchpath"
         <> short 's'
         <> metavar "PATH"
         <> showDefault
         <> value "."
         <> help "Scratch file path" )
     <*> switch
          ( long "haskell"
         <> short 'r'
         <> help "Use the Haskell runtime" )

opts :: ParserInfo Args
opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Run Unison"
     <> header "unison - Next generation programming language" )

main :: IO ()
main = do
  args <- execParser opts
  -- hSetBuffering stdout NoBuffering -- cool
  let codebasePath  = ".unison"
      initialBranchName = "master"
      scratchFilePath   = scratchPath args
      useHaskellRuntime = haskell args
      _initialFilePath = headMay $ initialFile args
      theCodebase =
        FileCodebase.codebase1 External formatSymbol formatAnn codebasePath
      launch = CommandLine.main
        scratchFilePath
        initialBranchName
        _initialFilePath
        (if useHaskellRuntime then pure Rt0.runtime
         else javaRuntime getSymbol 42441)
        theCodebase
  exists <- FileCodebase.exists codebasePath
  when (not exists) $ do
    putStrLn $ "☝️  No codebase exists here so I'm initializing one in: " <> codebasePath
    FileCodebase.initialize codebasePath
    Codebase.initialize theCodebase
  launch

formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())
