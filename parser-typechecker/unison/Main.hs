{-# LANGUAGE TypeApplications, OverloadedStrings #-}

module Main where

import qualified Unison.Codebase.CommandLine as CommandLine
import           Unison.Codebase.Runtime.JVM (javaRuntime)
import Unison.Symbol (Symbol)
import qualified Unison.Codebase as Codebase
import Unison.Parser (Ann(External))
import Unison.Codebase.Serialization.V0 (formatSymbol)
import qualified Unison.Codebase.Serialization as S

main :: IO ()
main = CommandLine.main "." "master"
        (javaRuntime @Symbol 42441)
        (Codebase.codebase1 External formatSymbol formatAnn ".codebase")


formatAnn :: S.Format Ann
formatAnn = S.Format (pure External) (\_ -> pure ())
