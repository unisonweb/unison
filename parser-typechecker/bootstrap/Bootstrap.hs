{-# Language OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit (die)
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Data.ByteString as BS
import qualified Unison.Result as Result

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      unisonFile <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
      let r = Result.toEither $ FileParsers.serializeUnisonFile unisonFile
          f (_unisonFile', typ, bs) = do
            putStrLn ("typechecked as " ++ show typ)
            BS.writeFile outputFile bs
      either (const $ die "a failure occurred TODO") f r

    _ -> putStrLn "usage: bootstrap <in-file.u> <out-file.ub>"
