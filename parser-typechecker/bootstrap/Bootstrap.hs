{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString    as BS
import           Data.Foldable      (toList)
import           Data.Text          (unpack)
import qualified Data.Text.IO
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import qualified Unison.FileParsers as FileParsers
import qualified Unison.Parser      as Parser
import qualified Unison.Parsers     as Parsers
import           Unison.PrintError  (printNoteWithSourceAsAnsi)
import           Unison.Result      (Result (Result))
import qualified Unison.Result      as Result
import           Unison.Symbol      (Symbol)
import           Unison.Util.Monoid

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      source <- unpack <$> Data.Text.IO.readFile sourceFile
      (env0, unisonFile) <- Parsers.unsafeReadAndParseFile Parser.penv0 sourceFile
      let (Result notes' r) = FileParsers.serializeUnisonFile unisonFile
          f (_unisonFile', typ, bs) = do
            putStrLn ("typechecked as " ++ show typ) -- todo, render this type with the errorEnv
            BS.writeFile outputFile bs
          showNote :: [Result.Note Symbol Parser.Ann] -> String
          showNote notes =
            intercalateMap "\n\n" (printNoteWithSourceAsAnsi env0 source) notes
      putStrLn . showNote . toList $ notes'
      maybe exitFailure f r

    _ -> putStrLn "usage: bootstrap <in-file.u> <out-file.ub>"
