module Main where

import System.Environment (getArgs)
import Unison.Codecs as Codecs
import Data.Bytes.Put (runPutS)
import Control.Monad.State (evalStateT)
import qualified Unison.FileParser as FileParser
import qualified Unison.Parser as Parser
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFile, outputFile] -> do
      unisonFile <- FileParser.unsafeReadAndParseFile Parser.penv0 sourceFile
      let bs = runPutS $ flip evalStateT 0 $ Codecs.serializeFile unisonFile
      BS.writeFile outputFile bs

    _ -> putStrLn "usage: bootstrap in.u out.ub"
