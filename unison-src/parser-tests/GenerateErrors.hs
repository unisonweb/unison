{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{- For every file foo.u in the current directory write the parse error to foo.message.txt -}
module GenerateErrors where
import qualified Data.Text                as Text
import           Prelude
import           System.Directory         ( listDirectory, getCurrentDirectory )
import           System.FilePath          ( takeExtension, dropExtension )
import           System.IO                ( putStrLn )
import qualified Unison.Builtin           as B
import           Unison.Parser            ( Err )
import qualified Unison.Parsers           as P
import           Unison.PrintError        ( prettyParseError )
import           Unison.Symbol            ( Symbol )
import qualified Unison.Util.ColorText    as Color
import           Unison.Var               ( Var )


unisonFilesInDir :: FilePath -> IO [String]
unisonFilesInDir p = do
  files <- listDirectory p
  pure $ filter ((==) ".u" . takeExtension) files

unisonFilesInCurrDir :: IO [String]
unisonFilesInCurrDir = getCurrentDirectory >>= unisonFilesInDir

errorFileName :: String -> String
errorFileName n = dropExtension n ++ ".message.txt"

emitAsPlainTextTo :: Var v => String -> Err v -> FilePath -> IO ()
emitAsPlainTextTo src e f = writeUtf8 f plainErr
  where plainErr = Color.toPlain $ prettyParseError src e

printError :: Var v => String -> Err v -> IO ()
printError src e = putStrLn $ B.showParseError src e

processFile :: FilePath -> IO ()
processFile f = do
  content <- Text.unpack <$> readUtf8 f
  let res = P.parseFile f content B.names
  case res of
    Left err -> do
      emitAsPlainTextTo content (err :: Err Symbol) (errorFileName f)
      printError content err
    Right _  -> putStrLn $
      "Error: " ++ f ++ " parses successfully but none of the files in this directory should parse"

main :: IO ()
main =  unisonFilesInCurrDir >>= mapM_ processFile
