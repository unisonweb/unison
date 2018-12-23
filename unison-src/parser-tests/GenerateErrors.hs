{- For every file foo.u in the current directory write the parse error to foo.message.txt -}
module GenerateErrors where
import qualified Data.Text                as Text
import           Data.Text.IO             ( readFile )
import           Prelude           hiding ( readFile )
import           System.Directory         ( listDirectory, getCurrentDirectory )
import           System.FilePath          ( takeExtension, dropExtension )
import           System.IO                ( putStrLn )
import qualified Unison.Builtin           as B
import           Unison.Parser            ( Err )
import qualified Unison.Parsers           as P
import           Unison.Symbol            ( Symbol )


unisonFilesInDir :: FilePath -> IO [String]
unisonFilesInDir p = do
  files <- listDirectory p
  pure $ filter ((==) ".u" . takeExtension) files

unisonFilesInCurrDir :: IO [String]
unisonFilesInCurrDir = getCurrentDirectory >>= unisonFilesInDir

errorFileName :: String -> String
errorFileName n = dropExtension n ++ ".message.txt"

processFile :: FilePath -> IO ()
processFile f = do
  content <- Text.unpack <$> readFile f
  let res = P.parseFile f content B.names
  case res of
    Left err ->
      let prettyErr = B.showParseError content (err :: Err Symbol)
      in writeFile (errorFileName f) prettyErr
    Right _  -> putStrLn $
      "Error: " ++ f ++ " parses successfully but none of the files in this directory should parse"

main :: IO ()
main =  unisonFilesInCurrDir >>= mapM_ processFile
