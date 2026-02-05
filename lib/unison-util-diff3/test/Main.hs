module Main (main) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory qualified as Directory
import System.FilePath ((</>))
import System.FilePath qualified as FilePath
import Unison.Prelude
import Unison.Util.Diff3 (Hunk (..), diff3)

main :: IO ()
main = do
  testFiles <- Directory.listDirectory ("test" </> "testcases")
  let testNames = sort (nubOrd (map FilePath.dropExtension testFiles))
  mapM_ runTest testNames

runTest :: String -> IO ()
runTest name = do
  let dir = "test" </> "testcases"
  lca <- Text.lines <$> Text.readFile (dir </> (name ++ ".lca"))
  alice <- Text.lines <$> Text.readFile (dir </> (name ++ ".alice"))
  bob <- Text.lines <$> Text.readFile (dir </> (name ++ ".bob"))
  let merged = foldMap renderHunk (diff3 lca alice bob)
  Text.writeFile (dir </> (name ++ ".merged")) merged

renderHunk :: Hunk Text -> Text
renderHunk = \case
  Hunk hunk -> Text.unlines hunk
  Conflict lca alice bob ->
    "<<<<<<<\n"
      <> Text.unlines alice
      <> "=======\n"
      <> Text.unlines lca
      <> "=======\n"
      <> Text.unlines bob
      <> ">>>>>>>\n"
