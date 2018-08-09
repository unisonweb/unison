{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Test.Typechecker where

import           EasyTest

import           Control.Monad          (join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable          (traverse_)
import           Data.Text              (unpack)
import           Data.Text.IO           (readFile)
import           System.FilePath        (joinPath, splitPath)
import           System.FilePath.Find   (always, extension, find, (==?))
import           Unison.FileParsers     (parseAndSynthesizeAsFile)
import qualified Unison.Result          as Result
import           Unison.Symbol          (Symbol)

good = void <$> EasyTest.expectRight
bad = void <$> EasyTest.expectLeft

test = scope "typechecker.2" . tests $
        [ go shouldPassNow good
        , go shouldFailNow bad
        , go shouldPassLater (pending . bad)
        , go shouldFailLater (pending . good)
        ]

shouldPassPath = "unison-src/tests"
shouldFailPath = "unison-src/errors"

shouldPassNow :: IO [FilePath]
shouldPassNow = find always (extension ==? ".u") shouldPassPath

shouldFailNow :: IO [FilePath]
shouldFailNow = find always (extension ==? ".u") shouldFailPath

shouldPassLater :: IO [FilePath]
shouldPassLater = find always (extension ==? ".uu") shouldPassPath

shouldFailLater :: IO [FilePath]
shouldFailLater = find always (extension ==? ".uu") shouldFailPath

go :: IO [FilePath] -> (forall e a. Either e a -> Test ()) -> Test ()
go files how = do
  files' <- liftIO files
  traverse_ (makePassingTest how) files'

makePassingTest :: (forall e a. Either e a -> Test ()) -> FilePath -> Test ()
makePassingTest how f = join . liftIO $ do
  let shortName = joinPath . drop 1 . splitPath $ f
  source <- unpack <$> Data.Text.IO.readFile f
  pure $ scope shortName . how . Result.toEither .
    (parseAndSynthesizeAsFile @Symbol) shortName $ source
