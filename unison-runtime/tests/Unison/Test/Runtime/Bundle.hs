{-# LANGUAGE CPP #-}

-- | The bundle format (Unison.Runtime.Bundle): a bundle written from
-- unisonrt holds the program it was given, a bundle written from a bundle
-- holds the new one, a bundle runs as one, and on macOS its signature
-- covers it. unisonrt is the test suite's build tool, so it is on the PATH.
module Unison.Test.Runtime.Bundle (test) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.List (isInfixOf)
import EasyTest
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Unison.Runtime.Bundle (bundledProgram, writeBundle)

test :: Test ()
test = scope "bundle" do
  rt <- io (findExecutable "unisonrt") >>= maybe (crash "unisonrt is not on the PATH") pure
  plain <- io (B.readFile rt)
  scope "unisonrt is no bundle" $ expectEqual Nothing (bundledProgram plain)
  -- a program that fits the section's first page, one that grows it, and a
  -- small one again, which shrinks it
  let small = "not a program"
      big = B.replicate 40000 7
  results <- io $ withSystemTempDirectory "bundle-test" \dir -> do
    a <- writeBundle rt small (dir </> "a")
    b <- either (pure . Left) (\pa -> writeBundle pa big (dir </> "b")) a
    c <- either (pure . Left) (\pb -> writeBundle pb small (dir </> "c")) b
    case (a, b, c) of
      (Right pa, Right pb, Right pc) -> do
        programs <- traverse (fmap bundledProgram . B.readFile) [pa, pb, pc]
        sizes <- traverse (fmap B.length . B.readFile) [pa, pb, pc]
        -- it runs as a bundle: the program is read, and is not one
        (code, _, err) <- readProcessWithExitCode pa [] ""
        signed <- signatureValid pc
        pure (Right (programs, sizes, code, err, signed))
      _ -> pure (Left (show (a, b, c)))
  case results of
    Left err -> crash ("writing the bundles: " ++ err)
    Right (programs, sizes, code, err, signed) -> do
      scope "each holds its program" $ expectEqual [Just small, Just big, Just small] programs
      scope "the first and last are the same size" $ expectEqual (take 1 sizes) (drop 2 sizes)
      scope "it runs as a bundle" do
        expectEqual (ExitFailure 1) code
        unless ("is not a compiled program" `isInfixOf` err) $ crash ("stderr: " ++ err)
      scope "its signature is valid" $ expect signed

-- | codesign --verify --strict, on macOS; elsewhere there is no signature
signatureValid :: FilePath -> IO Bool
#if defined(darwin_HOST_OS)
signatureValid path = do
  (code, _, _) <- readProcessWithExitCode "codesign" ["--verify", "--strict", path] ""
  pure (code == ExitSuccess)
#else
signatureValid _ = pure True
#endif
