{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Test.Codebase.Serialization where

import           Control.Monad                             (forM, forM_)
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import qualified Data.ByteString                           as BS
import           EasyTest

import           System.Directory                          (doesDirectoryExist,
                                                            getDirectoryContents)
import           System.FilePath                           ((</>))
import           Data.List                                 (isSuffixOf)
import           Unison.Codebase.Serialization.Serializers
import           Unison.Parser                             (Ann (External))
import           Unison.Symbol                             (Symbol (..))
import           Unison.Term                               (AnnotatedTerm)
import qualified Unison.Term                               as Term
import qualified Unison.Var                                as Var

test :: Test ()
test = tests
  [ scope "random terms, v1 serializer" $ do
      terms <- listOf 100 genTerm
      forM_ terms $ \t -> expect (t == roundTrip v1Serializer t)
  , scope "random terms, v1Cborg serializer" $ do
      terms <- listOf 100 genTerm
      forM_ terms $ \t -> expect (t == roundTrip v1SerializerCborg t)
  , scope "compiledTerms terms, v1 serializer" $ do
      terms <- liftIO $ compiledTerms v1Serializer
      forM_ terms $ \t -> expect (t == roundTrip v1Serializer t)
  , scope "compiledTerms terms, v1Cborg serializer" $ do
      terms <- liftIO $ compiledTerms v1Serializer
      forM_ terms $ \t -> expect (t == roundTrip v1SerializerCborg t)
  ]

genTerm :: Test (AnnotatedTerm Symbol Ann)
genTerm = genTerm' genSymbol genAnn

genSymbol :: Test Symbol
genSymbol = Symbol <$> word64 <*> pure Var.Blank -- todo: gen var

genAnn :: Test Ann
genAnn = pure External

genTerm' :: Ord v => Test v -> Test a -> Test (Term.AnnotatedTerm v a)
genTerm' _ genA = pick =<< sequence [
    Term.int <$> genA <*> int64
  , Term.nat <$> genA <*> word64
  , Term.float <$> genA <*> double
  , Term.boolean <$> genA <*> bool
  ]

type Term = AnnotatedTerm Symbol Ann

testTermSerialization :: Term -> IO Bool
testTermSerialization term = do
    let termV1  = roundTrip v1Serializer      term
        termV1C = roundTrip v1SerializerCborg term
    print term
    return $ term == termV1 && term == termV1C

compiledTermsFiles :: MonadIO m => m [FilePath]
compiledTermsFiles =
  filter ("compiled.ub" `isSuffixOf`) <$> getRecursiveContents ".unison/terms/"

compiledTerms :: MonadIO m => TermSerializer -> m [Term]
compiledTerms ts = traverse (getTermFromFile ts) =<< compiledTermsFiles

getTermFromFile :: MonadIO m => TermSerializer -> FilePath -> m Term
getTermFromFile ts path = getTerm ts <$> liftIO (BS.readFile path)

getRecursiveContents :: MonadIO m => FilePath -> m [FilePath]
getRecursiveContents topPath =
  liftIO $ do
    names <- getDirectoryContents topPath
    let properNames = filter (`notElem` [".", ".."]) names
    paths <-
      forM properNames $ \name -> do
        let path = topPath </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then getRecursiveContents path
          else return [path]
    return (concat paths)
