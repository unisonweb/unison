{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.Test.Codebase.Serialization where

import           Control.Monad                             (forM, forM_)
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           EasyTest

import           Data.List                                 (isSuffixOf)
import           System.Directory                          (doesDirectoryExist,
                                                            getDirectoryContents)
import           System.FilePath                           ((</>))
import           Unison.Codebase.Serialization.Serializers
import           Unison.Parser                             (Ann (External))
import           Unison.Symbol                             (Symbol (..))
import           Unison.Term                               (AnnotatedTerm)
import qualified Unison.Term                               as Term
import qualified Unison.Var                                as Var

test :: Test ()
test = tests
  [
  -- random terms
    scope "random terms, v1 serializer" $
      listOf 100 genTerm >>= roundTripTest v1Serializer
  , scope "random terms, v1Cborg serializer" $
      listOf 100 genTerm >>= roundTripTest v1SerializerCborg
  -- compiled terms (from files)
  ,  scope "compiled terms, v0Cborg serializer" $
      compiledTerms' >>= roundTripTest v0SerializerCborg
  , scope "compiled terms, v0 serializer" $
      compiledTerms' >>= roundTripTest v0Serializer
  , scope "compiled terms, v1 serializer" $
      compiledTerms' >>= roundTripTest v1Serializer
  , scope "compiled terms, v1Cborg serializer" $
      compiledTerms' >>= roundTripTest v1SerializerCborg
--  -- json
--  , scope "compiled terms, round trip json, v1Cborg serializer" $
--      compiledTerms' >>= roundTripTestJSON v1SerializerCborg
  ]

roundTripTest :: TermSerializer -> [AnnotatedTerm Symbol Ann] -> Test ()
roundTripTest ts = roundTripTest' (roundTrip ts)

roundTripTestJSON :: TermSerializer -> [AnnotatedTerm Symbol Ann] -> Test ()
roundTripTestJSON ts = roundTripTest' (roundTripJSON ts)

roundTripTest' ::
     (AnnotatedTerm Symbol Ann -> Either DeserializeError (AnnotatedTerm Symbol Ann))
  -> [AnnotatedTerm Symbol Ann]
  -> Test ()
roundTripTest' f terms =
  forM_ terms $ \t -> scope (show t) $ expectEqual (Right t) (f t)

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
  -- ... todo
  ]

type Term = AnnotatedTerm Symbol Ann

testTermSerialization :: Term -> IO Bool
testTermSerialization term = do
    let termV1  = roundTrip v1Serializer      term
        termV1C = roundTrip v1SerializerCborg term
    print term
    return $ Right term == termV1 && Right term == termV1C

compiledTermsFiles :: MonadIO m => m [FilePath]
compiledTermsFiles =
  filter ("compiled.ub" `isSuffixOf`) <$> getRecursiveContents ".unison/terms/"

compiledTerms :: MonadIO m => TermSerializer -> m [Either DeserializeError Term]
compiledTerms ts = traverse (getTermFromFile ts) =<< compiledTermsFiles

-- all terms using the v1 serializer to read them in.
compiledTerms' :: Test [Term]
compiledTerms' = traverse f =<< liftIO (compiledTerms v1Serializer)
  where
  f (Left err) = crash $ "Couldn't deserialize: " ++ show err
  f (Right t) = pure t

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
