{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Test.Typechecker where

import           Control.Lens           ( view )
import           Control.Lens.Tuple     ( _5 )
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map               as Map
import           Data.Sequence          (Seq)
import           Data.Text              (unpack)
import           Data.Text.IO           (readFile)
import           EasyTest
import           System.FilePath        (joinPath, splitPath, replaceExtension)
import           System.FilePath.Find   (always, extension, find, (==?))
import           System.Directory       ( doesFileExist )
import qualified Unison.Builtin         as Builtin
import           Unison.Codebase.Runtime  ( Runtime, evaluateWatches )
import           Unison.Parser          as Parser
import qualified Unison.Parsers         as Parsers
import qualified Unison.PrettyPrintEnv  as PPE
import qualified Unison.PrintError      as PrintError
import           Unison.Result          (pattern Result, Result)
import qualified Unison.Result          as Result
import qualified Unison.Runtime.Rt1IO   as RT
import           Unison.Symbol          (Symbol)
import qualified Unison.Term            as Term
import           Unison.Term            ( amap )
import           Unison.Test.Common     (parseAndSynthesizeAsFile)
import qualified Unison.UnisonFile      as UF
import           Unison.Util.Monoid     (intercalateMap)

type Note = Result.Note Symbol Parser.Ann

type TFile = UF.TypecheckedUnisonFile Symbol Ann
type SynthResult =
  Result (Seq Note)
         (PrintError.Env, Maybe TFile)

type EitherResult = Either String TFile

ppEnv :: PPE.PrettyPrintEnv
ppEnv = PPE.fromNames Builtin.names

expectRight' :: Either String a -> Test a
expectRight' (Left  e) = crash e
expectRight' (Right a) = ok >> pure a

good :: EitherResult -> Test ()
good = void <$> expectRight'

bad :: EitherResult -> Test ()
bad = void <$> EasyTest.expectLeft

test :: Test ()
test = do
  let rt = RT.runtime
  scope "typechecker"
    . tests
    $ [ go rt shouldPassNow   good
      , go rt shouldFailNow   bad
      , go rt shouldPassLater (pending . bad)
      , go rt shouldFailLater (pending . good)
      ]

shouldPassPath, shouldFailPath :: String
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

go :: Runtime Symbol -> IO [FilePath] -> (EitherResult -> Test ()) -> Test ()
go rt files how = do
  files' <- liftIO files
  tests (makePassingTest rt how <$> files')

showNotes :: Foldable f => String -> PrintError.Env -> f Note -> String
showNotes source env notes =
  intercalateMap "\n\n" (PrintError.renderNoteAsANSI env source) notes

decodeResult
  :: String -> SynthResult -> EitherResult--  String (UF.TypecheckedUnisonFile Symbol Ann)
decodeResult source (Result notes Nothing) =
  Left $ showNotes source ppEnv notes
decodeResult source (Result notes (Just (env, Nothing))) =
  Left $ showNotes source env notes
decodeResult _source (Result _notes (Just (_env, Just uf))) =
  Right uf

makePassingTest
  :: Runtime Symbol -> (EitherResult -> Test ()) -> FilePath -> Test ()
makePassingTest rt how filepath = scope shortName $ do
  let valueFile = replaceExtension filepath "ur"
  source <- io $ unpack <$> Data.Text.IO.readFile filepath
  let r = decodeResult source $ parseAndSynthesizeAsFile [] shortName source
  rFileExists <- io $ doesFileExist valueFile
  case (rFileExists, r) of
    (True, Right file) -> do
      values <- io $ unpack <$> Data.Text.IO.readFile valueFile
      let untypedFile = UF.discardTypes file
      let term        = Parsers.parseTerm values $ UF.toNames untypedFile
      (bindings, watches) <- io $ evaluateWatches Builtin.codeLookup
                                      (const $ pure Nothing)
                                      rt
                                      untypedFile
      case term of
        Right tm -> do
          -- compare the the watch expression from the .u with the expr in .ur
          let [watchResult] = view _5 <$> Map.elems watches
              tm' = Term.letRec' False bindings watchResult
          -- note . show $ tm'
          -- note . show $ amap (const ()) tm
          expect $ tm' == amap (const ()) tm
        Left e -> crash $ show e
    _ -> pure ()
  how r
  where shortName = joinPath . drop 1 . splitPath $ filepath
