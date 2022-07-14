{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Test.UnisonSources where

import Control.Exception (throwIO)
import Control.Lens (view)
import Control.Lens.Tuple (_5)
import qualified Data.Map as Map
import Data.Text (unpack)
import EasyTest
import System.Directory (doesFileExist)
import System.FilePath (joinPath, replaceExtension, splitPath)
import System.FilePath.Find (always, extension, find, (==?))
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Runtime (Runtime, evaluateWatches)
import Unison.Names (Names)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann)
import qualified Unison.Parsers as Parsers
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Names as PPE
import qualified Unison.PrintError as PrintError
import Unison.Result (Result, pattern Result)
import qualified Unison.Result as Result
import qualified Unison.Runtime.Interface as RTI
import Unison.Symbol (Symbol)
import qualified Unison.Term as Term
import Unison.Test.Common (parseAndSynthesizeAsFile, parsingEnv)
import qualified Unison.Test.Common as Common
import qualified Unison.UnisonFile as UF
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty (toPlain)

type Note = Result.Note Symbol Ann

type TFile = UF.TypecheckedUnisonFile Symbol Ann

type SynthResult =
  Result
    (Seq Note)
    (Either Names TFile)

type EitherResult = Either String TFile

ppEnv :: PPE.PrettyPrintEnv
ppEnv = PPE.fromNames Common.hqLength Builtin.names

expectRight' :: Either String a -> Test a
expectRight' (Left e) = crash e
expectRight' (Right a) = ok >> pure a

good :: EitherResult -> Test TFile
good = expectRight'

bad :: EitherResult -> Test TFile
bad r = EasyTest.expectLeft r >> done

test :: Test ()
test = do
  rt <- io (RTI.startRuntime False RTI.OneOff "")
  scope "unison-src"
    . tests
    $ [ go rt shouldPassNow good,
        go rt shouldFailNow bad,
        go rt shouldPassLater (pending . bad),
        go rt shouldFailLater (pending . good)
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

go :: Runtime Symbol -> IO [FilePath] -> (EitherResult -> Test TFile) -> Test ()
go rt files how = do
  files' <- liftIO files
  tests (makePassingTest rt how <$> files')

showNotes :: Foldable f => String -> PrintError.Env -> f Note -> String
showNotes source env =
  intercalateMap "\n\n" $ PrintError.renderNoteAsANSI 60 env source Path.absoluteEmpty

decodeResult ::
  String -> SynthResult -> EitherResult --  String (UF.TypecheckedUnisonFile Symbol Ann)
decodeResult source (Result notes Nothing) =
  Left $ showNotes source ppEnv notes
decodeResult source (Result notes (Just (Left errNames))) =
  Left $
    showNotes
      source
      ( PPE.fromNames
          Common.hqLength
          (NamesWithHistory.shadowing errNames Builtin.names)
      )
      notes
decodeResult _source (Result _notes (Just (Right uf))) =
  Right uf

makePassingTest ::
  Runtime Symbol -> (EitherResult -> Test TFile) -> FilePath -> Test ()
makePassingTest rt how filepath = scope (shortName filepath) $ do
  uf <- typecheckingTest how filepath
  resultTest rt uf filepath

shortName :: FilePath -> FilePath
shortName = joinPath . drop 1 . splitPath

typecheckingTest :: (EitherResult -> Test TFile) -> FilePath -> Test TFile
typecheckingTest how filepath = scope "typecheck" $ do
  source <- io $ unpack <$> readUtf8 filepath
  how . decodeResult source $ parseAndSynthesizeAsFile [] (shortName filepath) source

resultTest ::
  Runtime Symbol -> TFile -> FilePath -> Test ()
resultTest rt uf filepath = do
  let valueFile = replaceExtension filepath "ur"
  rFileExists <- io $ doesFileExist valueFile
  if rFileExists
    then scope "result" $ do
      values <- io $ unpack <$> readUtf8 valueFile
      let term = Parsers.parseTerm values parsingEnv
      let report e = throwIO (userError $ toPlain 10000 e)
      (bindings, watches) <-
        io $
          either report pure
            =<< evaluateWatches
              Builtin.codeLookup
              mempty
              (const $ pure Nothing)
              rt
              uf
      case term of
        Right tm -> do
          -- compare the the watch expression from the .u with the expr in .ur
          let [watchResult] = view _5 <$> Map.elems watches
              tm' = Term.letRec' False bindings watchResult
          -- note . show $ tm'
          -- note . show $ Term.amap (const ()) tm
          expectEqual tm' (Term.amap (const ()) tm)
        Left e -> crash $ show e
    else pure ()
