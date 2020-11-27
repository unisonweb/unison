{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Test.UnisonSources where

import Control.Exception (throwIO)
import Control.Lens (view)
import Control.Lens.Tuple (_5)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Text (unpack)
import Data.Text.IO (readFile)
import EasyTest
import System.Directory (doesFileExist)
import System.FilePath (joinPath, replaceExtension, splitPath)
import System.FilePath.Find (always, extension, find, (==?))
import qualified Unison.ABT as ABT
import qualified Unison.Builtin as Builtin
import Unison.Codebase.Runtime (Runtime, evaluateWatches)
import Unison.Codebase.Serialization (getFromBytes, putBytes)
import qualified Unison.Codebase.Serialization.V1 as V1
import Unison.DataDeclaration (DataDeclaration, EffectDeclaration)
import qualified Unison.Names3
import Unison.Parser as Parser
import qualified Unison.Parsers as Parsers
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrintError as PrintError
import Unison.Reference (Reference)
import Unison.Result (Result, pattern Result)
import qualified Unison.Result as Result
import qualified Unison.Runtime.Interface as RTI
import qualified Unison.Runtime.Rt1IO as RT
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Test.Common (parseAndSynthesizeAsFile, parsingEnv)
import qualified Unison.Test.Common as Common
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty (toPlain)
import qualified Unison.Var as Var

type Note = Result.Note Symbol Parser.Ann

type TFile = UF.TypecheckedUnisonFile Symbol Ann

type SynthResult =
  Result
    (Seq Note)
    (Either Unison.Names3.Names0 TFile)

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

test :: Bool -> Test ()
test new = do
  rt <- if new then io RTI.startRuntime else pure RT.runtime
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
  intercalateMap "\n\n" $ PrintError.renderNoteAsANSI 60 env source

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
          (Unison.Names3.shadowing errNames Builtin.names)
      )
      notes
decodeResult _source (Result _notes (Just (Right uf))) =
  Right uf

makePassingTest ::
  Runtime Symbol -> (EitherResult -> Test TFile) -> FilePath -> Test ()
makePassingTest rt how filepath = scope (shortName filepath) $ do
  uf <- typecheckingTest how filepath
  resultTest rt uf filepath *> serializationTest uf

shortName :: FilePath -> FilePath
shortName = joinPath . drop 1 . splitPath

typecheckingTest :: (EitherResult -> Test TFile) -> FilePath -> Test TFile
typecheckingTest how filepath = scope "typecheck" $ do
  source <- io $ unpack <$> Data.Text.IO.readFile filepath
  how . decodeResult source $ parseAndSynthesizeAsFile [] (shortName filepath) source

resultTest ::
  Runtime Symbol -> TFile -> FilePath -> Test ()
resultTest rt uf filepath = do
  let valueFile = replaceExtension filepath "ur"
  rFileExists <- io $ doesFileExist valueFile
  if rFileExists
    then scope "result" $ do
      values <- io $ unpack <$> Data.Text.IO.readFile valueFile
      let untypedFile = UF.discardTypes uf
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
              untypedFile
      case term of
        Right tm -> do
          -- compare the the watch expression from the .u with the expr in .ur
          let [watchResult] = view _5 <$> Map.elems watches
              tm' = Term.letRec' False bindings watchResult
          -- note . show $ tm'
          -- note . show $ Term.amap (const ()) tm
          expect $ tm' == Term.amap (const ()) tm
        Left e -> crash $ show e
    else pure ()

serializationTest :: TFile -> Test ()
serializationTest uf =
  scope "serialization" . tests . concat $
    [ map testDataDeclaration (Map.toList $ UF.dataDeclarations' uf),
      map testEffectDeclaration (Map.toList $ UF.effectDeclarations' uf),
      map testTerm (Map.toList $ UF.hashTerms uf)
    ]
  where
    putUnit :: Monad m => () -> m ()
    putUnit () = pure ()
    getUnit :: Monad m => m ()
    getUnit = pure ()
    testDataDeclaration :: (Symbol, (Reference, DataDeclaration Symbol Ann)) -> Test ()
    testDataDeclaration (name, (_, decl)) =
      scope (Var.nameStr name) $
        let decl' :: DataDeclaration Symbol ()
            decl' = void decl
            bytes = putBytes (V1.putDataDeclaration V1.putSymbol putUnit) decl'
            decl'' = getFromBytes (V1.getDataDeclaration V1.getSymbol getUnit) bytes
         in expectEqual decl'' (Just decl')
    testEffectDeclaration :: (Symbol, (Reference, EffectDeclaration Symbol Ann)) -> Test ()
    testEffectDeclaration (name, (_, decl)) =
      scope (Var.nameStr name) $
        let decl' :: EffectDeclaration Symbol ()
            decl' = void decl
            bytes = putBytes (V1.putEffectDeclaration V1.putSymbol putUnit) decl'
            decl'' = getFromBytes (V1.getEffectDeclaration V1.getSymbol getUnit) bytes
         in expectEqual decl'' (Just decl')
    testTerm :: (Symbol, (Reference, Term Symbol Ann, Type Symbol Ann)) -> Test ()
    testTerm (name, (_, tm, tp)) =
      scope (Var.nameStr name) $
        let tm' :: Term Symbol ()
            tm' = Term.amap (const ()) tm
            tp' :: Type Symbol ()
            tp' = ABT.amap (const ()) tp
            tmBytes = putBytes (V1.putTerm V1.putSymbol putUnit) tm'
            tpBytes = putBytes (V1.putType V1.putSymbol putUnit) tp'
            tm'' = getFromBytes (V1.getTerm V1.getSymbol getUnit) tmBytes
            tp'' = getFromBytes (V1.getType V1.getSymbol getUnit) tpBytes
         in tests
              [ scope "type" $ expectEqual tp'' (Just tp'),
                scope "term" $ expectEqual tm'' (Just tm')
              ]
