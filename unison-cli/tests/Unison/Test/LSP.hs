{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.LSP (test) where

import Control.Error.Safe (rightMay)
import qualified Crypto.Random as Random
import Data.String.Here.Uninterpolated (here)
import Data.Text
import qualified Data.Text as Text
import EasyTest
import qualified System.IO.Temp as Temp
import Text.Megaparsec
import qualified Unison.Cli.TypeCheck as Typecheck
import Unison.Codebase (Codebase)
import Unison.Codebase.Editor.RemoteRepo
import Unison.Codebase.Editor.VersionParser
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.Lexer.Pos as Lexer
import Unison.Parser.Ann (Ann (..))
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import Unison.Term (Term)
import qualified Unison.Term as Term
import qualified Unison.Test.Ucm as Ucm
import Unison.Type (Type)
import qualified Unison.UnisonFile as UF

test :: Test ()
test =
  scope "annotations" . tests . fmap makeTest $
    [ ( "Binary Op lhs",
        [here|term = 100 + 200|],
        Lexer.Pos 0 8,
        Left (Term.Nat 100)
      )
    ]

makeTest :: (String, Text, Lexer.Pos, Either ((Term.F Symbol Ann Ann (Term Symbol Ann))) (Type Symbol Ann)) -> Test ()
makeTest (name, src, pos, expected) = scope name $ do
  (pf, tf) <- withTestCodebase \codebase -> do
    let generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG
    let ambientAbilities = []
    let parseNames = mempty
    let lexedSource = (src, L.lexer name (Text.unpack src))
    r <- Typecheck.typecheckHelper codebase generateUniqueName ambientAbilities parseNames (Text.pack name) lexedSource
    let Result.Result _notes mayResult = r
    let (parsedFile, typecheckedFile) = case mayResult of
          Nothing -> (Nothing, Nothing)
          Just (Left uf) -> (Just uf, Nothing)
          Just (Right tf) -> (Just $ UF.discardTypes tf, Just tf)
    pure (parsedFile, typecheckedFile)
  scope "parsed file" $ do
    pfResult <-
      UF.terms parsedFile
        & find \(_v, trm) ->
          LSPQ.findSmallestEnclosingNode pos trm
    expectEqual pfResult expected

  scope "typechecked file" $ do
    pfResult <-
      UF.hashTermsId
        & find \(_refId, _wk, trm, _typ) ->
          LSPQ.findSmallestEnclosingNode pos trm
    expectEqual (bimap ABT.out ABT.out <$> pfResult) (Just expected)

withTestCodebase ::
  (Codebase IO Symbol Ann -> IO r) -> Test r
withTestCodebase action = do
  r <- io do
    tmp <-
      Temp.getCanonicalTemporaryDirectory
        >>= flip Temp.createTempDirectory "lsp-test"
    Codebase.Init.withCreatedCodebase SC.init "lsp-test" tmp SC.DoLock action
  either (crash . show) pure r
