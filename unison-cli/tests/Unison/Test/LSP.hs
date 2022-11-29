{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.LSP (test) where

import qualified Crypto.Random as Random
import Data.Bifunctor (bimap)
import Data.List.Extra (firstJust)
import Data.String.Here.Uninterpolated (here)
import Data.Text
import qualified Data.Text as Text
import EasyTest
import qualified System.IO.Temp as Temp
import qualified Unison.ABT as ABT
import qualified Unison.Cli.TypeCheck as Typecheck
import Unison.Codebase (Codebase)
import qualified Unison.Codebase.Init as Codebase.Init
import qualified Unison.Codebase.SqliteCodebase as SC
import qualified Unison.LSP.Queries as LSPQ
import qualified Unison.Lexer.Pos as Lexer
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.UnisonFile as UF

test :: Test ()
test =
  scope "annotations" . tests . fmap makeNodeSelectionTest $
    [ ( "Binary Op lhs",
        [here|term = tr|ue && false|],
        True,
        Left (Term.Boolean True)
      ),
      ( "Binary Op rhs",
        [here|term = true && fa|lse|],
        True,
        Left (Term.Boolean False)
      )
    ]

-- | Test helper which lets you specify a cursor position inline with source text as a '|'.
extractCursor :: Text -> Test (Lexer.Pos, Text)
extractCursor txt =
  case Text.splitOn "|" txt of
    [before, after] ->
      let col = Text.length $ Text.takeWhileEnd (/= '\n') before
          line = Prelude.length $ Text.lines before
       in pure $ (Lexer.Pos line col, before <> after)
    _ -> crash "expected exactly one cursor"

makeNodeSelectionTest :: (String, Text, Bool, Either ((Term.F Symbol Ann Ann (Term Symbol Ann))) (Type.F (Type Symbol Ann))) -> Test ()
makeNodeSelectionTest (name, testSrc, testTypechecked, expected) = scope name $ do
  (pos, src) <- extractCursor testSrc
  (mayParsedFile, mayTypecheckedFile) <- withTestCodebase \codebase -> do
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
    pf <- maybe (crash "Failed to parse") pure mayParsedFile
    let pfResult =
          UF.terms pf
            & firstJust \(_v, trm) ->
              LSPQ.findSmallestEnclosingNode pos trm
    expectEqual (Just $ bimap ABT.Tm ABT.Tm expected) (bimap ABT.out ABT.out <$> pfResult)

  when testTypechecked $
    scope "typechecked file" $ do
      tf <- maybe (crash "Failed to typecheck") pure mayTypecheckedFile
      let tfResult =
            UF.hashTermsId tf
              & toList
              & firstJust \(_refId, _wk, trm, _typ) ->
                LSPQ.findSmallestEnclosingNode pos trm
      expectEqual (Just $ bimap ABT.Tm ABT.Tm expected) (bimap ABT.out ABT.out <$> tfResult)

withTestCodebase ::
  (Codebase IO Symbol Ann -> IO r) -> Test r
withTestCodebase action = do
  r <- io do
    tmp <- Temp.getCanonicalTemporaryDirectory
    tmpDir <- Temp.createTempDirectory tmp "lsp-test"
    Codebase.Init.withCreatedCodebase SC.init "lsp-test" tmpDir SC.DontLock action
  either (crash . show) pure r
