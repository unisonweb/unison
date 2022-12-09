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
import qualified Unison.Reference as Reference
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
        [here|term = tr^ue && false|],
        True,
        Left (Term.Boolean True)
      ),
      ( "Binary Op rhs",
        [here|term = true && fa^lse|],
        True,
        Left (Term.Boolean False)
      ),
      ( "Custom Op lhs",
        [here|
a &&& b = a && b
term = tr^ue &&& false
|],
        True,
        Left (Term.Boolean True)
      ),
      ( "Simple type annotation on non-typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = "this won't typecheck"
|],
        False,
        Right (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Simple type annotation on typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = This
|],
        True,
        Right (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations within bindings for do-block elements",
        [here|
term = do
  first = false
  second = tr^ue
  first && second
        |],
        True,
        Left (Term.Boolean True)
      ),
      ( "Test annotations within bindings for let-block elements",
        [here|
term = let
  first = false
  second = tr^ue
  first && second
        |],
        True,
        Left (Term.Boolean True)
      ),
      ( "Test annotations within actions for let-block elements",
        [here|
term = let
  first = false
  first && tr^ue
        |],
        True,
        Left (Term.Boolean True)
      ),
      ( "Test annotations for blocks with destructing binds",
        [here|
term = let
  (first, second) = (false, true)
  (third, fourth) = (false, tr^ue)
  first && second && third && fourth
        |],
        True,
        Left (Term.Boolean True)
      ),
      ( "Test annotations for blocks recursive binds",
        [here|
term = let
  f x = g true && x
  g y = f fal^se && y
  f true
        |],
        True,
        Left (Term.Boolean False)
      )
    ]

-- | Test helper which lets you specify a cursor position inline with source text as a '|'.
extractCursor :: Text -> Test (Lexer.Pos, Text)
extractCursor txt =
  case Text.splitOn "^" txt of
    [before, after] ->
      let col = Text.length $ Text.takeWhileEnd (/= '\n') before
          line = Prelude.length $ Text.lines before
       in pure $ (Lexer.Pos line col, before <> after)
    _ -> crash "expected exactly one cursor"

makeNodeSelectionTest :: (String, Text, Bool, Either ((Term.F Symbol Ann Ann (Term Symbol Ann))) (Type.F (Type Symbol Ann))) -> Test ()
makeNodeSelectionTest (name, testSrc, testTypechecked, expected) = scope name $ do
  (pos, src) <- extractCursor testSrc
  (notes, mayParsedFile, mayTypecheckedFile) <- withTestCodebase \codebase -> do
    let generateUniqueName = Parser.uniqueBase32Namegen <$> Random.getSystemDRG
    let ambientAbilities = []
    let parseNames = mempty
    let lexedSource = (src, L.lexer name (Text.unpack src))
    r <- Typecheck.typecheckHelper codebase generateUniqueName ambientAbilities parseNames (Text.pack name) lexedSource
    let Result.Result notes mayResult = r
    let (parsedFile, typecheckedFile) = case mayResult of
          Nothing -> (Nothing, Nothing)
          Just (Left uf) -> (Just uf, Nothing)
          Just (Right tf) -> (Just $ UF.discardTypes tf, Just tf)
    pure (notes, parsedFile, typecheckedFile)
  scope "parsed file" $ do
    pf <- maybe (crash (show ("Failed to parse" :: String, notes))) pure mayParsedFile
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
