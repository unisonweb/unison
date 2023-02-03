{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.LSP (test) where

import qualified Crypto.Random as Random
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
import qualified Unison.Parser.Ann as Ann
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import qualified Unison.Reference as Reference
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Parser as Parser
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.UnisonFile as UF
import Unison.Util.Monoid (foldMapM)

test :: Test ()
test = do
  scope "annotations" $
    tests
      [ refFinding,
        annotationNesting
      ]

trm :: Term.F Symbol () () (ABT.Term (Term.F Symbol () ()) Symbol ()) -> LSPQ.SourceNode ()
trm = LSPQ.TermNode . ABT.tm

typ :: Type.F (ABT.Term Type.F Symbol ()) -> LSPQ.SourceNode ()
typ = LSPQ.TypeNode . ABT.tm

pat :: Pattern.Pattern () -> LSPQ.SourceNode ()
pat = LSPQ.PatternNode

-- | Test that we can find the correct reference for a given cursor position.
refFinding :: Test ()
refFinding =
  scope "refs" . tests . fmap makeNodeSelectionTest $
    [ ( "Binary Op lhs",
        [here|term = tr^ue && false|],
        True,
        trm (Term.Boolean True)
      ),
      ( "Binary Op rhs",
        [here|term = true && fa^lse|],
        True,
        trm (Term.Boolean False)
      ),
      ( "Custom Op lhs",
        [here|
a &&& b = a && b
term = tr^ue &&& false
|],
        True,
        trm (Term.Boolean True)
      ),
      ( "Simple type annotation on non-typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = "this won't typecheck"
|],
        False,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Simple type annotation on typechecking file",
        [here|
structural type Thing = This | That
term : Thi^ng
term = This
|],
        True,
        typ (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations within bindings for do-block elements",
        [here|
term = do
  first = false
  second = tr^ue
  first && second
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations within bindings for let-block elements",
        [here|
term = let
  first = false
  second = tr^ue
  first && second
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations within actions for let-block elements",
        [here|
term = let
  first = false
  first && tr^ue
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations for blocks with destructuring binds",
        [here|
structural type Identity a = Identity a
term = let
  (Identity a) = Identity tr^ue
  a
        |],
        True,
        trm (Term.Boolean True)
      ),
      ( "Test annotations for destructuring tuples (they have a special parser)",
        [here|
term = let
  (true, fal^se)
        |],
        True,
        trm (Term.Boolean False)
      ),
      ( "Test annotations within pattern binds",
        [here|
term = let
  (third, tr^ue) = (false, true)
  true
  |],
        True,
        pat (Pattern.Boolean () True)
      ),
      ( "Test annotations for types with arrows",
        [here|
structural type Thing = This | That

term : Thing -> Thing -> Thi^ng
term a b = This
        |],
        True,
        Right (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for types with effects",
        [here|
unique ability Foo a where
    foo : a

unique ability Bar b where
    bar : b

structural type Thing = This | That

term : (Thing -> {Foo a, Bar b} Th^ing) -> {Foo a, Bar b} Thing
term f = f This
        |],
        True,
        Right (Type.Ref (Reference.unsafeFromText "#6kbe32g06nqg93cqub6ohqc4ql4o49ntgnunifds0t75qre6lacnbsr3evn8bkivj68ecbvmhkbak4dbg4fqertcpgb396rmo34tnh0"))
      ),
      ( "Test annotations for effects themselves",
        [here|
structural ability Foo a where
    foo : a

structural type Thing = This | That

term : () -> {F^oo a} Thing
term _ = This
        |],
        True,
        Right (Type.Ref (Reference.unsafeFromText "#h4uhcub76va4tckj1iccnsb07rh0fhgpigqapb4jh5n07s0tugec4nm2vikuv973mab7oh4ne07o6armcnnl7mbfjtb4imphgrjgimg"))
      ),
      ( "Test annotations for blocks recursive binds",
        [here|
term = let
  f x = g true && x
  g y = f fal^se && y
  f true
        |],
        True,
        trm (Term.Boolean False)
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

makeNodeSelectionTest :: (String, Text, Bool, LSPQ.SourceNode ()) -> Test ()
makeNodeSelectionTest (name, testSrc, testTypechecked, expected) = scope name $ do
  (pos, src) <- extractCursor testSrc
  (notes, mayParsedFile, mayTypecheckedFile) <- typecheckSrc name src
  scope "parsed file" $ do
    pf <- maybe (crash (show ("Failed to parse" :: String, notes))) pure mayParsedFile
    let pfResult =
          UF.terms pf
            & firstJust \(_v, trm) ->
              LSPQ.findSmallestEnclosingNode pos trm
    expectEqual (Just expected) (void <$> pfResult)

  when testTypechecked $
    scope "typechecked file" $ do
      tf <- maybe (crash "Failed to typecheck") pure mayTypecheckedFile
      let tfResult =
            UF.hashTermsId tf
              & toList
              & firstJust \(_refId, _wk, trm, _typ) ->
                LSPQ.findSmallestEnclosingNode pos trm
      expectEqual (Just expected) (void <$> tfResult)

-- | Tests which assert that the annotation for each ABT node spans at least the span of
-- its children, i.e. all child annotations are contained within the annotation of their parent.
annotationNesting :: Test ()
annotationNesting =
  scope "nesting" . tests . fmap annotationNestingTest $
    [ ( "let blocks",
        [here|
term = let
  x = true
  y = false
  true && false
|]
      ),
      ( "let-rec blocks",
        [here|
term = let
  x a = a && y true
  y b = b && x true
  x true && y true
|]
      ),
      ( "function bindings",
        [here|
term x y = x && y
|]
      )
    ]

annotationNestingTest :: (String, Text) -> Test ()
annotationNestingTest (name, src) = scope name do
  (_notes, _pf, maytf) <- typecheckSrc name src
  tf <- maybe (crash "Failed to typecheck") pure maytf
  UF.hashTermsId tf
    & toList
    & traverse_ \(_refId, _wk, trm, _typ) ->
      assertAnnotationsAreNested trm

-- | Asserts that for all nodes in the provided ABT, the annotations of all child nodes are
-- within the span of the parent node.
assertAnnotationsAreNested :: forall f. (Foldable f, Functor f, Show (f (Either String Ann))) => ABT.Term f Symbol Ann -> Test ()
assertAnnotationsAreNested term = do
  case ABT.cata alg term of
    Right _ -> pure ()
    Left err -> crash err
  where
    alg :: Ann -> ABT.ABT f Symbol (Either String Ann) -> Either String Ann
    alg ann abt = do
      childSpan <- abt & foldMapM id
      case ann `Ann.encompasses` childSpan of
        -- one of the annotations isn't in the file, don't bother checking.
        Nothing -> pure (ann <> childSpan)
        Just isInFile
          | isInFile -> pure ann
          | otherwise -> Left $ "Containment breach: children aren't contained with the parent:" <> show (ann, abt)

typecheckSrc :: String -> Text -> Test (Seq (Result.Note Symbol Ann), Maybe (UF.UnisonFile Symbol Ann), Maybe (UF.TypecheckedUnisonFile Symbol Ann))
typecheckSrc name src = do
  withTestCodebase \codebase -> do
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

withTestCodebase ::
  (Codebase IO Symbol Ann -> IO r) -> Test r
withTestCodebase action = do
  r <- io do
    tmp <- Temp.getCanonicalTemporaryDirectory
    tmpDir <- Temp.createTempDirectory tmp "lsp-test"
    Codebase.Init.withCreatedCodebase SC.init "lsp-test" tmpDir SC.DontLock action
  either (crash . show) pure r
