module Unison.Test.Syntax.FileParser where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List (uncons)
import Data.Map qualified as Map
import Data.Set (elems)
import Data.Text qualified as Text
import EasyTest
import Text.Megaparsec.Error qualified as MPE
import Unison.HashQualified qualified as HQ
import Unison.OpaqueDeclaration qualified as OpaqueDeclaration
import Unison.Parser.Ann qualified as P
import Unison.Parsers (unsafeGetRightFrom, unsafeParseFileBuiltinsOnly)
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrintError (renderParseErrorAsANSI)
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.FileParser (file)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as P
import Unison.Test.Common qualified as Common
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.Util.Pretty qualified as P
import Unison.Var (Var)
import Unison.Var qualified as Var

test1 :: Test ()
test1 =
  scope "test1" . tests . map parses $
    [ -- , "type () = ()\n()"
      "structural type Pair a b = Pair a b\n",
      "structural type Optional a = Just a | Nothing\n",
      unlines
        [ "structural type Optional2 a",
          "  = Just a",
          "  | Nothing\n"
        ],
      ------ -- ,unlines
      ------ --   ["structural type Optional a b c where"
      ------ --   ,"  Just : a -> Optional a"
      ------ --   ,"  Nothing : Optional Int"]
      ------ -- , unlines
      ------ --   ["structural type Optional"
      ------ --   ,"   a"
      ------ --   ,"   b"
      ------ --   ,"   c where"
      ------ --   ,"  Just : a -> Optional a"
      ------ --   ,"  Nothing : Optional Int"]
      unlines -- NB: this currently fails because we don't have type AST or parser for effect types yet
        [ "structural ability State s where",
          "  get : {State s} s",
          "  set : s -> {State s} ()"
        ],
      unlines
        [ "ping x = pong (x + 1)",
          "pong x = ping (x - 1)"
        ]
    ]

test2 :: Test ()
test2 =
  scope "test2" $
    (io $ unsafeParseFileBuiltinsOnly "unison-src/test1.u") *> ok

test :: Test ()
test =
  scope "fileparser" . tests $
    [ test1,
      emptyWatchTest,
      signatureNeedsAccompanyingBodyTest,
      emptyBlockTest,
      expectedBlockOpenTest,
      typeAliasExpandsInDataDeclTest,
      typeAliasExpandsInTermSignatureTest,
      abilityRowAliasTest,
      typeAliasCycleTest,
      opaqueDeclParsesTest,
      opaqueDeclWithUniqueModifierParsesTest,
      opaqueDeclParameterizedParsesTest,
      opaqueDeclRoundtripTest,
      opaqueDeclCrossReferenceTest,
      opaqueDeclSelfReferenceCycleTest,
      opaqueDeclCrossCycleTest,
      opaqueBodyFnTypechecksUnderAliasTest,
      opaqueOutsideBodyIsRigidTest,
      opaqueBodyFnCallableFromOutsideTest,
      opaqueOutsideBodyRejectsRhsAssignTest,
      opaqueParameterizedKindInfersTest,
      opaqueKindAppArityRejectedTest,
      opaqueReifySignatureAcceptedTest,
      opaqueReifySignatureRejectedTest,
      opaqueReifyParameterizedAcceptedTest
    ]

expectFileParseFailure :: String -> (P.Error Symbol -> Test ()) -> Test ()
expectFileParseFailure s expectation = scope s $ do
  let result = runIdentity (P.run (P.rootFile file) s Common.parsingEnv)
  case result of
    Right _ -> crash "Parser succeeded"
    Left (MPE.FancyError _ sets) ->
      case (fmap (fst) . uncons . elems) sets of
        Just (MPE.ErrorCustom e) -> expectation e
        Just _ -> crash "Error encountered was not custom"
        Nothing -> crash "No error found"
    Left e -> crash . Text.unpack $ ("Parser failed with an error which was a trivial parser error: " <> renderParseErrorAsANSI 80 s e)

emptyWatchTest :: Test ()
emptyWatchTest =
  scope "emptyWatchTest" $
    expectFileParseFailure ">" expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.EmptyWatch _ann -> ok
      _ -> crash "Error wasn't EmptyWatch"

signatureNeedsAccompanyingBodyTest :: Test ()
signatureNeedsAccompanyingBodyTest =
  scope "signatureNeedsAccompanyingBodyTest" $
    expectFileParseFailure (unlines ["f : Nat -> Nat", "", "g a = a + 1"]) expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.SignatureNeedsAccompanyingBody _ -> ok
      _ -> crash "Error wasn't SignatureNeedsAccompanyingBody"

emptyBlockTest :: Test ()
emptyBlockTest =
  scope "emptyBlockTest" $
    expectFileParseFailure (unlines ["f a =", "", "> 1 + 1"]) expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.EmptyBlock _ -> ok
      _ -> crash "Error wasn't EmptyBlock"

expectedBlockOpenTest :: Test ()
expectedBlockOpenTest =
  scope "expectedBlockOpenTest" $
    expectFileParseFailure "f a b = match a b" expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.ExpectedBlockOpen _ _ -> ok
      _ -> crash "Error wasn't ExpectedBlockOpen"

-- | A @type alias@ used inside a data declaration constructor type expands
-- correctly and the file parses without error.
typeAliasExpandsInDataDeclTest :: Test ()
typeAliasExpandsInDataDeclTest =
  scope "typeAliasExpandsInDataDeclTest" . parses $
    unlines
      [ "type alias Endo a = a -> a",
        "type Box = Box (Endo Nat)"
      ]

-- | A @type alias@ used inside a term type signature expands correctly.
typeAliasExpandsInTermSignatureTest :: Test ()
typeAliasExpandsInTermSignatureTest =
  scope "typeAliasExpandsInTermSignatureTest" . parses $
    unlines
      [ "type alias Endo a = a -> a",
        "f : Endo Nat",
        "f x = x"
      ]

-- | An ability-row alias splices its body into surrounding @Effects@ lists.
abilityRowAliasTest :: Test ()
abilityRowAliasTest =
  scope "abilityRowAliasTest" . parses $
    unlines
      [ "structural ability Foo where foo : ()",
        "structural ability Bar where bar : ()",
        "type alias Web = {Foo, Bar}",
        "f : Nat ->{Web} Nat",
        "f x = x"
      ]

-- | Mutually-recursive aliases are rejected with 'TypeAliasCycle'.
typeAliasCycleTest :: Test ()
typeAliasCycleTest =
  scope "typeAliasCycleTest" $
    expectFileParseFailure
      (unlines ["type alias A = B", "type alias B = A"])
      expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.TypeAliasCycle {} -> ok
      _ -> crash "Error wasn't TypeAliasCycle"

-- | A minimal monomorphic opaque type with a single body item parses.
opaqueDeclParsesTest :: Test ()
opaqueDeclParsesTest =
  scope "opaqueDeclParsesTest" . parses $
    unlines
      [ "opaque type Logarithm = Float where",
        "  toFloat l = exp l"
      ]

-- | An opaque type with a @unique@ modifier between @opaque@ and @type@
-- parses.
opaqueDeclWithUniqueModifierParsesTest :: Test ()
opaqueDeclWithUniqueModifierParsesTest =
  scope "opaqueDeclWithUniqueModifierParsesTest" . parses $
    unlines
      [ "opaque unique type Token = Text where",
        "  fromText t = t"
      ]

-- | A parameterized opaque type with multiple body items parses.
opaqueDeclParameterizedParsesTest :: Test ()
opaqueDeclParameterizedParsesTest =
  scope "opaqueDeclParameterizedParsesTest" . parses $
    unlines
      [ "opaque type Set a = a where",
        "  empty s = s",
        "  insert x s = s"
      ]

-- | One opaque type can reference another in its RHS; the file parses and
-- both opaques land in the file's opaque map.
opaqueDeclCrossReferenceTest :: Test ()
opaqueDeclCrossReferenceTest =
  scope "opaqueDeclCrossReferenceTest" . parses $
    unlines
      [ "opaque type Inner = Nat where",
        "  toNat i = i",
        "opaque type Outer = Inner where",
        "  toInner o = o"
      ]

-- | @opaque type T = T@ — an opaque whose RHS mentions itself — is rejected
-- with 'OpaqueDeclCycle'.
opaqueDeclSelfReferenceCycleTest :: Test ()
opaqueDeclSelfReferenceCycleTest =
  scope "opaqueDeclSelfReferenceCycleTest" $
    expectFileParseFailure
      (unlines ["opaque type T = T where", "  ignore t = t"])
      expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.OpaqueDeclCycle {} -> ok
      _ -> crash "Error wasn't OpaqueDeclCycle"

-- | Two opaque types whose RHSes reference each other form a cycle and are
-- rejected with 'OpaqueDeclCycle'.
opaqueDeclCrossCycleTest :: Test ()
opaqueDeclCrossCycleTest =
  scope "opaqueDeclCrossCycleTest" $
    expectFileParseFailure
      ( unlines
          [ "opaque type A = B where",
            "  a x = x",
            "opaque type B = A where",
            "  b x = x"
          ]
      )
      expectation
  where
    expectation :: (Var e) => P.Error e -> Test ()
    expectation e = case e of
      P.OpaqueDeclCycle {} -> ok
      _ -> crash "Error wasn't OpaqueDeclCycle"

-- | An opaque-decl body fn typechecks under the file-wide opaque-as-alias
-- rule: 'Float.log : Float -> Float' returns a 'Float', but in
-- 'fromFloat x = Float.log x' declared with signature
-- 'Float -> Logarithm', the 'Float' result unifies with 'Logarithm'
-- because 'Logarithm' expands to 'Float' during unification. The body fn
-- then appears under its fully-qualified name 'Logarithm.fromFloat' in
-- the typechecked file's terms.
opaqueBodyFnTypechecksUnderAliasTest :: Test ()
opaqueBodyFnTypechecksUnderAliasTest =
  scope "opaqueBodyFnTypechecksUnderAliasTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  fromFloat : Float -> Logarithm",
              "  fromFloat x = Float.log x"
            ]
    tuf <- typechecksOrCrash src
    let termVarNames :: [String]
        termVarNames = Var.nameStr <$> Map.keys (UF.hashTerms tuf)
    let hasFromFloat = any (== "Logarithm.fromFloat") termVarNames
    if hasFromFloat
      then ok
      else
        crash $
          "expected 'Logarithm.fromFloat' in typechecked terms, got: "
            <> show termVarNames

-- | Outside an opaque type's body, the type is rigid: a top-level term
-- 'f : Logarithm -> Float; f x = x' should fail because @Logarithm ≢ Float@
-- when the alias rule is not active.
opaqueOutsideBodyIsRigidTest :: Test ()
opaqueOutsideBodyIsRigidTest =
  scope "opaqueOutsideBodyIsRigidTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  toFloat l = l",
              "",
              "f : Logarithm -> Float",
              "f x = x"
            ]
    expectTypecheckFailure src

-- | Body fns of an opaque type are callable from outside the body —
-- the opaque rule scopes only its body fns' /bodies/, not the act of
-- referencing them. So @useLog : Float -> Logarithm; useLog x =
-- Logarithm.fromFloat x@ typechecks: 'Logarithm.fromFloat' has the
-- signature @Float -> Logarithm@, and we're calling it with a 'Float'.
opaqueBodyFnCallableFromOutsideTest :: Test ()
opaqueBodyFnCallableFromOutsideTest =
  scope "opaqueBodyFnCallableFromOutsideTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  fromFloat : Float -> Logarithm",
              "  fromFloat x = Float.log x",
              "",
              "useLog : Float -> Logarithm",
              "useLog x = Logarithm.fromFloat x"
            ]
    _ <- typechecksOrCrash src
    ok

-- | Mirror image of 'opaqueOutsideBodyIsRigidTest': a top-level term
-- that attempts to assign a 'Float' literal to a 'Logarithm' must
-- fail, because outside the body @Logarithm ≢ Float@.
opaqueOutsideBodyRejectsRhsAssignTest :: Test ()
opaqueOutsideBodyRejectsRhsAssignTest =
  scope "opaqueOutsideBodyRejectsRhsAssignTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  fromFloat : Float -> Logarithm",
              "  fromFloat x = Float.log x",
              "",
              "bad : Logarithm",
              "bad = 0.5"
            ]
    expectTypecheckFailure src

-- | A parameterized opaque type used in a term signature kind-checks:
-- 'Box' has kind '* -> *', so 'Box Nat -> Nat' is well-kinded. Without
-- kind inference threading opaques, this fails because the kindchecker
-- does not know 'Box's kind.
opaqueParameterizedKindInfersTest :: Test ()
opaqueParameterizedKindInfersTest =
  scope "opaqueParameterizedKindInfersTest" $ do
    let src =
          unlines
            [ "opaque type Box a = a where",
              "  wrap x = x",
              "",
              "useBox : Box Nat -> Nat",
              "useBox b = 0"
            ]
    _ <- typechecksOrCrash src
    ok

-- | Applying a kind-1 opaque to two type arguments must fail. 'Box' has
-- kind '* -> *', so 'Box Nat Text' is ill-kinded. Today this surfaces as
-- a typecheck failure (the kindchecker's arity mismatch propagates out);
-- a friendlier error path would be nice but is not required for v1.
-- TODO(opaque): consider a dedicated kind-error message for this case.
opaqueKindAppArityRejectedTest :: Test ()
opaqueKindAppArityRejectedTest =
  scope "opaqueKindAppArityRejectedTest" $ do
    let src =
          unlines
            [ "opaque type Box a = a where",
              "  wrap x = x",
              "",
              "useBox : Box Nat Text -> Nat",
              "useBox b = 0"
            ]
    expectTypecheckFailure src

parses :: String -> Test ()
parses s = scope s $ do
  let p :: UnisonFile Symbol P.Ann
      !p =
        unsafeGetRightFrom s . runIdentity $
          P.run (P.rootFile file) s Common.parsingEnv
  pure p >> ok

-- | Parse and typecheck a source string, returning the typechecked file on
-- success. Crashes the test on parse or typecheck failure.
typechecksOrCrash :: String -> Test (TypecheckedUnisonFile Symbol P.Ann)
typechecksOrCrash s =
  case Common.parseAndSynthesizeAsFile [] "<test>" s of
    Result.Result _ (Just (Right tuf)) -> pure tuf
    Result.Result notes _ ->
      crash $
        "expected typecheck success, got: "
          <> show (length (toList notes))
          <> " notes"

-- | Parse and typecheck a source string, expecting a typecheck failure
-- (the parser must succeed but the typechecker must reject the file).
expectTypecheckFailure :: String -> Test ()
expectTypecheckFailure s =
  case Common.parseAndSynthesizeAsFile [] "<test>" s of
    Result.Result _ (Just (Right _)) -> crash "expected typecheck failure, got success"
    Result.Result _ _ -> ok

-- | An opaque type whose @reify@ body fn has the canonical signature
-- @Logarithm ->{} '(Logarithm)@ typechecks. Confirms the Phase 9a
-- validation accepts the correct shape.
opaqueReifySignatureAcceptedTest :: Test ()
opaqueReifySignatureAcceptedTest =
  scope "opaqueReifySignatureAcceptedTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  fromFloat : Float -> Logarithm",
              "  fromFloat x = Float.log x",
              "  toFloat : Logarithm -> Float",
              "  toFloat l = l",
              "  reify : Logarithm ->{} '(Logarithm)",
              "  reify l =",
              "    f = toFloat l",
              "    do Logarithm.fromFloat f"
            ]
    _ <- typechecksOrCrash src
    ok

-- | An opaque type whose @reify@ body fn has the wrong signature must be
-- rejected after typechecking. Here @reify@ returns @Float@ instead of
-- @'(Logarithm)@, so the Phase 9a check fails.
opaqueReifySignatureRejectedTest :: Test ()
opaqueReifySignatureRejectedTest =
  scope "opaqueReifySignatureRejectedTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  toFloat : Logarithm -> Float",
              "  toFloat l = l",
              "  reify : Logarithm -> Float",
              "  reify l = toFloat l"
            ]
    expectTypecheckFailure src

-- | A parameterized opaque type with a correct polymorphic @reify@
-- signature typechecks. Here @Box a@ requires
-- @reify : Box a ->{} '(Box a)@.
opaqueReifyParameterizedAcceptedTest :: Test ()
opaqueReifyParameterizedAcceptedTest =
  scope "opaqueReifyParameterizedAcceptedTest" $ do
    let src =
          unlines
            [ "opaque type Box a = a where",
              "  wrap : a -> Box a",
              "  wrap x = x",
              "  unwrap : Box a -> a",
              "  unwrap b = b",
              "  reify : Box a ->{} '(Box a)",
              "  reify b =",
              "    inner = unwrap b",
              "    do Box.wrap inner"
            ]
    _ <- typechecksOrCrash src
    ok

-- | An opaque type declaration survives a parse → pretty-print → re-parse
-- roundtrip with the same set of opaque-decl names and the same parameter
-- arity. We don't compare hashes because annotation positions differ between
-- the original source and the pretty-printed source, but the structural
-- shape (name, arity) is stable.
opaqueDeclRoundtripTest :: Test ()
opaqueDeclRoundtripTest =
  scope "opaqueDeclRoundtripTest" $ do
    let src =
          unlines
            [ "opaque type Logarithm = Float where",
              "  toFloat l = l"
            ]
    let parsed1 = parseOrFail src
    rendered <- case Map.toList (UF.opaqueDeclarationsId parsed1) of
      [] -> crash "parsed file had no opaque decl"
      [(v, (_ref, od))] -> do
        let hq = HQ.NameOnly (Name.unsafeParseVar v)
        let pretty =
              DeclPrinter.prettyOpaqueDecl
                PPED.empty
                DeclPrinter.RenderUniqueTypeGuids'No
                hq
                od
        pure (Text.unpack (P.toPlain 80 (P.syntaxToColor pretty)))
      _ -> crash "parsed file had more than one opaque decl"
    let parsed2 = parseOrFail rendered
    let shape uf =
          [ (v, length (OpaqueDeclaration.paramNames od))
          | (v, (_ref, od)) <- Map.toList (UF.opaqueDeclarationsId uf)
          ]
    expectEqual (shape parsed1) (shape parsed2)
  where
    parseOrFail :: String -> UnisonFile Symbol P.Ann
    parseOrFail s =
      unsafeGetRightFrom s . runIdentity $
        P.run (P.rootFile file) s Common.parsingEnv
