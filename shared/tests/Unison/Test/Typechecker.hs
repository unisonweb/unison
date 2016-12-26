{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker where

import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Codebase.MemCodebase ()
import Unison.Symbol (Symbol)
import Unison.Term as E
import Unison.Paths (Path)
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.View (DFO)
import Unison.Reference (Reference)
import Unison.DataDeclaration (DataDeclaration)
import qualified Unison.Codebase as Codebase
import qualified Unison.Note as Note
import qualified Unison.Parsers as Parsers
import qualified Unison.Paths as Paths
import qualified Unison.Test.Common as Common
import qualified Unison.Test.Term as Term

type V = Symbol DFO
type TTerm = Term.TTerm
type TType = Type V
type TEnv f = T.Env f V
type TCodebase = IO Common.TCodebase

infixr 1 -->
(-->) :: TType -> TType -> TType
(-->) = T.arrow

data StrongEq = StrongEq TType
instance Eq StrongEq where StrongEq t1 == StrongEq t2 = Typechecker.equals t1 t2
instance Show StrongEq where show (StrongEq t) = show t

env :: TCodebase -> TEnv IO
env codebase r = do
  (codebase, _, _, _) <- Note.lift codebase
  Codebase.typeAt codebase (E.ref r) mempty

decls :: TCodebase -> Reference -> Note.Noted IO (DataDeclaration V)
decls code r = do
  (code, _, _, _) <- Note.lift code
  Codebase.dataDeclaration code r

localsAt :: TCodebase -> Path -> TTerm -> IO [(V, Type V)]
localsAt codebase path e = Note.run $ Typechecker.locals (env codebase) (decls codebase) path e

synthesizesAt :: TCodebase -> Path -> TTerm -> TType -> Assertion
synthesizesAt codebase path e t = Note.run $ do
  (codebase, _, _, _) <- Note.lift codebase
  t2 <- Codebase.typeAt codebase e path
  _ <- Note.fromEither (Typechecker.subtype t2 t)
  _ <- Note.fromEither (Typechecker.subtype t t2)
  pure ()

checksAt :: TCodebase -> Path -> TTerm -> TType -> Assertion
checksAt code path e t = Note.run . void $
  Typechecker.synthesize (env code) (decls code) (Paths.modifyTerm' (\e -> E.wrapV (E.ann e t)) path e)

synthesizesAndChecksAt :: TCodebase -> Path -> TTerm -> TType -> Assertion
synthesizesAndChecksAt code path e t =
  synthesizesAt code path e t >> checksAt code path e t

synthesizes :: TCodebase -> TTerm -> TType -> Assertion
synthesizes code e t = Note.run $ do
  t2 <- Typechecker.synthesize (env code) (decls code) e
  _ <- Note.fromEither (Typechecker.subtype t2 t)
  _ <- Note.fromEither (Typechecker.subtype t t2)
  pure ()

checks :: TCodebase -> TTerm -> TType -> Assertion
checks code e t = void $ Note.run (Typechecker.check (env code) (decls code) e t)

checkSubtype :: TType -> TType -> Assertion
checkSubtype t1 t2 = case Typechecker.subtype t1 t2 of
  Left err -> assertFailure ("subtype failure:\n" ++ show err)
  Right _ -> pure ()

synthesizesAndChecks :: TCodebase -> TTerm -> TType -> Assertion
synthesizesAndChecks code e t =
  synthesizes code e t >> checks code e t

--singleTest = withResource Common.node (\_ -> pure ()) $ \node -> testGroup "Typechecker"
--  [
--   testTerm "f -> let x = (let saved = f; 42); 1" $ \tms ->
--    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks node
--      (unsafeParseTerm tms)
--      (unsafeParseType "forall x. x -> Number")
--  ]

testTerm :: String -> (String -> TestTree) -> TestTree
testTerm termString f = f termString

unsafeParseTerm :: String -> TTerm
unsafeParseTerm = Parsers.unsafeParseTerm

unsafeParseType :: String -> TType
unsafeParseType = Parsers.unsafeParseType

tests :: TestTree
tests = withResource Common.codebase (\_ -> pure ()) $ \code -> testGroup "Typechecker"
  [
    testCase "alpha equivalence (type)" $ assertEqual "const"
      (unsafeParseType "forall a b . a -> b -> a")
      (unsafeParseType "forall x y . x -> y -> x")
  , testCase "subtype (1)" $ checkSubtype
      (unsafeParseType "Number")
      (unsafeParseType "Number")
  , testCase "subtype (2)" $ checkSubtype
      (unsafeParseType "forall a . a")
      (unsafeParseType "Number")
  , testCase "subtype (3)" $ checkSubtype
      (unsafeParseType "forall a . a")
      (unsafeParseType "forall a . a")
  , testCase "strong equivalence (type)" $ assertEqual "types were not equal"
      (StrongEq (unsafeParseType "forall a b . a -> b -> a"))
      (StrongEq (unsafeParseType "forall y x . x -> y -> x"))
  , testTerm "42" $ \tms -> testCase ("synthesize/check" ++ tms) $ synthesizesAndChecks code
      (unsafeParseTerm tms)
      (unsafeParseType "Number")
  , testCase "synthesize/check Term.id" $ synthesizesAndChecks code
      (unsafeParseTerm "a -> a")
      (unsafeParseType "forall b . b -> b")
  , testCase "synthesize/check Term.const" $ synthesizesAndChecks code
      (unsafeParseTerm "x y -> x")
      (unsafeParseType "forall a b . a -> b -> a")
  , testCase "synthesize/check (x y -> y)" $ synthesizesAndChecks code
      (unsafeParseTerm "x y -> y")
      (unsafeParseType "forall a b . a -> b -> b")
  , testCase "synthesize/check (let f = (+); f 1;;)" $ synthesizesAndChecks code
      (unsafeParseTerm "let { f = (+); f 1 }")
      (T.lit T.Number --> T.lit T.Number)
  , testCase "synthesize/check (let { blank x = _; blank 1 })" $ synthesizesAndChecks code
      (unsafeParseTerm "let { blank x = _; blank 1 }")
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check Term.fix" $ synthesizesAndChecks code
      (unsafeParseTerm "let rec { fix f = f (fix f); fix }")
      (forall' ["a"] $ (T.v' "a" --> T.v' "a") --> T.v' "a")
  , testCase "synthesize/check Term.pingpong1" $ synthesizesAndChecks code
      Term.pingpong1
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check [1,2,1+1]" $ synthesizesAndChecks code
      (unsafeParseTerm "[1, 2, 1 + 1]")
      (T.lit T.Vector `T.app` T.lit T.Number)
  , testTerm "[1, 2, 1 + 1]" $ \tms ->
    testCase ("synthesize/checkAt "++tms++"@[Paths.Arg, Index 2]") $ synthesizesAndChecksAt code
      [Paths.Arg, Paths.Index 2] (unsafeParseTerm tms) (T.lit T.Number)
  , testTerm "let { x = _; _}" $ \tms ->
    testCase ("synthesize/checkAt ("++tms++")@[Binding 0,Body]") $ synthesizesAndChecksAt code
      [Paths.Binding 0, Paths.Body] (unsafeParseTerm tms) unconstrained
  -- fails
  , testTerm "f -> let { x = let { saved = f; 42 }; 1 }" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks code
      (unsafeParseTerm tms)
      (unsafeParseType "forall x . x -> Number")
  , testTerm "f -> let { x = (b a -> b) 42 f; 1 }" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks code
      (unsafeParseTerm tms) (unsafeParseType "forall x . x -> Number")
  , testTerm "f x y -> (x y -> y) f _ + _" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ do
      synthesizesAndChecks code
        (unsafeParseTerm tms)
        (unsafeParseType "forall a b c . a -> b -> c -> Number")
  , testTerm "(id -> let { x = id 42; y = id \"hi\"; 43 }) : (forall a . a -> a) -> Number" $ \tms ->
    testCase ("higher rank checking: " ++ tms) $
      let
        t = unsafeParseType "(forall a . a -> a) -> Number"
        tm = unsafeParseTerm tms
      in synthesizesAndChecks code tm t
  -- Let generalization not implemented yet; this test fails
  --, testCase "let generalization: let id a = a; x = id 42; y = id 'hi'; 23" $
  --    let
  --      tm = E.let1'
  --        [ ("id", E.lam' ["a"] (E.var' "a") `E.ann` T.forall' ["a"] (T.v' "a")),
  --          ("id@Number", E.var' "id" `E.app` E.num 42),
  --          ("id@Text", E.var' "id" `E.app` E.text "hi")
  --        ] (E.num 43)
  --    in synthesizesAndChecks code tm $ T.lit T.Number
  , testTerm "x y -> _ + _" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Body,Fn,Arg]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt), (_,yt)] <- localsAt code [Paths.Body, Paths.Body, Paths.Fn, Paths.Arg] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
      assertEqual "yt unconstrainted" unconstrained (T.generalize yt)
  , testTerm "let { x = _; _ }" $ \tms ->
    testCase ("locals ("++tms++")") $ do
      let tm = unsafeParseTerm tms
      [(_,xt)] <- localsAt code [Paths.Body] tm
      [] <- localsAt code [Paths.Binding 0, Paths.Body] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
  , testTerm "let { x = _; y = _; _ }" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Body]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt), (_,yt)] <- localsAt code [Paths.Body, Paths.Body] tm
      assertEqual "xt unconstrained" unconstrained (T.generalize xt)
      assertEqual "yt unconstrained" unconstrained (T.generalize yt)
  , testTerm "let { x = _; y = _; _ }" $ \tms ->
    -- testTerm "let x = 42; y = _; _" $ \tms ->
    -- testTerm "let x = 42; y = 43; _" $ \tms ->
    -- testTerm "let x = 42; y = 43; 4224" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Binding 0,Body]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt)] <- localsAt code [Paths.Body, Paths.Binding 0, Paths.Body] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
  ]

unconstrained :: TType
unconstrained = unsafeParseType "forall a . a"

main :: IO ()
main = defaultMain tests
-- main = defaultMain singleTest
