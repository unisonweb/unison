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

localsAt :: TCodebase -> Path -> TTerm -> IO [(V, Type V)]
localsAt codebase path e = Note.run $ do
  t2 <- Typechecker.locals (env codebase) path e
  pure t2

synthesizesAt :: TCodebase -> Path -> TTerm -> TType -> Assertion
synthesizesAt codebase path e t = Note.run $ do
  (codebase, _, _, _) <- Note.lift codebase
  t2 <- Codebase.typeAt codebase e path
  _ <- Note.fromEither (Typechecker.subtype t2 t)
  _ <- Note.fromEither (Typechecker.subtype t t2)
  pure ()

checksAt :: TCodebase -> Path -> TTerm -> TType -> Assertion
checksAt node path e t = Note.run . void $
  Typechecker.synthesize (env node) (Paths.modifyTerm' (\e -> E.wrapV (E.ann e t)) path e)

synthesizesAndChecksAt :: TCodebase -> Path -> TTerm -> TType -> Assertion
synthesizesAndChecksAt node path e t =
  synthesizesAt node path e t >> checksAt node path e t

synthesizes :: TCodebase -> TTerm -> TType -> Assertion
synthesizes node e t = Note.run $ do
  t2 <- Typechecker.synthesize (env node) e
  _ <- Note.fromEither (Typechecker.subtype t2 t)
  _ <- Note.fromEither (Typechecker.subtype t t2)
  pure ()

checks :: TCodebase -> TTerm -> TType -> Assertion
checks node e t = void $ Note.run (Typechecker.check (env node) e t)

checkSubtype :: TType -> TType -> Assertion
checkSubtype t1 t2 = case Typechecker.subtype t1 t2 of
  Left err -> assertFailure ("subtype failure:\n" ++ show err)
  Right _ -> pure ()

synthesizesAndChecks :: TCodebase -> TTerm -> TType -> Assertion
synthesizesAndChecks node e t =
  synthesizes node e t >> checks node e t

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
tests = withResource Common.codebase (\_ -> pure ()) $ \node -> testGroup "Typechecker"
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
  , testTerm "42" $ \tms -> testCase ("synthesize/check" ++ tms) $ synthesizesAndChecks node
      (unsafeParseTerm tms)
      (unsafeParseType "Number")
  , testCase "synthesize/check Term.id" $ synthesizesAndChecks node
      (unsafeParseTerm "a -> a")
      (unsafeParseType "forall b . b -> b")
  , testCase "synthesize/check Term.const" $ synthesizesAndChecks node
      (unsafeParseTerm "x y -> x")
      (unsafeParseType "forall a b . a -> b -> a")
  , testCase "synthesize/check (x y -> y)" $ synthesizesAndChecks node
      (unsafeParseTerm "x y -> y")
      (unsafeParseType "forall a b . a -> b -> b")
  , testCase "synthesize/check (let f = (+); f 1;;)" $ synthesizesAndChecks node
      (unsafeParseTerm "let { f = (+); f 1 }")
      (T.lit T.Number --> T.lit T.Number)
  , testCase "synthesize/check (let { blank x = _; blank 1 })" $ synthesizesAndChecks node
      (unsafeParseTerm "let { blank x = _; blank 1 }")
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check Term.fix" $ synthesizesAndChecks node
      (unsafeParseTerm "let rec { fix f = f (fix f); fix }")
      (forall' ["a"] $ (T.v' "a" --> T.v' "a") --> T.v' "a")
  , testCase "synthesize/check Term.pingpong1" $ synthesizesAndChecks node
      Term.pingpong1
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check [1,2,1+1]" $ synthesizesAndChecks node
      (unsafeParseTerm "[1, 2, 1 + 1]")
      (T.lit T.Vector `T.app` T.lit T.Number)
  , testTerm "[1, 2, 1 + 1]" $ \tms ->
    testCase ("synthesize/checkAt "++tms++"@[Paths.Arg, Index 2]") $ synthesizesAndChecksAt node
      [Paths.Arg, Paths.Index 2] (unsafeParseTerm tms) (T.lit T.Number)
  , testTerm "let { x = _; _}" $ \tms ->
    testCase ("synthesize/checkAt ("++tms++")@[Binding 0,Body]") $ synthesizesAndChecksAt node
      [Paths.Binding 0, Paths.Body] (unsafeParseTerm tms) unconstrained
  -- fails
  , testTerm "f -> let { x = let { saved = f; 42 }; 1 }" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks node
      (unsafeParseTerm tms)
      (unsafeParseType "forall x . x -> Number")
  , testTerm "f -> let { x = (b a -> b) 42 f; 1 }" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks node
      (unsafeParseTerm tms) (unsafeParseType "forall x . x -> Number")
  , testTerm "f x y -> (x y -> y) f _ + _" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ do
      synthesizesAndChecks node
        (unsafeParseTerm tms)
        (unsafeParseType "forall a b c . a -> b -> c -> Number")
  , testTerm "(id -> let { x = id 42; y = id \"hi\"; 43 }) : (forall a . a -> a) -> Number" $ \tms ->
    testCase ("higher rank checking: " ++ tms) $
      let
        t = unsafeParseType "(forall a . a -> a) -> Number"
        tm = unsafeParseTerm tms
      in synthesizesAndChecks node tm t
  -- Let generalization not implemented yet; this test fails
  --, testCase "let generalization: let id a = a; x = id 42; y = id 'hi'; 23" $
  --    let
  --      tm = E.let1'
  --        [ ("id", E.lam' ["a"] (E.var' "a") `E.ann` T.forall' ["a"] (T.v' "a")),
  --          ("id@Number", E.var' "id" `E.app` E.num 42),
  --          ("id@Text", E.var' "id" `E.app` E.text "hi")
  --        ] (E.num 43)
  --    in synthesizesAndChecks node tm $ T.lit T.Number
  , testTerm "x y -> _ + _" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Body,Fn,Arg]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt), (_,yt)] <- localsAt node [Paths.Body, Paths.Body, Paths.Fn, Paths.Arg] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
      assertEqual "yt unconstrainted" unconstrained (T.generalize yt)
  , testTerm "let { x = _; _ }" $ \tms ->
    testCase ("locals ("++tms++")") $ do
      let tm = unsafeParseTerm tms
      [(_,xt)] <- localsAt node [Paths.Body] tm
      [] <- localsAt node [Paths.Binding 0, Paths.Body] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
  , testTerm "let { x = _; y = _; _ }" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Body]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt), (_,yt)] <- localsAt node [Paths.Body, Paths.Body] tm
      assertEqual "xt unconstrained" unconstrained (T.generalize xt)
      assertEqual "yt unconstrained" unconstrained (T.generalize yt)
  , testTerm "let { x = _; y = _; _ }" $ \tms ->
    -- testTerm "let x = 42; y = _; _" $ \tms ->
    -- testTerm "let x = 42; y = 43; _" $ \tms ->
    -- testTerm "let x = 42; y = 43; 4224" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Binding 0,Body]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt)] <- localsAt node [Paths.Body, Paths.Binding 0, Paths.Body] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
  ]

unconstrained :: TType
unconstrained = unsafeParseType "forall a . a"

main :: IO ()
main = defaultMain tests
-- main = defaultMain singleTest
