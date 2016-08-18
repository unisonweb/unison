{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker where

import Data.Functor
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Node.MemNode ()
import Unison.Symbol (Symbol)
import Unison.Term as E
import Unison.Parsers (unsafeParseTerm, unsafeParseType)
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.View (DFO)
import Unison.Paths (Path)
import qualified Unison.Node as Node
import qualified Unison.Note as Note
import qualified Unison.Paths as Paths
import qualified Unison.Test.Common as Common
import qualified Unison.Test.Term as Term

type V = Symbol DFO
type TTerm = Term.TTerm
type TType = Type V
type TEnv f = T.Env f V
type TNode = IO Common.TNode

infixr 1 -->
(-->) :: TType -> TType -> TType
(-->) = T.arrow

data StrongEq = StrongEq TType
instance Eq StrongEq where StrongEq t1 == StrongEq t2 = Typechecker.equals t1 t2
instance Show StrongEq where show (StrongEq t) = show t

env :: TNode -> TEnv IO
env node r = do
  (node, _, _) <- Note.lift node
  Node.typeAt node (E.ref r) mempty

localsAt :: TNode -> Path -> TTerm -> IO [(V, Type V)]
localsAt node path e = Note.run $ do
  t2 <- Typechecker.locals (env node) path e
  pure t2

synthesizesAt :: TNode -> Path -> TTerm -> TType -> Assertion
synthesizesAt node path e t = Note.run $ do
  (node, _, _) <- Note.lift node
  t2 <- Node.typeAt node e path
  _ <- Note.fromEither (Typechecker.subtype t2 t)
  _ <- Note.fromEither (Typechecker.subtype t t2)
  pure ()

checksAt :: TNode -> Path -> TTerm -> TType -> Assertion
checksAt node path e t = Note.run . void $
  Typechecker.synthesize (env node) (Paths.modifyTerm' (\e -> E.wrapV (E.ann e t)) path e)

synthesizesAndChecksAt :: TNode -> Path -> TTerm -> TType -> Assertion
synthesizesAndChecksAt node path e t =
  synthesizesAt node path e t >> checksAt node path e t

synthesizes :: TNode -> TTerm -> TType -> Assertion
synthesizes node e t = Note.run $ do
  t2 <- Typechecker.synthesize (env node) e
  _ <- Note.fromEither (Typechecker.subtype t2 t)
  _ <- Note.fromEither (Typechecker.subtype t t2)
  pure ()

checks :: TNode -> TTerm -> TType -> Assertion
checks node e t = void $ Note.run (Typechecker.check (env node) e t)

checkSubtype :: TType -> TType -> Assertion
checkSubtype t1 t2 = case Typechecker.subtype t1 t2 of
  Left err -> assertFailure ("subtype failure:\n" ++ show err)
  Right _ -> pure ()

synthesizesAndChecks :: TNode -> TTerm -> TType -> Assertion
synthesizesAndChecks node e t =
  synthesizes node e t >> checks node e t

--singleTest = withResource Common.node (\_ -> pure ()) $ \node -> testGroup "Typechecker"
--  [
--   testTerm "f -> let x = (let saved = f in 42) in 1" $ \tms ->
--    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks node
--      (unsafeParseTerm tms)
--      (unsafeParseType "forall x. x -> Number")
--  ]

testTerm :: String -> (String -> TestTree) -> TestTree
testTerm termString f = f termString

tests :: TestTree
tests = withResource Common.node (\_ -> pure ()) $ \node -> testGroup "Typechecker"
  [
    testCase "alpha equivalence (type)" $ assertEqual "const"
      (unsafeParseType "forall a b. a -> b -> a")
      (unsafeParseType "forall x y. x -> y -> x")
  , testCase "subtype (1)" $ checkSubtype
      (unsafeParseType "Number")
      (unsafeParseType "Number")
  , testCase "subtype (2)" $ checkSubtype
      (unsafeParseType "forall a. a")
      (unsafeParseType "Number")
  , testCase "subtype (3)" $ checkSubtype
      (unsafeParseType "forall a. a")
      (unsafeParseType "forall a. a")
  , testCase "strong equivalence (type)" $ assertEqual "types were not equal"
      (StrongEq (unsafeParseType "forall a b. a -> b -> a"))
      (StrongEq (unsafeParseType "forall y x. x -> y -> x"))
  , testTerm "42" $ \tms -> testCase ("synthesize/check" ++ tms) $ synthesizesAndChecks node
      (unsafeParseTerm tms)
      (unsafeParseType "Number")
  , testCase "synthesize/check Term.id" $ synthesizesAndChecks node
      (unsafeParseTerm "a -> a")
      (unsafeParseType "forall b. b -> b")
  , testCase "synthesize/check Term.const" $ synthesizesAndChecks node
      (unsafeParseTerm "x y -> x")
      (unsafeParseType "forall a b. a -> b -> a")
  , testCase "synthesize/check (x y -> y)" $ synthesizesAndChecks node
      (unsafeParseTerm "x y -> y")
      (unsafeParseType "forall a b. a -> b -> b")
  , testCase "synthesize/check (let f = (+) in f 1)" $ synthesizesAndChecks node
      (unsafeParseTerm "let f = (+) in f 1")
      (T.lit T.Number --> T.lit T.Number)
  , testCase "synthesize/check (let blank x = _ in blank 1)" $ synthesizesAndChecks node
      (unsafeParseTerm "let blank x = _ in blank 1")
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check Term.fix" $ synthesizesAndChecks node
      (unsafeParseTerm "let rec fix f = f (fix f) in fix")
      (forall' ["a"] $ (T.v' "a" --> T.v' "a") --> T.v' "a")
  , testCase "synthesize/check Term.pingpong1" $ synthesizesAndChecks node
      Term.pingpong1
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check [1,2,1+1]" $ synthesizesAndChecks node
      (unsafeParseTerm "[1, 2, 1 + 1]")
      (T.lit T.Vector `T.app` T.lit T.Number)
  , testTerm "[1, 2, 1 + 1]" $ \tms ->
    testCase ("synthesize/checkAt "++tms++"@[Index 2]") $ synthesizesAndChecksAt node
      [Paths.Index 2] (unsafeParseTerm tms) (T.lit T.Number)
  , testTerm "let x = _ in _" $ \tms ->
    testCase ("synthesize/checkAt ("++tms++")@[Binding 0,Body]") $ synthesizesAndChecksAt node
      [Paths.Binding 0, Paths.Body] (unsafeParseTerm tms) unconstrained
  -- fails
  , testTerm "f -> let x = (let saved = f in 42) in 1" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks node
      (unsafeParseTerm tms)
      (unsafeParseType "forall x. x -> Number")
  , testTerm "f -> let x = (b a -> b) 42 f in 1" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ synthesizesAndChecks node
      (unsafeParseTerm tms) (unsafeParseType "forall x. x -> Number")
  , testTerm "f x y -> (x y -> y) f _ + _" $ \tms ->
    testCase ("synthesize/check ("++tms++")") $ do
      synthesizesAndChecks node
        (unsafeParseTerm tms)
        (unsafeParseType "forall a b c. a -> b -> c -> Number")
  , testTerm "(id -> let x = id 42; y = id \"hi\" in 43) : (forall a . a -> a) -> Number" $ \tms ->
    testCase ("higher rank checking: " ++ tms) $
      let
        t = unsafeParseType "(forall a . a -> a) -> Number"
        tm = unsafeParseTerm tms
      in synthesizesAndChecks node tm t
  -- Let generalization not implemented yet; this test fails
  --, testCase "let generalization: let id a = a; x = id 42; y = id 'hi' in 23" $
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
  , testTerm "let x = _ in _" $ \tms ->
    testCase ("locals ("++tms++")") $ do
      let tm = unsafeParseTerm tms
      [(_,xt)] <- localsAt node [Paths.Body] tm
      [] <- localsAt node [Paths.Binding 0, Paths.Body] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
  , testTerm "let x = _; y = _ in _" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Body]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt), (_,yt)] <- localsAt node [Paths.Body, Paths.Body] tm
      assertEqual "xt unconstrained" unconstrained (T.generalize xt)
      assertEqual "yt unconstrained" unconstrained (T.generalize yt)
  , testTerm "let x = _; y = _ in _" $ \tms ->
    -- testTerm "let x = 42; y = _ in _" $ \tms ->
    -- testTerm "let x = 42; y = 43 in _" $ \tms ->
    -- testTerm "let x = 42; y = 43 in 4224" $ \tms ->
    testCase ("locals ("++tms++")@[Body,Binding 0,Body]") $ do
      let tm = unsafeParseTerm tms
      [(_,xt)] <- localsAt node [Paths.Body, Paths.Binding 0, Paths.Body] tm
      assertEqual "xt unconstrainted" unconstrained (T.generalize xt)
  ]

unconstrained :: TType
unconstrained = unsafeParseType "forall a. a"

main :: IO ()
main = defaultMain tests
-- main = defaultMain singleTest
