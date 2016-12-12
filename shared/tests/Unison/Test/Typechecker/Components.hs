module Unison.Test.Typechecker.Components where

import Test.Tasty
import Test.Tasty.HUnit
import Unison.Parsers (unsafeParseTerm)
import qualified Unison.Codebase as Codebase
import qualified Unison.Note as Note
import qualified Unison.Term as Term
import qualified Unison.Test.Common as Common
import qualified Unison.Typechecker.Components as Components

tests :: TestTree
tests = withResource Common.codebase (\_ -> pure ()) $ \codebase ->
  let
    tests =
      [
      -- simple case, no minimization done
        t "let { id x = x; g = id 42; y = id id g; y }"
          "let { id x = x; g = id 42; y = id id g; y }"
      -- check that we get let generalization
      , t "let rec { id x = x; g = id 42; y = id id g; y }"
          "let { id x = x; g = id 42; y = id id g; y }"
      -- check that we preserve order of components as much as possible
      , t "let rec { id2 x = x; id1 x = x; id3 x = x; id3 }"
          "let { id2 x = x; id1 x = x; id3 x = x; id3 }"
      -- check that we reorder according to dependencies
      , t "let rec { g = id 42; y = id id g; id x = x; y }"
          "let { id x = x; g = id 42; y = id id g; y }"
      -- insane example, checks for: generalization, reordering,
      -- preservation of order when possible
      , t "let rec { g = id 42; y = id id g; ping x = pong x; pong x = id (ping x); id x = x; y }"
          "let { id x = x; g = id 42; y = id id g ; (let rec { ping x = pong x; pong x = id (ping x) ; y })}"
      ]
    t before after = testCase (before ++ " ⟹  " ++ after) $ do
      (codebase, _, _, _) <- codebase
      let term = unsafeParseTerm before
      let after' = Components.minimize' term
      _ <- Note.run $ Codebase.typeAt codebase after' []
      assertEqual "comparing results" (unsafeParseTerm after) after'
  in testGroup "Typechecker.Components" tests

main = defaultMain tests
