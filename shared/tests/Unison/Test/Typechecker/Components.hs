module Unison.Test.Typechecker.Components where

import EasyTest
import Unison.Parsers (unsafeParseTerm)
import qualified Unison.Codebase as Codebase
import qualified Unison.Note as Note
import qualified Unison.Test.Common as Common
import qualified Unison.Typechecker.Components as Components

test :: Common.CodebaseIOV -> Test ()
test codebase = scope "Typechecker.Components" $ tests
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
  where
  t before after = scope (before ++ " ⟹  " ++ after) $ do
    let term = unsafeParseTerm before
    let after' = Components.minimize' term
    _ <- io . Note.run $ Codebase.typeAt codebase after' []
    expect (unsafeParseTerm after ==  after')
