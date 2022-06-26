module Unison.Test.Typechecker.Components where

-- import Control.Monad
import EasyTest

-- import Unison.Syntax.Parsers (unsafeParseTerm)
-- import qualified Unison.Note as Note
-- import qualified Unison.Test.Common as Common
-- import qualified Unison.Typechecker.Components as Components

test :: Test ()
test = scope "Typechecker.Components" $ ok

-- [
-- -- simple case, no minimization done
--   t "{ id x = x; g = id 42; y = id id g; y }"
--     "{ id x = x; g = id 42; y = id id g; y }"
-- -- check that we get let generalization
-- , t "{ id x = x; g = id 42; y = id id g; y }"
--     "{ id x = x; g = id 42; y = id id g; y }"
-- -- check that we preserve order of components as much as possible
-- , t "{ id2 x = x; id1 x = x; id3 x = x; id3 }"
--     "{ id2 x = x; id1 x = x; id3 x = x; id3 }"
-- -- check that we reorder according to dependencies
-- , t "{ g = id 42; y = id id g; id x = x; y }"
--     "{ id x = x; g = id 42; y = id id g; y }"
-- -- insane example, checks for: generalization, reordering,
-- -- preservation of order when possible
-- , t "{ g = id 42; y = id id g; ping x = pong x; pong x = id (ping x); id x = x; y }"
--     "{ id x = x; g = id 42; y = id id g ; ({ ping x = pong x; pong x = id (ping x) ; y })}"
-- ]
-- where
-- t before after = scope (before ++ " ⟹  " ++ after) $ do
--   let term = unsafeParseTerm before
--   let after' = Components.minimize' term
--   guard $ Common.typechecks' after'
--   expect (unsafeParseTerm after ==  after')
