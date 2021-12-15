{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# Language OverloadedStrings #-}

module Unison.Test.Term where

import EasyTest
import qualified Data.Map         as Map
import           Data.Map         ( (!) )
import Data.Text.Encoding (encodeUtf8)
import qualified U.Util.Hash      as Hash
import qualified Unison.Reference as R
import           Unison.Symbol    ( Symbol )
import qualified Unison.Term      as Term
import qualified Unison.Type      as Type
import qualified Unison.Var       as Var
import qualified Unison.Var.RefNamed as Var

test :: Test ()
test = scope "term" $ tests
  [ scope "Term.substTypeVar" $ do
      -- check that capture avoidance works in substTypeVar
      let v s = Var.nameds s :: Symbol
          tv s = Type.var() (v s)
          v1 s = Var.freshenId 1 (v s)
          tm :: Term.Term Symbol ()
          tm = Term.ann() (Term.ann()
                             (Term.nat() 42)
                             (Type.introOuter() (v "a") $
                               Type.arrow() (tv "a") (tv "x")))
                          (Type.forall() (v "a") (tv "a"))
          tm' = Term.substTypeVar (v "x") (tv "a") tm
          expected =
            Term.ann() (Term.ann()
                          (Term.nat() 42)
                          (Type.introOuter() (v1 "a") $
                            Type.arrow() (Type.var() $ v1 "a") (tv "a")))
                       (Type.forall() (v1 "a") (Type.var() $ v1 "a"))
      note $ show tm'
      note $ show expected
      expect $ tm == tm
      expect $ tm' == tm'
      expect $ tm' == expected
      ok
  , scope "Term.unhashComponent" $
      let h = Hash.fromByteString (encodeUtf8 "abcd")
          ref = R.Derived h 0 1
          v1 = Var.refNamed @Symbol ref
          -- input component: `ref = \v1 -> ref`
          component = Map.singleton ref (Term.lam () v1 (Term.ref () ref))
          component' = Term.unhashComponent component
          -- expected unhashed component: `v2 = \v1 -> v2`, where `v2 /= v1`,
          -- i.e. `v2` cannot be just `ref` converted to a ref-named variable,
          -- since that would collide with `v1`
          (v2, _) = component' ! ref
      in expect $ v2 /= v1
  ]
