{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The wide convention's escape analysis (Unison.Runtime.MCode.Wide) on
-- small terms: which binders it marks and where it places the boxes. The
-- boxes depend on whether the convention is on in the test process
-- (UNISON_JIT or UNISON_OPTIMIZATIONS=wide), so each case says what it
-- expects either way.
module Unison.Test.Runtime.MCode.Wide (test) where

import EasyTest
import Unison.ABT.Normalized (pattern TAbss)
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF
  ( ANormal,
    Branched (..),
    Direction (..),
    Func (..),
    Mem (..),
    pattern TApp,
    pattern TBox,
    pattern TLets,
    pattern TMatch,
    pattern TName,
    pattern TVar,
  )
import Unison.Runtime.MCode.Wide (wideBody)
import Unison.Runtime.TypeTags (CTag (..))
import Unison.Runtime.Wide (wideOn)
import Unison.Symbol (Symbol)
import Unison.Util.EnumContainers (mapFromList)
import Unison.Var qualified as Var

f, g, h, c :: Reference
f = Builtin "f"
g = Builtin "g"
h = Builtin "h"
c = Builtin "c"

x, y, a, b :: Symbol
x = Var.named "x"
y = Var.named "y"
a = Var.named "a"
b = Var.named "b"

type Term = ANormal Reference Symbol

call :: Reference -> [Symbol] -> Term
call r = TApp (FComb r)

con :: Word -> [Symbol] -> Term
con t = TApp (FCon c (CTag (fromIntegral t)))

-- | A let whose value arrives by a yield, as the normalizer writes it.
yields :: Symbol -> Term -> Term -> Term
yields v = TLets (Indirect 1) [v] [BX]

-- | The same let, marked wide.
wide :: Symbol -> Term -> Term -> Term
wide v = TLets (Indirect 1) [v] [WD]

-- | A box, when the convention is on.
boxed :: Symbol -> Term -> Term
boxed v bo
  | wideOn = TLets Direct [] [] (TBox v) bo
  | otherwise = bo

match :: Symbol -> [(Word, [Mem], [Symbol], Term)] -> Term
match v arms = TMatch v (MatchData c (mapFromList [(CTag (fromIntegral t), (ms, TAbss vs bo)) | (t, ms, vs, bo) <- arms]) Nothing)

test :: Test ()
test =
  scope "mcode.wide" . tests $
    [ scope "an escape on one of two arms is boxed in that arm" $
        expectEqual
          (wide x (call f [a]) (match x [(0, [], [], boxed x (call g [x])), (1, [BX], [b], TVar x)]))
          (wideBody (yields x (call f [a]) (match x [(0, [], [], call g [x]), (1, [BX], [b], TVar x)]))),
      scope "a variable escaping twice on a path is boxed once, before the first" $
        expectEqual
          (wide x (call f [a]) (boxed x (TLets Direct [y] [BX] (con 0 [x, x]) (call h [x, y]))))
          (wideBody (yields x (call f [a]) (TLets Direct [y] [BX] (con 0 [x, x]) (call h [x, y])))),
      scope "an alias of a wide variable is wide" $
        expectEqual
          (wide x (call f [a]) (wide y (TVar x) (boxed y (call g [y]))))
          (wideBody (yields x (call f [a]) (yields y (TVar x) (call g [y])))),
      scope "a small constructor matched locally is wide and never boxed" $
        expectEqual
          (TLets Direct [x] [if wideOn then WD else BX] (con 0 [a]) (match x [(0, [BX], [b], TVar b)]))
          (wideBody (TLets Direct [x] [BX] (con 0 [a]) (match x [(0, [BX], [b], TVar b)]))),
      scope "a wide variable returned is not boxed" $
        expectEqual
          (wide x (call f [a]) (TVar x))
          (wideBody (yields x (call f [a]) (TVar x))),
      scope "a box inside a let's binding is the binding's; the body boxes again" $
        expectEqual
          (wide x (call f [a]) (wide y (boxed x (call g [x])) (boxed x (call h [x]))))
          (wideBody (yields x (call f [a]) (yields y (call g [x]) (call h [x])))),
      scope "a let of several variables over a worker call is narrow, boxing inside the binding" $
        expectEqual
          (wide x (call f [a]) (TLets (Indirect 1) [y, b] [BX, BX] (boxed x (TApp (FWorker g 1) [x])) (boxed x (call h [x, y, b]))))
          (wideBody (yields x (call f [a]) (TLets (Indirect 1) [y, b] [BX, BX] (TApp (FWorker g 1) [x]) (call h [x, y, b])))),
      scope "a variable a closure captures escapes" $
        expectEqual
          (wide x (call f [a]) (boxed x (TName y (Left g) [x] (TVar y))))
          (wideBody (yields x (call f [a]) (TName y (Left g) [x] (TVar y)))),
      scope "an arm's field shadowing a wide variable is narrow" $
        expectEqual
          (wide x (call f [a]) (match a [(0, [BX], [x], call g [x])]))
          (wideBody (yields x (call f [a]) (match a [(0, [BX], [x], call g [x])])))
    ]
