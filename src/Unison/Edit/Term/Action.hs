module Unison.Edit.Term.Action where

data Action e
  = Abstract -- Turn target into function parameter
  | Beta -- Beta reduce the target
  | Eta -- Eta reduce the target
  | LetFloat -- Float the target out to a let binding, as far as possible
  | WHNF -- Simplify target to weak head normal form
  | HNF -- Simplify target to head normal form
  | Apply e -- Replace the target, `e`, with `f e`

-- combine fst snd
-- uncurry f = (x,y) -> f x y
-- x y -> x + y
-- uncurry (x y -> x + y)
-- (x,y) -> (x y -> x + y) x y
-- (x,y) -> x + y
-- what about combining function parameters?
--bar = foo (12, "hi")
--foo p = g (fst p) (snd p)
--
--bar = foo 12 "hi"
--foo x y = g x y
--foo x _ = x + 12
-- flip f = y x -> f x y
-- x y -> x + y
-- flip (x y -> x + y) -- Apply flip
-- y x -> (x y -> x + y) x y -- Step
-- y x -> x + y
-- Preapply can be implemented using abstract and trim
-- need ways to combine function arguments, and ways to
-- split function arguments
-- to combine two arguments, we replace the input param
-- of types `x` and `x2` with a single param of type `p`,
-- and supply two functions, a `p -> x` and a `p -> x2`
-- to split an argument, of type `p`, we find all the
-- places where a closed function is applied to `p`, and get
-- a set of functions `p -> x1`, `p -> x2`...
-- each of `x1`, `x2`... become parameters to the function
-- and the functions `p -> x1`... are shifted to the call sites
-- this can be interactive, depending on how we want to do the
-- splitting
-- An Address type, can target multiple expressions

{-
Abstract : turns a subexpression into a function parameter, bound at the nearest lambda. If applied to a free variable, does a lambda-lifting step.

  x -> x + [[ 1 ]]
  ----------------
  x y = x + y

Apply f : replace the target, `e` with `f e`

  foo x * bar [[ e ]]
  -------------------
  foo x * bar (f e)
-}
