
# Documentation of the Unison runtime

This directory has the code for the Unison runtime.

The Unison runtime is responsible for evaluating Unison code which has been parsed and typechecked. Evaluation converts _computations_, which contain reducible expressions (an expression like `1 + 1` or `case (a,b) of ..`) to _values_ (like `42`), which don't have redexes and which are said to be in _normal form_. The runtime has some design constraints:

##### It should be possible at runtime to hash, serialize, deserialize, and compute the dependencies of any value in the language, including functions.

These capabilities are needed for the implementation of Unison's distributed programming API which ships arbitrary values over the network (and these functions are also just super convenient for other reasons too). That is, it needs to be possible to have functions like:

  * `encode : forall a . a -> Bytes`
  * `decode : forall a . Bytes -> Either Err a`
  * `dependencies : forall a . a -> Set Reference`
  * `hash : forall a . a -> Hash`
  * Note: the types of these might be more constrained than this so you can't break parametricity and use them in parametric code, but the idea is that they could in principle have these types and they really do work _for all_ `a`.

Importantly, values may contain cycles (a recursive function, for instance), and these to be serializable and hashable as well, so there needs to be a way of detecting and encoding these cycles reliably in all cases (having the serialization code blow up when it hits a cycle is not allowed).

##### The runtime should make it possible to decompile any value back to a Unison term.

When you evaluate a watch expression, you see a normalized term. This is nicer than the usual approach of having runtime values be their own universe and requiring the programmer to write a bunch of boilerplate to extract useful information from these runtime values.

##### The runtime should support algebraic effects, which requires being able to manipulate continuations of a running program.

##### This first version of the Haskell runtime isn't aiming for extreme speed. It should be correct, simple, and easy for us to understand and maintain.

Within these parameters, if there's easy speed improvements to be had, great. And perhaps later, we can have a more complicated but insanely fast runtime which is also correct because of intense engineering effort. But now is not the time for that.

##### The runtime should be modular, so that pieces of it can be reused even if we move from, say, directly interpreting some instruction set to JIT-ing via LLVM.

The old Scala runtime was monolithic, going directly from a term to a compiled form.

## Overview

To evaluate a Unison term, `p : AnnotatedTerm v a` which has been successfully typechecked, the runtime goes through several phases:

    p0 : AnnotatedTerm v a
             ||
         let rec minimization
             ||
    p1 : AnnotatedTerm v a
             ||
         lambda lifting
             ||
    p2 : AnnotatedTerm v a
             ||
         A-normal form (ANF) conversion
             ||
    p3 : AnnotatedTerm v a
             ||
         compilation
             ||
    p4 : IR (see `IR.hs`)
             ||
         evaluation
             ||
    p5 : IR.V (see `IR.hs`)
             ||
         decompilation
             ||
    p6 : AnnotatedTerm v ()

Here's a brief overview of these phases:

* let rec minimization eliminates needless cycles and reduces cycle sizes to the minimum, to prepare for just having `let` be the only place in the runtime that must deal with ability requests.
* lambda lifting eliminates lambdas with free variables by adding extra parameters for each free variable, simplifying the later compilation phase.
* ANF moves any function calls or ability requests to the body of a `let` or `let rec`, which is the last thing needed to ensure that `let` is the only place we need to deal with ability requests.
* Compilation converts the ANF code to an intermediate representation, which can be interpreted directly to produce a value, `V`.
* After evaluation, the `V` can be decompiled back to a term, which can be displayed to the user in the codebase editor tool.

#### Phase 1: let rec minimization

_let rec minimization_ breaks up recursive blocks into a mix of `let` and minimally-sized `let rec` blocks. The implementation is in `Unison.Typechecker.Components.minimize`.

__Why do we do this?__

* We decided for sanity and simplicity that the bindings of a cycle (like `ping` and `pong`) can't use any abilities, since it's unclear what order things happen in (if `ping` uses abilities and has a forward reference to `pong`, and `pong` has a reference to `ping` and uses abilities, which effect should happen first??). To clarify, mutually recursive functions in a let rec may use abilities in their body, since those abilities aren't required until the function is called.
* But when the source of a program reveals a clear dependency order to the bindings, we want to be able to use abilities.
* This transformation is also useful in conjunction with ANF conversion - it means that interpretation of `let` is the _one place in the runtime_ where we need to expect an ability request. It makes it very easy to construct the continuations which are passed to the ability handlers.

_Note:_ The typechecker also does this same let rec minimization transform before typechecking, and when typechecking any `let rec` blocks that remain, it sets the ambient ability to be empty. (TODO: check that typechecker does in fact do this, and fix if not)

#### Phase 2: lambda lifting

This transform is currently in the `ANF.hs` file, see the `lambdaLift` function there. This transform eliminates free variables from any lambdas in the term, by turning them into ordinary function parameters. This leaves only closed lambdas, which are easy to compile.

A lambda with free variables is really a program that will generate a function at runtime when values for those free variables are known. Turning these free variables into function parameters just means less cases to deal with later on during compilation.

#### Phase 3: ANF conversion

See [https://en.wikipedia.org/wiki/A-normal_form]. __Why do we do this?__ It's much simpler to compile and optimize, and importantly, it leaves us with __just one place__, in `let`, where the continuations of ability requests are processed by the runtime.

Example:

```
handle (state 0) in
  x = State.get + 3
  y = x + 1
  State.set 42
  99
```

This isn't in ANF, and if we tried to compile this directly, our code for doing function application (the `State.get + 3`) would need to be prepared to deal with ability requests and would need to be able to construct the appropriate continuation:

```
r -> let
  x = r + 3
  y = x + 1
  State.set 42
  99
```

In contrast, if the code is in ANF, then function application doesn't need to deal with ability requests, as functions are always being applied to values:

```
handle (state 0) in
  r = State.get
  x = r + 3
  y = x + 1
  State.set 42
  99
```

#### Phase 4: compilation to IR

The `IR` type, defined in `IR.hs`, denotes a `[Value] -> Result Value`, where:

```
type Result v = Request Value IR | MatchFail | Done v
```

`Value` (called `V` in `IR.hs`) has no redexes and is in normal form.

`IR` is represented as an ANF tree, where variable references are represented by their [De Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index): the nearest / innermost bound variable has an index of 0, the next nearest has an index of 1, etc. Some more interesting examples:

* In `x y -> x + y`, in the body of the lambda, `x` has an index of 1 and `y` has an index of 0.
* In `let x = 1; y = x + 1`, in the body of `y`, `x` has an index of 0.
* In `let rec ping x = pong x; pong x = ping x`, in the body of `ping`, `x` has an index of `0`, `pong` has an index of `1`, and `ping` has an index of `2`.
* In `case x of (y, (z, q)) -> y + z`, in the `y + z` expression, `y` has an index of `2` and `z` has an index of `1` and `q` has an index of 0.

Put another way, variable bindings are pushed onto a stack in "source order", and their position on the stack at each usage site is their De Bruijn index.

In order to convert from the ANF form of the term, which has named variable references, and the De Bruijn indexed `IR` type, we need to convert from named references to De Bruijn indices. The main way we do that is via the function `ABT.annotateBound'`, which is defined for any abstract binding tree:

```haskell
annotateBound'
  :: (Ord v, Functor f, Foldable f)
  => Term f v a0
  -> Term f v [v]
```

This annotates an ABT with the list of bound variables, in order, such that the de bruijn index of variable, `v`, can be computed via [`elemIndex`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:elemIndex) of `v` in this list. Easy peasy.

Once we have that, the conversion to `IR`, defined in `IR.compile0`, is straightforward. A couple wrinkles:

* When a lambda is partially applied, for instance `(x y -> foo x + y) 42`, we recompile the lambda, subsituting the arguments already applied, in this case substituting `x` with `42`, leaving the lambda `(y -> foo 42 + y)`. To support this, the `compile0` function actually takes this environment of already evaluated values, as a `[(SymbolC, Maybe V)]`. This is added onto the end of the list of bound variables.
* Variables which are lambda-lifted out of a function are compiled as "lazy stack lookups" (the `LazySlot` constructor), which doesn't look inside references on the stack. Why is this done?

Well, consider

```Haskell
let rec loop = n -> loop (n + 1)

```

After lambda lifting, this looks like:

```Haskell
let rec loop = (self n -> self (n + 1)) loop
                                        ^^^^
```

But this isn't quite right - the `loop` which is passed to itself needs to be passed lazily, otherwise this would be an infinite loop. Really, we want something more like:

```Haskell
let rec loop = (self n -> !self (n + 1)) 'loop
                                          ^^^^
```

But we don't literally need to use thunks like this, we can just avoid forcing the reference which appears at that stack slot.

The `SymbolC` variable type used by the `compile0` function just tracks which variables need to get this treatment.

#### Phase 5: evaluation

TODO
