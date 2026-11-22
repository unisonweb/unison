# `ir` and `ir.optimized`

`ir` shows the code the runtime compiles a term to, before optimization. `ir.optimized` shows it after.

The code is printed in a syntax close to Unison's. Each example explains the new syntax it uses, and [the table at the end](#syntax-reference) lists all of it.

Most examples use definitions in the scratch file, which don't need to be added first. One set is added to the codebase.

``` ucm :hide
scratch/main> builtins.mergeio
```

Unison's intermediate representation is in A-normal form, which means that function calls never take composite expressions as arguments. Each argument is a variable. So `fib (drop n 1)` becomes two steps: first `v4 = (builtin Nat.drop) n v3`, then `fib v4`. Even a constant like `2` gets its own binding, `v1 = 2`, before it's used.

Every intermediate value gets a name this way:

  - Variables you named keep their names, like `n`.
  - Others are numbered `v1`, `v2`, ... within each definition.
  - An `if` becomes a `match` on `true` and `false`. `_` is the default case.

Before optimization, `fib` calls builtin functions, written `(builtin Nat.drop) n v3`. Optimization replaces each call with the instruction itself, written as plain `Nat.drop n v3`, or `v5 Nat.+ v8` for an operator.

``` unison :hide
fib : Nat -> Nat
fib n = if n < 2 then n else fib (drop n 1) + fib (drop n 2)
```

``` ucm
scratch/main> ir fib

  fib n =
    v1 = 2
    v2 = (builtin <) n v1
    match v2 with
      false ->
        v3 = 1
        v4 = (builtin Nat.drop) n v3
        v5 = fib v4
        v6 = 2
        v7 = (builtin Nat.drop) n v6
        v8 = fib v7
        (builtin Nat.+) v5 v8
      _ -> n

scratch/main> ir.optimized fib

  fib n =
    v1 = 2
    v2 = n < v1
    match v2 with
      false ->
        v3 = 1
        v4 = Nat.drop n v3
        v5 = fib v4
        v6 = 2
        v7 = Nat.drop n v6
        v8 = fib v7
        v5 Nat.+ v8
      _ -> n
```

For a function in a cycle, both commands show every function in the cycle.

``` unison :hide
even : Nat -> Boolean
even x = if x == 0 then true else odd (drop x 1)

odd : Nat -> Boolean
odd x = if x == 0 then false else even (drop x 1)
```

``` ucm
scratch/main> ir even

  even x =
    v1 = 0
    v2 = (builtin ==) x v1
    match v2 with
      false ->
        v3 = 1
        v4 = (builtin Nat.drop) x v3
        odd v4
      _ -> true

  odd x =
    v1 = 0
    v2 = (builtin ==) x v1
    match v2 with
      false ->
        v3 = 1
        v4 = (builtin Nat.drop) x v3
        even v4
      _ -> false
```

Lambdas and local functions become top-level functions:

  - They're named after the function they came from, like `addAll$Lambda0` or `sumWith$go`.
  - The variables they use from outside become extra arguments. Here that's `k`.
  - `partialApply f a b` builds a closure: `f` with its first arguments filled in, not called yet.

Some variables get names of their own:

  - `x1` is the argument of a `cases` function.
  - `p1` is a value a pattern matches but doesn't name, like the inside of `Some 0`.

A list match first takes the list apart with `##List.viewl`, then matches `VElem x rest` or `VEmpty`.

``` unison :hide
addAll : Nat -> [Nat] -> [Nat]
addAll k xs = List.map (x -> x + k) xs

sumWith : Nat -> [Nat] -> Nat
sumWith k xs =
  go acc = cases
    [] -> acc
    x +: rest -> go (acc + x * k) rest
  go 0 xs

describe : Optional Nat -> Text
describe = cases
  None -> "none"
  Some 0 -> "zero"
  Some n -> "some " ++ Nat.toText n
```

``` ucm
scratch/main> ir addAll

  addAll k xs =
    v1 = addAll$Lambda0 k
    List.map v1 xs

  addAll$Lambda0 k x = (builtin Nat.+) x k

scratch/main> ir sumWith

  sumWith k xs =
    v1 = 0
    sumWith$go k v1 xs

  sumWith$go k acc x1 =
    v1 = (builtin ##List.viewl) x1
    match v1 with
      VElem x rest ->
        v2 = (builtin Nat.*) x k
        v3 = (builtin Nat.+) acc v2
        sumWith$go k v3 rest
      VEmpty -> acc

scratch/main> ir describe

  describe x1 =
    match x1 with
      Some p1 ->
        match p1 with
          0 -> "zero"
          _ ->
            v1 = "some "
            v2 = (builtin Nat.toText) p1
            (builtin Text.++) v1 v2
      None -> "none"
```

Handlers:

  - `handle {Counter} e with h` runs `e` with `h` handling `Counter`.
  - In a handler, `{bump}` matches the request `bump`, and `{a}` matches the value returned without a request.
  - A helper function shows up inside the function that uses it, like `f1` here.

Optimization notices that this handler resumes each request at most once, as its last step. That makes it *affine*, and the optimizer adds a faster second version of the handler, `v2`. Which version runs is decided when `handle` runs: `with if noNonAffineHandlers then v2 else v1`.

  - The general version, `f1`, saves the rest of the computation as `cont1 = shift {Counter}`, then resumes it through `k`.
  - The affine version, `f2`, saves nothing. It handles the request in place (`inHandlerEnv aff`), stores the handler for the next request (`setHandler aff`), and returns `n` to the code that asked.

``` unison :hide
ability Counter where
  bump : Nat

runCounter : '{Counter} a -> a
runCounter p =
  h n = cases
    { a } -> a
    { bump -> k } -> handle k n with h (n + 1)
  handle p() with h 0
```

``` ucm
scratch/main> ir runCounter

  runCounter p =
    v1 = 0
    v2 = runCounter$h v1
    v3 = partialApply runCounter$Lambda0 p
    v2 v3

  runCounter$h n x1 =
    f1 n v2 =
      match v2 with
        {bump} ->
          cont1 = shift {Counter}
          k = partialApply (builtin ##jumpCont) cont1
          v3 = 1
          v4 = (builtin Nat.+) n v3
          v5 = runCounter$h v4
          v6 = partialApply runCounter$h$Lambda0 k n
          v5 v6
        {a} -> a
    v1 = partialApply f1 n
    handle {Counter}
      x1()
    with v1

  runCounter$h$Lambda0 k n = k n

  runCounter$Lambda0 p =
    v1 = ()
    p v1

scratch/main> ir.optimized runCounter

  runCounter p =
    v1 = 0
    v2 = partialApply runCounter$Lambda0 p
    runCounter$h v1 v2

  runCounter$h n x1 =
    f1 n v3 =
      match v3 with
        {bump} ->
          cont1 = shift {Counter}
          k = partialApply (builtin ##jumpCont) cont1
          v4 = 1
          v5 = n Nat.+ v4
          v1 = partialApply f1 v5
          handle {Counter}
            k n
          with v1
        {a} -> a
    f2 n aff v3 =
      match v3 with
        {bump} ->
          inHandlerEnv aff ->
            v4 = 1
            v5 = n Nat.+ v4
            v1 = partialApply f2 v5 aff
            setHandler aff v1
            n
        {a} -> a
    v1 = partialApply f1 n
    v2 = partialApply f2 n
    handle {Counter}
      x1()
    with if noNonAffineHandlers then v2 else v1

  runCounter$h$Lambda0 k n = k n

  runCounter$Lambda0 p =
    v1 = ()
    p v1
```

`ir.optimized` also shows the extra functions optimization creates. Each one is named after its original function, plus a label: `#spec1`, `#return` or `#join1`.

Here, the loop's state is a tuple, so optimization makes `sumCount$go#spec1`, which takes the two numbers as separate arguments instead.

  - Arguments added by optimization are named `eta1`, `eta2`, ....
  - A tuple is a chain of pairs, so taking one apart matches `Cons acc p1`, then `p1`.
  - A name used twice gets numbered, like `acc1`.

``` unison :hide
sumCount : [Nat] -> Nat
sumCount xs =
  go : (Nat, Nat) -> [Nat] -> Nat
  go st = cases
    [] -> match st with (acc, n) -> acc + n
    x +: rest -> match st with (acc, n) -> go (acc + x, n + 1) rest
  go (0, 0) xs
```

``` ucm
scratch/main> ir.optimized sumCount

  sumCount xs =
    v1 = 0
    v2 = 0
    sumCount$go#spec1 v1 v2 xs

  sumCount$go st x1 =
    v1 = ##List.viewl x1
    match v1 with
      VElem x rest ->
        match st with
          Cons acc p1 ->
            match p1 with
              Cons n p2 ->
                match p2 with
                  () ->
                    v2 = acc Nat.+ x
                    v3 = 1
                    v4 = n Nat.+ v3
                    sumCount$go#spec1 v2 v4 rest
      VEmpty ->
        match st with
          Cons acc1 p3 ->
            match p3 with
              Cons n1 p4 ->
                match p4 with
                  () -> acc1 Nat.+ n1

  sumCount$go#spec1 eta1 eta2 x1 =
    v1 = ##List.viewl x1
    match v1 with
      VElem x rest ->
        v2 = eta1 Nat.+ x
        v3 = 1
        v4 = eta2 Nat.+ v3
        sumCount$go#spec1 v2 v4 rest
      VEmpty -> eta1 Nat.+ eta2
```

A function that returns a pair gets a version, `#return`, that returns the two values without building a pair:

  - It returns them with `yield (# q, r #)`. `(# ... #)` means several values, not a tuple.
  - A caller binds them with `(# q, r #) = ...`.
  - The original `divMod` still builds the pair, `(v4, eta2)`, for callers that need it.

``` unison :hide
divMod : Nat -> Nat -> (Nat, Nat)
divMod a b = if a < b then (0, a) else match divMod (drop a b) b with (q, r) -> (q + 1, r)

useDivMod : Nat -> Nat
useDivMod n = match divMod n 7 with (q, r) -> q + r
```

``` ucm
scratch/main> ir.optimized divMod

  divMod a b =
    v1 = a < b
    match v1 with
      false ->
        v2 = Nat.drop a b
        (# eta1, eta2 #) = divMod#return v2 b
        v3 = 1
        v4 = eta1 Nat.+ v3
        (v4, eta2)
      _ ->
        v7 = 0
        (v7, a)

  divMod#return a b =
    v1 = a < b
    match v1 with
      false ->
        v2 = Nat.drop a b
        (# eta1, eta2 #) = divMod#return v2 b
        v3 = 1
        v4 = eta1 Nat.+ v3
        yield (# v4, eta2 #)
      _ ->
        v7 = 0
        yield (# v7, a #)

scratch/main> ir.optimized useDivMod

  useDivMod n =
    v1 = 7
    (# eta1, eta2 #) = divMod#return n v1
    eta1 Nat.+ eta2
```

When a `match` looks at a value whose constructor is already known, optimization picks the matching case ahead of time. This set of definitions is added to the codebase:

``` unison :hide
swapSum : Nat -> Nat -> Nat
swapSum a b = match (b, a) with (x, y) -> x + y

classify : Nat -> Nat
classify n =
  t = if n < 10 then Some n else None
  match t with
    Some m -> m + 1
    None -> 0
```

``` ucm :hide
scratch/main> add
```

``` ucm
scratch/main> ir.optimized swapSum

  swapSum a b = b Nat.+ a

scratch/main> ir.optimized classify

  classify n =
    v1 = 10
    v2 = n < v1
    match v2 with
      false -> 0
      _ ->
        v3 = 1
        n Nat.+ v3
```

Builtins have intermediate code too.

``` ucm
scratch/main> ir Nat.+

  (Nat.+) v1 v2 = v1 Nat.+ v2
```

Constructors don't.

``` unison :hide
type Pair = Pair Nat Nat
```

``` ucm
scratch/main> ir Pair.Pair

  Pair.Pair is a constructor, which has no intermediate code of
  its own.
```

``` ucm :error
scratch/main> ir nope

  ⚠️

  The following names were not found in the codebase. Check your spelling.
    nope
```

Scratch file definitions take priority over the codebase's, as with `run`. Here, `swapSum` is the scratch file's version, not the one added above.

``` unison :hide
swapSum : Nat -> Nat -> Nat
swapSum a b = a * b

quadruple : Nat -> Nat
quadruple n = swapSum (swapSum n 2) 2
```

``` ucm
scratch/main> ir swapSum

  swapSum a b = (builtin Nat.*) a b

scratch/main> ir.optimized quadruple

  quadruple n =
    v1 = 2
    v2 = n Nat.* v1
    v3 = 2
    v2 Nat.* v3
```

## Syntax reference

| Syntax | Meaning |
| --- | --- |
| `f x y = e` | A top-level function. |
| `f$Lambda0`, `f$go` | A lambda, or the local function `go`, moved out of `f`. |
| `f#spec1` | A version of `f` that takes a tuple's or record's fields as separate arguments. |
| `f#return` | A version of `f` that returns its result's fields without building it. |
| `f#join1` | Code that several branches of `f` continue with. |
| `f1 x = e` inside a definition | A helper function of the definition. |
| `v1`, `v2`, ... | A value the program didn't name. Names you gave are kept, numbered if used twice (`acc1`). |
| `x1` | The argument of a `cases` function. |
| `p1` | A value a pattern matches but doesn't name. |
| `eta1` | A variable added by optimization. |
| `x = e` | Binds `x`. |
| `e` on its own line | Runs `e` and ignores its result. |
| `(# x, y #) = e` | Binds the several values `e` returns. |
| `yield (# x, y #)` | Returns several values. No tuple is built. |
| `(x, y)`, `()` | A tuple, and unit. |
| `{ a: x, b: y }` | A record. As a pattern, it takes the fields apart. |
| `r@a.set x` | `r` with field `a` changed to `x`. |
| `x Nat.+ y`, `Nat.drop n k` | A built-in instruction, named after its builtin. Operators are infix. |
| `prim.OP`, `foreign.F` | A built-in instruction with no builtin of its own. |
| `(builtin Nat.+) x y` | A call of a builtin function that wasn't replaced by its instruction. |
| `partialApply f a b` | `f` with its first arguments filled in, not called yet. |
| `x()` | Calls `x` with no arguments. |
| `match x with` | A match. `_` is the default case. `tag n` matches a case by number. |
| `{op x y} ->`, `{a} ->` | Handler cases: the request `op`, and the value returned without a request. |
| `handle {A} e with h` | Runs `e` with `h` handling ability `A`. |
| `cont1 = shift {A}` | Saves the rest of the computation, up to the handler of `A`, as `cont1`. |
| `with if noNonAffineHandlers then h' else h` | Uses the faster affine handler `h'` when every handler in effect is affine. |
| `inHandlerEnv aff -> e` | Runs `e` in place, as the affine handler. |
| `setHandler aff h` | Sets the affine handler for the next request. |
| `discard aff` | Drops an affine handler that is no longer needed. |
| `#v = e` | Binds `#v` unboxed, spread over several slots. Shown only when the JIT is on (`UNISON_JIT=on`). |
| `v = box #v` | Boxes `#v` into the ordinary value `v`. |
