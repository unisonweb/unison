# Meta.typecheck resolves codebase deps

`Meta.typecheck` must be able to typecheck a meta-decoded term whose body
contains references to user-defined functions, not just builtins. The
typechecker reaches into the running codebase to resolve those references'
types.

``` ucm :hide
scratch/main> builtins.mergeio
```

Define a helper and a function that references it.

``` unison
helper : Nat -> Nat
helper n = n + 1

usesHelper : Nat -> Nat
usesHelper n = helper n + helper n
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + helper     : Nat -> Nat
  + usesHelper : Nat -> Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Decompile `usesHelper` to a meta term, then typecheck it. The decompiled
body contains a `Ref` to `helper`, so this only succeeds if the typechecker
resolves `helper`'s type from the codebase.

``` unison
checkUsesHelper : '{IO} Either Text Nat
checkUsesHelper _ =
  metaTerm = Meta.decompile usesHelper
  match Meta.typecheck metaTerm with
    Left e -> Left e
    Right _ -> Right 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + checkUsesHelper : '{IO} Either Text Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> run checkUsesHelper

  Right 1
```
