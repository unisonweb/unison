TDNR selects local term (in file) that typechecks over local term (in file) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
bad.foo = "bar"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat
  + thing    : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (in file) that typechecks over local term (in namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 17
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat
  + thing    : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (in file) that typechecks over local term (shadowing namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 17
bad.foo = "baz"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat
  + thing    : Nat
  ~ bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over local term (in file) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
bad.foo = "bar"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo : Text
  + thing   : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over local term (in namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over local term (shadowing namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
bad.foo = "baz"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat
  ~ bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over local term (in file) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 18
bad.foo = "bar"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo : Text
  + thing   : Nat
  ~ good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over local term (in namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 18
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat
  ~ good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over local term (shadowing namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 18
bad.foo = "baz"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat
  ~ bad.foo  : Text
  ~ good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

\=== start local over direct dep

TDNR selects local term (in file) that typechecks over direct dependency that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 17
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat
  + thing    : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over direct dependency that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
lib.bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo    : Nat
  + lib.bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over direct dependency that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
lib.bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo    : Nat
  + lib.bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 18
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat
  ~ good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR not used to select local term (in file) that typechecks over indirect dependency that also typechecks.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.dep.lib.dep.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 17
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat
  + thing    : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR not used to select local term (in namespace) that typechecks over indirect dependency that also typechecks.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
lib.dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo            : Nat
  + lib.dep.lib.dep.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR not used to select local term (shadowing namespace) that typechecks over indirect dependency that also typechecks.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
good.foo = 17
lib.dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo            : Nat
  + lib.dep.lib.dep.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
good.foo = 18
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat
  ~ good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over local term (in file) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.good.foo = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
bad.foo = "bar"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo : Text
  + thing   : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over local term (in namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo      : Text
  + lib.good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over local term (shadowing namespace) that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo      : Text
  + lib.good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
bad.foo = "baz"
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat
  ~ bad.foo : Text

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over direct dependency that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.good.foo = 17
lib.bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.bad.foo  : Text
  + lib.good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR not used to select direct dependency that typechecks over indirect dependency that also typechecks.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.good.foo = 17
lib.dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.dep.lib.dep.foo : Nat
  + lib.good.foo        : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```

TDNR selects indirect dependency that typechecks over indirect dependency that doesn't.

``` ucm :hide
scratch/main> builtins.merge lib.builtin
```

``` unison
lib.dep.lib.good.foo = 17
lib.dep.lib.bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + lib.dep.lib.bad.foo  : Text
  + lib.dep.lib.good.foo : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  + (added), ~ (modified), - (deleted)

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```
