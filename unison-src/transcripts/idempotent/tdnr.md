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

  + (added), ~ (modified)

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

  + (added), ~ (modified)

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

  + (added), ~ (modified)

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

  + (added), ~ (modified)

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

  + (added), ~ (modified)

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
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo : Text

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move bad.foo lib.bad.foo

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
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move bad.foo lib.bad.foo

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

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
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move bad.foo lib.bad.foo

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

  + (added), ~ (modified)

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
dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.lib.dep.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move dep.lib.dep.foo lib.dep.lib.dep.foo

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
dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.lib.dep.foo : Nat
  + good.foo        : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move dep.lib.dep.foo lib.dep.lib.dep.foo

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

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
dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.lib.dep.foo : Nat
  + good.foo        : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move dep.lib.dep.foo lib.dep.lib.dep.foo

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

  + (added), ~ (modified)

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
good.foo = 17
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move good.foo lib.good.foo

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
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move good.foo lib.good.foo

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

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
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move good.foo lib.good.foo

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

  + (added), ~ (modified)

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
good.foo = 17
bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + bad.foo  : Text
  + good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move good.foo lib.good.foo

  Done.

scratch/main> move bad.foo lib.bad.foo

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

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
good.foo = 17
dep.lib.dep.foo = 217
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.lib.dep.foo : Nat
  + good.foo        : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move good.foo lib.good.foo

  Done.

scratch/main> move dep.lib.dep.foo lib.dep.lib.dep.foo

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

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
dep.lib.good.foo = 17
dep.lib.bad.foo = "bar"
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + dep.lib.bad.foo  : Text
  + dep.lib.good.foo : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> move dep.lib.good.foo lib.dep.lib.good.foo

  Done.

scratch/main> move dep.lib.bad.foo lib.dep.lib.bad.foo

  Done.
```

``` unison
thing = foo Nat.+ foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + thing : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
scratch/main> delete.project scratch
```
