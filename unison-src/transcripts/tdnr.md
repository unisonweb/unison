TDNR selects local term (in file) that typechecks over local term (in file) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
bad.foo = "bar"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (in file) that typechecks over local term (in namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
good.foo = 17
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (in file) that typechecks over local term (shadowing namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
good.foo = 17
bad.foo = "baz"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over local term (in file) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
```

```ucm
scratch/main> add
```

```unison
bad.foo = "bar"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over local term (in namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over local term (shadowing namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
bad.foo = "baz"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over local term (in file) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
```

```ucm
scratch/main> add
```

```unison
good.foo = 18
bad.foo = "bar"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over local term (in namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
good.foo = 18
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over local term (shadowing namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
good.foo = 18
bad.foo = "baz"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

=== start local over direct dep

TDNR selects local term (in file) that typechecks over direct dependency that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
good.foo = 17
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (in namespace) that typechecks over direct dependency that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
lib.bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects local term (shadowing namespace) that typechecks over direct dependency that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
lib.bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
good.foo = 18
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR not used to select local term (in file) that typechecks over indirect dependency that also typechecks.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.dep.lib.dep.foo = 217
```

```ucm
scratch/main> add
```

```unison
good.foo = 17
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR not used to select local term (in namespace) that typechecks over indirect dependency that also typechecks.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
lib.dep.lib.dep.foo = 217
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR not used to select local term (shadowing namespace) that typechecks over indirect dependency that also typechecks.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
good.foo = 17
lib.dep.lib.dep.foo = 217
```

```ucm
scratch/main> add
```

```unison
good.foo = 18
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over local term (in file) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.good.foo = 17
```

```ucm
scratch/main> add
```

```unison
bad.foo = "bar"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over local term (in namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.good.foo = 17
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over local term (shadowing namespace) that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.good.foo = 17
bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
bad.foo = "baz"
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects direct dependency that typechecks over direct dependency that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.good.foo = 17
lib.bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR not used to select direct dependency that typechecks over indirect dependency that also typechecks.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.good.foo = 17
lib.dep.lib.dep.foo = 217
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```

TDNR selects indirect dependency that typechecks over indirect dependency that doesn't.

```ucm:hide
scratch/main> builtins.merge lib.builtin
```

```unison
lib.dep.lib.good.foo = 17
lib.dep.lib.bad.foo = "bar"
```

```ucm
scratch/main> add
```

```unison
thing = foo Nat.+ foo
```

```ucm:hide
scratch/main> delete.project scratch
```
