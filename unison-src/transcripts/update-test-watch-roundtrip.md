
```ucm:hide
.> builtins.mergeio
```

Given a test that depends on another definition,

```unison:hide
foo n = n + 1

test> mynamespace.foo.test =
  n = 2
  if (foo n) == 2 then [ Ok "passed" ] else [ Fail "wat" ]
```

```ucm
.> add
```

if we change the type of the dependency, the test should show in the scratch file as a test watch.

```unison
foo n = "hello, world!"
```

```ucm:error
.> update
```
