
Given a test that depends on another definition,

```unison
foo n = n + 1

test> mynamespace.foo.test =
  n = 2
  if (foo n) == 2 then [ Ok "passed" ] else [ Fail "wat" ]
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    foo                  : Nat -> Nat
    mynamespace.foo.test : [Result]

```
if we change the type of the dependency, the test should show in the scratch file as a test watch.

```unison
foo n = "hello, world!"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      foo : n -> Text

```
```ucm
.> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  test> mynamespace.foo.test : [Result]
  mynamespace.foo.test =
    n = 2
    if foo n == 2 then [Ok "passed"] else [Fail "wat"]
  
  foo n = "hello, world!"

  Typechecking failed. I've updated your scratch file with the
  definitions that need fixing. Once the file is compiling, try
  `update` again.

```
