``` ucm
> builtins.mergeio

  Done.
```

``` unison
test> pass = [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + pass : [Result]

  Run `update` to apply these changes to your codebase.

    1 | test> pass = [Ok "Passed"]
    
    ✅ Passed Passed
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
test> pass = [Ok "Passed"]
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | test> pass = [Ok "Passed"]
    
    ✅ Passed Passed (cached)
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> test

  Cached test results (`help testcache` to learn more)

    1. pass   ◉ Passed

  ✅ 1 test(s) passing

  Tip: Use view 1 to view the source of a test.
```

``` unison
> ImmutableArray.fromList [?a, ?b, ?c]
> ImmutableByteArray.fromBytes 0xs123456
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > ImmutableArray.fromList [?a, ?b, ?c]
          ⧩
          ImmutableArray.fromList [?a, ?b, ?c]

    2 | > ImmutableByteArray.fromBytes 0xs123456
          ⧩
          fromBytes 0xs123456
```
