``` ucm :hide
> builtins.merge
```

This tests a variable related bug in the ANF compiler.

The nested let would get flattened out, resulting in:

``` 
bar = result
```

which would be handled by renaming. However, the *context* portion of
the rest of the code was not being renamed correctly, so `bar` would
remain in the definition of `baz`.

``` unison
foo _ =
  id x = x
  void x = ()
  bar = let
    void (Debug.watch "hello" "hello")
    result = 5
    void (Debug.watch "goodbye" "goodbye")
    result
  baz = id bar
  baz

> !foo
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + foo : ∀ _. _ -> Nat

  Run `update` to apply these changes to your codebase.

    12 | > !foo
           ⧩
           5
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```
