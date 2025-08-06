``` ucm :hide
> builtins.merge lib.builtin
```

``` unison
unique type Foo
  = Bar Nat
  | Baz Nat Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type Foo

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` unison
unique type Foo
  = Bar Nat
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Foo

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

> view Foo

  type Foo = Bar Nat

> find.verbose

  1. -- #h88o5sirfn0a8f4o81sb012p2rha5h8r73n8bloc8qq94kqmltjq94iiep2e6dj7ppuulc8jce2f0vmddqp76nm0hqs9jh53s502v4g
     type Foo
     
  2. -- #h88o5sirfn0a8f4o81sb012p2rha5h8r73n8bloc8qq94kqmltjq94iiep2e6dj7ppuulc8jce2f0vmddqp76nm0hqs9jh53s502v4g#0
     Foo.Bar : Nat -> Foo
     
```
