# lib.install.local

``` ucm
scratch/main> builtins.merge

  Done.
```

``` unison
myTerm = 1
type MyType = Con
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type MyType

  + myTerm : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Add some history so we can see if we're squashing as expected.

``` unison
myTerm = 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ myTerm : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update
-- Simplest version should install main branch under the lib name
myproject/main> lib.install.local scratch
myproject/main> ls lib
-- Can also specify a custom destination location
myproject/main> lib.install.local scratch/main coolerscratch
myproject/main> ls lib
-- Installed libs should be squashed.
myproject/main> history lib.scratch
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
⚠️

Sorry, I wasn’t sure how to process your request:

  I expected a project or branch, but couldn’t recognize “scratch” as one.

You can run `help lib.install.local` for more information on
using `lib.install.local`.
```
