# lib.install.local

``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
myTerm = 1
type MyType = Con
```

``` ucm :hide
scratch/main> update
```

Add some history so we can see if we're squashing as expected.

``` unison :hide
myTerm = 2
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

-- Simplest version should install main branch under the lib name

myproject/main> lib.install.local scratch

  I installed scratch/main into lib.scratch_main

myproject/main> ls lib

  1. scratch_main. (772 terms, 119 types)

-- Can also specify a custom destination location

myproject/main> lib.install.local scratch/main coolerscratch

  I installed scratch/main into lib.coolerscratch

myproject/main> ls lib

  1. coolerscratch. (772 terms, 119 types)
  2. scratch_main.  (772 terms, 119 types)

-- Installed libs should be squashed.

myproject/main> history lib.scratch

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #sg60bvjo91 (start of history)
```
