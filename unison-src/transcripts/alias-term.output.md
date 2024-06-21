`alias.term` makes a new name for a term.

```ucm
project/main> alias.term lib.builtins.bug foo

  Done.

project/main> ls

  1. foo  (a -> b)
  2. lib/ (643 terms, 92 types)

project/main> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #akvmucsmam .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #akvmucsmam`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When   Root Hash     Action
  1.   now    #94cs49dp5a   alias.term .__projects._f3c06c2f_7513_4da4_87a2_5b7860d8895f...
  2.   now    #akvmucsmam   builtins.mergeio .__projects._f3c06c2f_7513_4da4_87a2_5b7860...
  3.          #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

```
It won't create a conflicted name, though.

```ucm
project/main> alias.term lib.builtins.todo foo

  ⚠️
  
  A term by that name already exists.

```
```ucm
project/main> ls

  1. foo  (a -> b)
  2. lib/ (643 terms, 92 types)

project/main> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #akvmucsmam .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #akvmucsmam`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When   Root Hash     Action
  1.   now    #94cs49dp5a   alias.term .__projects._f3c06c2f_7513_4da4_87a2_5b7860d8895f...
  2.   now    #akvmucsmam   builtins.mergeio .__projects._f3c06c2f_7513_4da4_87a2_5b7860...
  3.          #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

```
You can use `alias.term.force` for that.

```ucm
project/main> alias.term.force lib.builtins.todo foo

  Done.

project/main> ls

  1. foo  (a -> b)
  2. foo  (a -> b)
  3. lib/ (643 terms, 92 types)

project/main> reflog

  Here is a log of the root namespace hashes, starting with the
  most recent, along with the command that got us there. Try:
  
    `fork 2 .old`             
    `fork #94cs49dp5a .old`   to make an old namespace
                              accessible again,
                              
    `reset-root #94cs49dp5a`  to reset the root namespace and
                              its history to that of the
                              specified namespace.
  
       When   Root Hash     Action
  1.   now    #agpq4mvdbu   alias.term.force .__projects._f3c06c2f_7513_4da4_87a2_5b7860...
  2.   now    #94cs49dp5a   alias.term .__projects._f3c06c2f_7513_4da4_87a2_5b7860d8895f...
  3.   now    #akvmucsmam   builtins.mergeio .__projects._f3c06c2f_7513_4da4_87a2_5b7860...
  4.          #sg60bvjo91   history starts here
  
  Tip: Use `diff.namespace 1 7` to compare namespaces between
       two points in history.

```
