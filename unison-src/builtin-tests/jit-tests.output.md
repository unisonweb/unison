
Note: This should be forked off of the codebase created by base.md

If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

TODO remove md5 alias when base is released
```ucm
.> run.native tests

  💔💥
  
  I've encountered a call to builtin.todo with the following
  value:
  
    "Lit.toScheme termlink"
  
  
  Stack trace:
    todo
    #j0ng3hvp1f
    #gkf1m9rjl7
    internal.compiler.anf.ANormal.toScheme
    internal.compiler.anf.SuperNormal.toScheme
    internal.compiler.anf.SuperGroup.toScheme
    internal.compiler.schemeOutput
    #o7tbrr7g31
    internal.compiler.scheme.generateDefns
    internal.compiler.generateScheme
    internal.compiler.saveScheme
    #gha9jlh8fv

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  💔💥
  
  I've encountered a call to builtin.todo with the following
  value:
  
    "Lit.toScheme termlink"
  
  
  Stack trace:
    todo
    #j0ng3hvp1f
    #gkf1m9rjl7
    internal.compiler.anf.ANormal.toScheme
    internal.compiler.anf.SuperNormal.toScheme
    internal.compiler.anf.SuperGroup.toScheme
    internal.compiler.schemeOutput
    #o7tbrr7g31
    internal.compiler.scheme.generateDefns
    internal.compiler.generateScheme
    internal.compiler.saveScheme
    #gha9jlh8fv

