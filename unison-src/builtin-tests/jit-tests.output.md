
Note: This should be forked off of the codebase created by base.md

.> compile.native.fetch
If you want to define more complex tests somewhere other than `tests.u`, just `load my-tests.u` then `add`,
then reference those tests (which should be of type `'{IO,Exception,Tests} ()`, written using calls
to `Tests.check` and `Tests.checkEqual`).

```ucm
.> load unison-src/builtin-tests/networking-tests.u.> add
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  I couldn't find any definitions matching the name Tls.newClient inside the namespace .
  
    114 |   tls = Tls.newClient tlsconfig sock
  
  Some common causes of this error include:
    * Your current namespace is too deep to contain the
      definition in its subtree
    * The definition is part of a library which hasn't been
      added to this project
  
  To add a library to this project use the command: `fork <.path.to.lib> .lib.<libname>`
  
  Whatever it is, its type should conform to ClientConfig -> Socket -> o.
  
  I found some terms in scope that have matching names and types. Maybe you meant one of these:
  
    - unison.internal.deps.base.io.Tls.newClient : ClientConfig -> Socket ->{IO} Either Failure Tls
    - base.IO.net.Tls.newClient : ClientConfig -> Socket ->{IO, Exception} Tls

