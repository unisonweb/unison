We had bugs in the calling conventions for both send and terminate which would 
cause pattern matching on the resulting (Right ()) would cause a runtime error.



```unison
use builtin.io2.Tls newClient send handshake terminate

frank: '{IO} ()
frank = do 
  (Right socket) = clientSocket.impl "example.com" "443"
  config = ClientConfig.default "example.com" 0xs
  (Right tls) = newClient.impl config socket
  (Right ()) = handshake.impl tls
  (Right ()) = send.impl tls 0xs
  (Right ()) = terminate.impl tls
  ()
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      frank : '{IO} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    frank : '{IO} ()

.> run frank

  ()

```
