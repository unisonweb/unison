```ucm:hide
.> builtins.merge
.> builtins.mergeio
```

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
.> add
.> run frank
```
