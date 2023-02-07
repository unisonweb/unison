```unison
use builtin.io2.Tls newClient send handshake terminate

frank: '{IO, Exception} ()
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
    
      frank : '{IO, Exception} ()

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    frank : '{IO, Exception} ()

.> run frank

  dumpData: bad closure: Foreign (Wrap ##Tls _)
  expected type: #00nv2

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  dumpData: bad closure: Foreign (Wrap ##Tls _)
  expected type: #00nv2

