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
  socket = assertRight (clientSocket.impl "example.com" "443")
  config = ClientConfig.default "example.com" 0xs
  tls = assertRight (newClient.impl config socket)
  () = assertRight (handshake.impl tls)
  () = assertRight (send.impl tls 0xs)
  () = assertRight (terminate.impl tls)
  ()

assertRight : Either a b -> b
assertRight = cases
  Right x -> x
  Left _ -> bug "expected a right but got a left"
```



```ucm
.> add
.> run frank
```
