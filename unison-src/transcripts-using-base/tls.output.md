# Tests for TLS builtins

```unison
-- generated with:
-- openssl req -newkey rsa:2048 -subj '/CN=test.unison.cloud/O=Unison/C=US' -nodes -keyout key.pem -x509 -days 365 -out cert.pem

self_signed_key_pem = "-----BEGIN PRIVATE KEY-----\nMIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQDBrpp+SxjCz/aQ\nQXT2hKXrT3lZ3Ek1VT/kgNre3J/RUyNZjZnZXCMyNjZ4IxYKxcCAIgGtfFpgvkzT\n5NRGZKLlSX4Y8HayV3gdEXO9hq4w/i/s+I0tOAJkVtHolXrrziZ7/4NZwMTbbqhO\n5hHHhbtBIpvNSw8el3AY3APkQ37+wM3fbvyeuG0ekoLqTa371W+/Z7pOi7QXYGEa\n8YHSNkuRKY46C4Y8oeWHOlSbQKu151GLuyQu74bnecGDN4KBdz9nyyKCCTpBsJpU\ni9Ozq3cps5L8dnN1zUNgaTyWp9kO3vbaTU8MY7p/dc7hNJ8pmGtSiiSs1xvni4Xl\nCBXBesxTAgMBAAECggEAAUtfcPSjh7nIFhK562PbkAUJ9JXKT3bwZGCQFek3kDiU\nBecyXgeFnLJMDuV9IjlMHg8cH8Ky/+6FqOzglk/Z3tps41HIGU0IWnlhYqThySYJ\nv/WxS9oR+gWyhXFqTuUj0LRWdmUZa7YDnfNfrwuvwrGuhOK5iSTN9PyTchUZZi50\ntxcNS/C3rk63c7TZLfuwxwGoUCeJvZZ/rmeVchhsuoo3QdSW0Aee7UtFtnvBfLCK\nXKdz+3q49fLZlDyx9/olJh+TY7GuF+G/LSfyQGi85beQhkXUH8/gIQIRI8INIEPB\n0XeTlv7Sgw5upqplJvHCXjAa+jz/Mo87znXBTMoIIQKBgQDorAlZCjzKxGDIaZoD\nDBXYzhSnnIhthThW4edCQ9/ZnJpX4vdTw4FngW504d8SPStMCYeBeMt8iwTczI4W\nHfK+AlVTlPfH/9NnIVADqqr9kobJW6782MYSW2/758d+L5bq8NGATyh8nPll9joN\nYAk7tNO2bGO2bEk2DbZMf3qnOQKBgQDVGdD005kUT3D+DfgeLTGzwk/2wCCPjoJ5\n6fsjsFAeZWU/qioIB3xHt1w8NsT6O+7XOKp/GRbsvsJR9Z/zmA3Ii5yrHYn48UzM\n+UyGLv+2HCpO+8A3szz/aDhKIxNFpXyZzvOXdtqBwTQbICOW2WRWOOgDrS2W1i9l\nD69xRLqj6wKBgBW0xwJ5hAZen7DSuT2DiR46y45/efLNtN3WIV77OgzxIS0FzZEQ\n8ieX2Zgp4kevUTS8xtl7TXCG/6MhqjfB/31edltf0GXmJfC/GNneuCkD3HM4jHCm\nQIRB54aWrvPEuM2ePc08lUha1KGAgRXyWaoqSn4ASqUgIQxb5x/n3KdxAoGAXlD0\nyMc2Q2T9r5PjMwfxrYvc9GsIfkEmwmqqupr4etuJumnH/JHDkcI30nazK8WG6j6s\nR2CFYvby7m92AcxUnWQdTSbfwAycX0QfeGwoxSMmHpsR8hUkdC5ea4Gmr/aUdUbB\nTVJPV4p5U2AgIE3LONYq6iWlvdLCW0pb7hfrO00CgYAb8bXz9BNow4soEkSbQysg\n4sGAr1+iSPY+ErffKRDpcFRnWcQdnTfI4xd8bgnC6OZwVpLbRZaZf3opDJ+axWqa\nEgAeHErTDY4R8aMecvyQj780sQ35kVq4VK0rSQyiKRBcjEust8UEzwYsUog2ysN0\n3zLHVEvFTfwOSctnEQRw1w==\n-----END PRIVATE KEY-----\n"

self_signed_cert_pem = "-----BEGIN CERTIFICATE-----\nMIIDVTCCAj2gAwIBAgIUZI9WPZk1rv2Sx5XSK17BZSV4t7gwDQYJKoZIhvcNAQEL\nBQAwOjEaMBgGA1UEAwwRdGVzdC51bmlzb24uY2xvdWQxDzANBgNVBAoMBlVuaXNv\nbjELMAkGA1UEBhMCVVMwHhcNMjEwMTIyMDkxMTE3WhcNMjIwMTIyMDkxMTE3WjA6\nMRowGAYDVQQDDBF0ZXN0LnVuaXNvbi5jbG91ZDEPMA0GA1UECgwGVW5pc29uMQsw\nCQYDVQQGEwJVUzCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMGumn5L\nGMLP9pBBdPaEpetPeVncSTVVP+SA2t7cn9FTI1mNmdlcIzI2NngjFgrFwIAiAa18\nWmC+TNPk1EZkouVJfhjwdrJXeB0Rc72GrjD+L+z4jS04AmRW0eiVeuvOJnv/g1nA\nxNtuqE7mEceFu0Eim81LDx6XcBjcA+RDfv7Azd9u/J64bR6SgupNrfvVb79nuk6L\ntBdgYRrxgdI2S5EpjjoLhjyh5Yc6VJtAq7XnUYu7JC7vhud5wYM3goF3P2fLIoIJ\nOkGwmlSL07Ordymzkvx2c3XNQ2BpPJan2Q7e9tpNTwxjun91zuE0nymYa1KKJKzX\nG+eLheUIFcF6zFMCAwEAAaNTMFEwHQYDVR0OBBYEFFE3RQYASDWtwSdXL+qtQrjy\nH4SZMB8GA1UdIwQYMBaAFFE3RQYASDWtwSdXL+qtQrjyH4SZMA8GA1UdEwEB/wQF\nMAMBAf8wDQYJKoZIhvcNAQELBQADggEBAH7D8W68cR0QvNNPugCY7lPvA/F94Qam\nwCC2A55edcagfjqFy77xx4Ljrd2TC19yiSzyeeJ+YuohbcipLce90BaaaiYq9aah\n5DICDCUhm1qbhJzqNB2Lqgl4aN+jRMeRVC+rpQBYBNmdPBqdv/k+T2uyghwmLWXe\n/AxCjCLA0zoYzOMytS86veD6CQbF5DpSTZx5dyZTeGhk2izhoM8cgiu+/7YncAbJ\nt7b7UT5Yu3+z1hAdUF5Q21bkEksGBC8UW0G0PMy8XNRMuMsz+2LC39u3u7QyX/+e\nuQGST3aCreV27zd0lrF8LHjwD2XcjVVzHy46VYQvf1r+6gatedDBjqc=\n-----END CERTIFICATE-----\n"

```

# Using an alternative certificate store

First lets make sure we can load our cert and private key

```unison
test> match (decodeCert.impl (toUtf8 self_signed_cert_pem) with
  Left (Failure _ t _) -> [Fail t]
  Right _ -> [Ok "succesfully decoded self_signed_pem"]

```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
    test.cf6t0id1ti (Unison bug, unknown term)
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | test> match (decodeCert.impl (toUtf8 self_signed_cert_pem) with
    
    ✅ Passed succesfully decoded self_signed_pem

```
Test handshaking a client/server a local TCP connection using our
self-signed cert.

We'll create a server and a client, and start threads for each.

The server will report the port it is bound to via a passed MVar which
the client can read.

```unison
serverThread: MVar Nat -> Text -> '{io2.IO}()
serverThread portVar toSend = 'let
  go: '{io2.IO, Exception}()
  go = 'let
       -- load our self signed cert
    cert = decodeCert (toUtf8 self_signed_cert_pem)

       -- assume there is exactly one key decoded from our Bytes
    key = match (decodePrivateKey (toUtf8 self_signed_key_pem)) with k +: _ -> k

       -- create a default configuration using our credentials (certificate chain and key)
    tlsconfig = Tls.ServerConfig.default [cert] key

       -- Open a TCP server port:
    -- we pass the special port "0" to mean "please find us an open port"
    sock = serverSocket (Some "127.0.0.1") "0"

       -- find out what port we got
    port = socketPort sock

       -- report the port back so that the client knows where to connect
    put portVar port

       -- start listening to the socket so that it starts accepting connections
    listen sock

    watch ("server listening on port: " ++ (toText port)) ()

       -- accept a single connection on this socket
    sock' = socketAccept sock

       -- attach TLS to our TCP connection
    tls = newServer tlsconfig sock'

       -- try to handshake the TLS connection with the client
    handshake tls

       -- send our message over our tls channel
    send tls (toUtf8 toSend)
    terminate tls
    closeSocket sock'

  match (toEither go) with
    Left (Failure _ t _) -> watch ("error in server: " ++ t) ()
    _ -> watch "server finished" ()

testClient : Optional SignedCert -> Text -> MVar Nat -> '{io2.IO, Exception} Text
testClient cert hostname portVar _ =
  -- create a client that will expect a cert from the given hostname (CN)
  defaultClient = (Tls.ClientConfig.default hostname Bytes.empty)

  -- if we were passed a certificate to trust, it is the only certificate we trust
  tlsconfig = match cert with
    None        -> defaultClient
    Some (cert) -> defaultClient |> ClientConfig.certificates.set [cert]

  -- wait to find out what port the server started on
  port = take portVar

  -- create a tcp connection with the server
  sock = clientSocket "127.0.0.1" (Nat.toText port)

  -- attach the TLS client to the TCP socket
  tls = newClient tlsconfig sock
  watch ("client connecting to port: " ++ (toText port)) ()

  -- verify that the server presents us with a certificate chain for
  -- test.unison.cloud originating with a certificate we trust, and
  -- that the server can use a compatible TLS version and cipher
  handshake tls

  -- receive a message from the server
  fromUtf8 (receive tls)

testConnectSelfSigned : '{io2.IO}[Result]
testConnectSelfSigned _ =
  test _ =
    -- Server
    portVar = !MVar.newEmpty
    toSend = "12345"
    forkComp (serverThread portVar toSend)

    -- Client
    cert = decodeCert (toUtf8 self_signed_cert_pem)
    received = !(testClient (Some cert) "test.unison.cloud" portVar)

    expectU "should have reaped what we've sown" toSend received

  runTest test

-- this client will trust whatever certs the system trusts
-- for signing certs. This should NOT trust the server
-- serving the self-signed cert, so both the client and
-- the server should have a failure during the handshake
testCAReject : '{io2.IO}[Result]
testCAReject _ =
  checkError : Either Failure a -> Result
  checkError = cases
    Right _ -> Fail "expected a handshake exception"
    Left (Failure _ t _) ->
      if contains "UnknownCa" t && contains "HandshakeFailed" t then Ok "correctly rejected self-signed cert" else
        Fail ("expected UnknownCa, got: " ++ t)

  test _ =
    -- Server
    portVar = !MVar.newEmpty
    toSend = "12345"
    forkComp (serverThread portVar toSend)

    -- Client
    testClient None "test.unison.cloud" portVar |> toEither |> checkError |> emit


  runTest test

-- this client will ask for a different hostname, and
-- therefore should fail during the handshake when the
-- server presents an cert with unexpected hostname
testCNReject : '{io2.IO}[Result]
testCNReject _ =
  checkError : Either Failure a -> Result
  checkError = cases
    Right _ -> Fail "expected a handshake exception"
    Left (Failure _ t _) -> if contains "NameMismatch" t && contains "HandshakeFailed" t then Ok "correctly rejected self-signed cert" else
        Fail ("expected UnknownCa, got: " ++ t)

  test _ =
    -- Server
    portVar = !MVar.newEmpty
    toSend = "12345"
    forkComp (serverThread portVar toSend)

    -- Client
    testClient None "wrong.host.name" portVar |> toEither |> checkError |> emit


  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      serverThread          : MVar Nat -> Text -> '{IO} ()
      testCAReject          : '{IO} [Result]
      testCNReject          : '{IO} [Result]
      testClient            : Optional SignedCert
                              -> Text
                              -> MVar Nat
                              -> '{IO, Exception} Text
      testConnectSelfSigned : '{IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    serverThread          : MVar Nat -> Text -> '{IO} ()
    testCAReject          : '{IO} [Result]
    testCNReject          : '{IO} [Result]
    testClient            : Optional SignedCert
                            -> Text
                            -> MVar Nat
                            -> '{IO, Exception} Text
    testConnectSelfSigned : '{IO} [Result]

.> io.test testConnectSelfSigned

    New test results:
  
  ◉ testConnectSelfSigned   should have reaped what we've sown
  
  ✅ 1 test(s) passing
  
  Tip: Use view testConnectSelfSigned to view the source of a
       test.

.> io.test testCAReject

    New test results:
  
  ◉ testCAReject   correctly rejected self-signed cert
  
  ✅ 1 test(s) passing
  
  Tip: Use view testCAReject to view the source of a test.

.> io.test testCNReject

    New test results:
  
  ◉ testCNReject   correctly rejected self-signed cert
  
  ✅ 1 test(s) passing
  
  Tip: Use view testCNReject to view the source of a test.

```
