# Tests for network related builtins

## Setup

You can skip the section which is just needed to make the transcript self-contained.

```unison
use .builtin.io2 Failure
ability Exception e where raise : e ->{Exception e} a

toException : Either e a ->{Exception e} a
toException = cases
    Left e  -> raise e
    Right a -> a

Exception.toEither.handler : Request {Exception e} a -> Either e a
Exception.toEither.handler = cases
    { a }          -> Right a
    {raise e -> _} -> Left e

Exception.toEither : '{g, Exception e} a ->{g} Either e a
Exception.toEither a = handle !a with Exception.toEither.handler

isNone = cases
  Some _ -> false
  None -> true

ability Stream a where
   emit: a -> ()

Stream.toList.handler : Request {Stream a} r -> [a]
Stream.toList.handler =
  go : [a] -> Request {Stream a} r -> [a]
  go acc = cases
    { Stream.emit a -> k } -> handle !k with go (acc :+ a)
    { _ } -> acc

  go []

Stream.toList : '{Stream a} r -> [a]
Stream.toList s = handle !s with toList.handler

Stream.collect.handler : Request {Stream a} r -> ([a],r)
Stream.collect.handler =
  go : [a] -> Request {Stream a} r -> ([a],r)
  go acc = cases
    { Stream.emit a -> k } -> handle !k with go (acc :+ a)
    { r } -> (acc, r)

  go []

Stream.collect : '{e, Stream a} r -> {e} ([a],r)
Stream.collect s =
  handle !s with Stream.collect.handler

-- Run tests which might fail, might create temporary directores and Stream out
-- results, returns the Results and the result of the test
evalTest: '{Stream Result, Exception Failure, io2.IO} a -> ([Result], Either Failure a)
evalTest a = handle
               (handle !a with Exception.toEither.handler)
             with Stream.collect.handler

-- Run tests which might fail, might create temporary directores and Stream out
-- results, but ignore the produced value and only return the test Results
runTest: '{Stream Result, Exception Failure, io2.IO} a -> [Result]
runTest t = match evalTest t with
              (results, Right _) -> results
              (results, Left (Failure _ t _)) -> results :+ (Fail t)


--
-- convenience functions for emitting test results
--
expect : (a -> Text) -> (a -> a -> Boolean) -> Text -> a -> a -> {Stream Result} ()
expect toText compare msg expected actual = let
  if (compare expected actual) then 
    emit (Ok msg) 
  else let
    failMsg = msg ++ "expected : " ++ (toText expected) ++ " actual: " ++ (toText actual)
    emit (Fail failMsg)

expectU : (a -> Text) -> Text -> a -> a -> {Stream Result} ()
expectU toText msg expected actual = expect toText (==) msg expected actual

check: Text -> Boolean -> {Stream Result} ()
check msg test = if test then emit (Ok msg) else emit (Fail msg)
```

### Creating server sockets

This section tests functions in the IO builtin related to binding to
TCP server socket, as to be able to accept incoming TCP connections.

```builtin
.io2.IO.serverSocket : Optional Text -> Text ->{io2.IO} Either Failure io2.Socket

```

This function takes two parameters, The first is the Hostname. If None
is provided, We will attempt to bind to 0.0.0.0 (All ipv4
addresses). We currently only support IPV4 (we should fix this!)
The second is the name of the port to bind to. This can be
a decimal representation of a port number between 1-65535. This can be
a named port like "ssh" (for port 22) or "kermit" (for port 1649),
This mapping of names to port numbers is maintained by the [nsswitch
service](https://en.wikipedia.org/wiki/Name_Service_Switch), typically
stored in `/etc/services` and queried with the `getent` tool:

    # map number to name
    $ getent services 22
    ssh                   22/tcp
    
    # map name to number
    $ getent services finger
    finger                79/tcp
    
    # get a list of all known names
    $ getent services | head
    tcpmux                1/tcp
    echo                  7/tcp
    echo                  7/udp
    discard               9/tcp sink null
    discard               9/udp sink null
    systat                11/tcp users
    daytime               13/tcp
    daytime               13/udp
    netstat               15/tcp
    qotd                  17/tcp quote

Below shows different examples of how we might specify the server coordinates.

```unison
testExplicitHost : '{io2.IO} [Result]
testExplicitHost _ = 
  test = 'let
    sock = toException (io2.IO.serverSocket (Some "127.0.0.1") "1028")
    emit (Ok "successfully created socket")
    port = toException (socketPort sock)
    putBytes (stdHandle StdOut) (toUtf8 (toText port))
    expectU Nat.toText "should have bound to port 1028" 1028 port 

  runTest test

testDefaultHost : '{io2.IO} [Result]
testDefaultHost _ = 
  test = 'let
    sock = toException (io2.IO.serverSocket None "1028")
    emit (Ok "successfully created socket")
    port = toException (socketPort sock)
    putBytes (stdHandle StdOut) (toUtf8 (toText port))
    expectU Nat.toText "should have bound to port 1028" 1028 port 

  runTest test

testDefaultPort : '{io2.IO} [Result]
testDefaultPort _ = 
  test = 'let
    sock = toException (io2.IO.serverSocket None "0")
    emit (Ok "successfully created socket")
    port = toException (socketPort sock)
    putBytes (stdHandle StdOut) (toUtf8 (toText port))
    
    check "port should be > 1024" (1024 < port)
    check "port should be < 65536" (65536 > port)

  runTest test
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      testDefaultHost  : '{io2.IO} [Result]
      testDefaultPort  : '{io2.IO} [Result]
      testExplicitHost : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    testDefaultHost  : '{io2.IO} [Result]
    testDefaultPort  : '{io2.IO} [Result]
    testExplicitHost : '{io2.IO} [Result]

.> io.test testDefaultPort

    New test results:
  
  ◉ testDefaultPort   successfully created socket
  ◉ testDefaultPort   port should be > 1024
  ◉ testDefaultPort   port should be < 65536
  
  ✅ 3 test(s) passing
  
  Tip: Use view testDefaultPort to view the source of a test.

```
This example demonstrates connecting a TCP client socket to a TCP server socket. A thread is started for both client and server. The server socket asks for any availalbe port (by passing "0" as the port number). The server thread then queries for the actual assigned port number, and puts that into an MVar which the client thread can read. The client thread then reads a string from the server and reports it back to the main thread via a different MVar.

```unison
serverThread: MVar Nat -> Text -> '{io2.IO}()
serverThread portVar toSend = 'let
  go = 'let
    sock = toException (serverSocket (Some "127.0.0.1") "0")
    port = toException (socketPort sock)
    toException (put portVar port)
    toException (listen sock)
    sock' = toException (socketAccept sock)
    toException (socketSend sock' (toUtf8 toSend))
    toException (closeSocket sock')

  match (toEither go) with 
    Left (Failure _ t _) -> watch t ()
    _ -> ()

clientThread : MVar Nat -> MVar Text -> '{io2.IO}()
clientThread portVar resultVar = 'let
  go : '{io2.IO, Exception Failure}()
  go = 'let
    port = toException (take portVar)
    sock = toException (clientSocket "127.0.0.1" (Nat.toText port))
    msg = toException (fromUtf8 (toException (socketReceive sock 100)))
    toException (MVar.put resultVar msg)

  match (toEither go) with
    Left (Failure _ t _) -> watch t ()
    _ -> ()

testTcpConnect : '{io2.IO}[Result]
testTcpConnect = 'let 
  test = 'let
    portVar = !MVar.newEmpty
    resultVar = !MVar.newEmpty
    
    toSend = "12345"

    forkComp (serverThread portVar toSend)
    forkComp (clientThread portVar resultVar)
    
    received = toException (MVar.take resultVar)

    expectU (a->a) "should have reaped what we've sown" toSend received
    
  runTest test
    
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      clientThread   : MVar Nat -> MVar Text -> '{io2.IO} ()
      serverThread   : MVar Nat -> Text -> '{io2.IO} ()
      testTcpConnect : '{io2.IO} [Result]

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    clientThread   : MVar Nat -> MVar Text -> '{io2.IO} ()
    serverThread   : MVar Nat -> Text -> '{io2.IO} ()
    testTcpConnect : '{io2.IO} [Result]

.> io.test testTcpConnect

    New test results:
  
  ◉ testTcpConnect   should have reaped what we've sown
  
  ✅ 1 test(s) passing
  
  Tip: Use view testTcpConnect to view the source of a test.

```
