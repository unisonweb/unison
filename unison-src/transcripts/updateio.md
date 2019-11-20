# A test for old versions of `IO`

Unison code written against older versions of the built-in `IO` type should keep
working. This test that they do.

## Test current IO

First let's test that the current version works:

```unison
use .base

testio.foo = '(io.printLine "Hello")
```

Let's see if it works:

```ucm
.> run testio.foo
```

Great! Now let's add an old version of `IO`:

```ucm
.> cd testio.old
```

```unison
type Either a b = Left a | Right b

type Optional a = None | Some a

-- This is linked to definitions that are considered tests
unique[e6dca08b40458b03ca1660cfbdaecaa7279b42d18257898b5fd1c34596aac36f] type
  IsTest = IsTest

-- Handles are unique identifiers.
-- The implementation of IO1 in the runtime will supply Haskell
-- file handles and map those to Unison handles.
-- A pure implementation of I/O might use some kind of pure supply
-- of unique IDs instead.
unique[d4597403ec40fd4fbee57c62b8096f9c3d382dff01f20108546fe3530a927e86] type
  io1.Handle = Handle Text

-- Ditto for sockets
unique[e1d94401fde8b2546d6dfc54e93f11e6a9285a7ea765d3255da19122a42715d3] type
  io.Socket = Socket Text

-- Builtin handles: standard in, out, error

use io Error Mode Handle IO1 Socket ThreadId HostName FilePath EpochTime
       BufferMode SeekMode ServiceName

use io1.Handle Handle

-- IO1 Modes from the Haskell API
type io.Mode = Read | Write | Append | ReadWrite

-- IO1 error types from the Haskell API
unique[bb57f367a3740d4a1608b9e0eee14fd744ec9e368f1529550cb436ef56c0b268] type
  io.ErrorType
    = AlreadyExists
    | NoSuchThing
    | ResourceBusy
    | ResourceExhausted
    | EOF
    | IllegalOperation
    | PermissionDenied
    | UserError

unique[b5c578f0a9977ed54a5a12b580dc6b0b2ba37bc3f517f48d1b3285a7f3e8c6bc] type
  io.ErrorLocation = ErrorLocation Text
unique[e6ca048b6bf540f93617c0ef9506afcbb490427a9581a01d51ffad39cdf2c554] type
  io.ErrorDescription = ErrorDescription Text
unique[d5d61b0a65f1d448dbdeed8af688f0bdbab6b3f775400da370eb5bfc34e428d5] type
  io.FilePath = FilePath Text

type io.Error = Error io.ErrorType Text

unique[cad7ab802bd143f0b674155c9caf18dde7145d16867a02659534d7bb01a5e287] type
  io.SeekMode = Absolute | Relative | FromEnd

-- If the buffer size is not specified,
-- use an implementation-specific size.
unique[e65de145a461a771de93d6c7885acae28552d77f8ae460bc8bf5de6f2a15ff77] type
  io.BufferMode = Line | Block (Optional Nat)

unique[e1f48f31982a720ae895c0bf4e6ea9a950f5c00d3a73101ad31e63461b7beded] type
  io.EpochTime = EpochTime Nat

-- Either a host name e.g., "unisonweb.org" or a numeric host address
-- string consisting of a dotted decimal IPv4 address
-- e.g., "192.168.0.1".
unique[c7279b501764751edc66f1f7b532e68354fc4704c9eb1ed201f01c894cdd86f4] type
  io.HostName = HostName Text

-- For example a port number like "8080"
unique[ee4ff0bda526b0513e4c7b7387b39811ce57938ddb31a77fdb0ff00ee2717c33] type
  io.ServiceName = ServiceName Text

unique[a38186de35c9fcd29d2b359b2148f9f890732413d91575af39d025fcded67e89] type
  io.ThreadId = ThreadId Text

ability IO1 where

  -- Basic file IO1
  openFile_ : io.FilePath -> io.Mode -> (Either io.Error io1.Handle)
  closeFile_ : io1.Handle -> (Either io.Error ())
  isFileEOF_ : io1.Handle -> (Either io.Error Boolean)
  isFileOpen_ : io1.Handle -> (Either io.Error Boolean)

  -- Text input and output

  --getChar : io1.Handle -> Char
  getLine_ : io1.Handle -> (Either io.Error Text)
  -- Get the entire contents of the file as text
  getText_ : io1.Handle -> (Either io.Error Text)
  -- putChar : io1.Handle -> Char -> ()
  putText_ : io1.Handle -> Text -> (Either io.Error ())

  -- Throw an error as an `IO1` effect
  throw : io.Error -> a

  -- File positioning
  isSeekable_ : io1.Handle -> (Either io.Error Boolean)
  seek_ : io1.Handle -> io.SeekMode -> Int -> (Either io.Error ())
  position_ : io1.Handle -> (Either io.Error Int)

  -- File buffering
  getBuffering_ : io1.Handle -> Either io.Error (Optional io.BufferMode)
  setBuffering_ : io1.Handle -> Optional io.BufferMode -> (Either io.Error ())

  -- Should we expose mutable arrays for byte buffering?
  -- Inclined to say no, although that sounds a lot like
  -- a decision to just be slow.
  -- We'll need a byte buffer manipulation library in that case.

  -- getBytes : io1.Handle -> Nat -> Bytes
  -- putBytes : io1.Handle -> Bytes -> ()

  -- getBytes : io1.Handle -> Nat -> ByteArray -> Nat
  -- putBytes : io1.Handle -> Nat -> ByteArray -> ()

  systemTime_ :  (Either io.Error io.EpochTime)

  -- File system operations
  getTemporaryDirectory_ :  (Either io.Error io.FilePath)
  getCurrentDirectory_ :  (Either io.Error io.FilePath)
  setCurrentDirectory_ : io.FilePath -> (Either io.Error ())
  directoryContents_ : io.FilePath -> Either io.Error [io.FilePath]
  fileExists_ : io.FilePath ->  (Either io.Error Boolean)
  isDirectory_ : io.FilePath -> (Either io.Error Boolean)
  createDirectory_ : io.FilePath -> (Either io.Error ())
  removeDirectory_ : io.FilePath -> (Either io.Error ())
  renameDirectory_ : io.FilePath -> io.FilePath ->  (Either io.Error ())
  removeFile_ : io.FilePath -> (Either io.Error ())
  renameFile_ : io.FilePath -> io.FilePath -> (Either io.Error ())
  getFileTimestamp_ : io.FilePath -> (Either io.Error io.EpochTime)
  getFileSize_ : io.FilePath -> (Either io.Error Nat)

  -- Simple TCP Networking

  -- Create a socket bound to the given local address.
  -- If a hostname is not given, this will use any available host.
  serverSocket_ : Optional io.HostName ->
                 io.ServiceName ->  (Either io.Error io.Socket)
  -- Start listening for connections
  listen_ : io.Socket -> (Either io.Error ())

  -- Create a socket connected to the given remote address
  clientSocket_ : io.HostName ->
                  io.ServiceName -> (Either io.Error io.Socket)

  closeSocket_ : io.Socket -> (Either io.Error ())

  --socketToio1.Handle : Socket -> Mode -> (Either io.Error io1.Handle)
  --handleToSocket : io1.Handle -> (Either io.Error Socket)

  -- Accept a connection on a socket.
  -- Returns a socket that can send and receive data on a new connection
  accept_ : io.Socket -> (Either io.Error io.Socket)

  -- Send some bytes to a socket.
  send_ : io.Socket -> Bytes -> (Either io.Error ())

  -- Read the spefified number of bytes from the socket.
  receive_ : io.Socket -> Nat -> (Either io.Error (Optional Bytes))

  -- scatter/gather mode network I/O
  -- sendMany : Socket -> [Bytes] -> Int

  -- Threading --

  -- Fork a thread
  fork_ : '{IO1} a -> (Either io.Error io.ThreadId)

  -- Kill a running thread
  kill_ : io.ThreadId -> (Either io.Error ())

  -- Suspend the current thread for a number of microseconds.
  delay_ : Nat -> (Either io.Error ())

  -- Safely acquire and release a resource
  bracket_ : '{IO1} a -> (a ->{IO1} b) -> (a ->{IO1} c) ->{IO1} (Either io.Error c)

namespace io1 where
  stdin : io1.Handle
  stdin = io1.Handle "stdin"

  stdout : io1.Handle
  stdout = io1.Handle "stdout"

  stderr : io1.Handle
  stderr = io1.Handle "stderr"

  -- Throw an I/O error on the left as an effect in `IO1`
  rethrow : (Either io.Error a) -> {IO1} a
  rethrow x = case x of
    Either.Left e -> IO1.throw e
    Either.Right a -> a

  -- Write some text to a file
  putText : Handle -> Text ->{IO1} ()
  putText h t = io1.rethrow (IO1.putText_ h t)

  -- Print a line to the standard output
  printLine : Text ->{IO1} ()
  printLine t =
    io1.putText io1.stdout t
    io1.putText io1.stdout "\n"

foo = '(io.printLine "Hello")

```

Let's see if _that_ works:

```ucm
.testio1> run foo
```
