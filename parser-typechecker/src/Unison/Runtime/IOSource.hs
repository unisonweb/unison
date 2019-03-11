{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}

module Unison.Runtime.IOSource where

import Control.Monad.Identity (runIdentity, Identity)
import Data.String (fromString)
import Data.Text (Text)
import Text.RawString.QQ (r)
import Unison.FileParsers (parseAndSynthesizeFile)
import Unison.Parser (Ann(..))
import Unison.Symbol (Symbol)
import qualified Unison.Builtin as Builtin
import qualified Unison.Result as Result
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF

typecheckedFile :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile = let
  tl :: a -> Identity (TL.TypeLookup Symbol Ann)
  tl = const $ pure (External <$ TL.builtinTypeLookup)
  r = parseAndSynthesizeFile [] tl Builtin.names "<IO.u builtin>" source
  in case runIdentity $ Result.runResultT r of
    (Nothing, notes) -> error $ "parsing failed: " <> show notes
    (Just (_ppe, Nothing), notes) -> error $ "typechecking failed" <> show notes
    (Just (_, Just file), _) -> file

source :: Text
source = fromString [r|

-- Handles are unique identifiers.
-- The implementation of IO in the runtime will supply Haskell
-- file handles and map those to Unison handles.
-- A pure implementation of I/O might use some kind of pure supply
-- of unique IDs instead.
type Handle = Handle Text

-- Ditto for sockets
type Socket = Socket Text

-- Builtin handles: standard in, out, error

namespace IO where
  stdin: Handle
  stdin = Handle "stdin"

  stdout: Handle
  stdout = Handle "stdout"

  stderr: Handle
  stderr = Handle "stderr"

  printLine : Text -> {IO} ()
  printLine t =
    IO.putText stdout t
    IO.putText stdout "\n"

-- IO Modes from the Haskell API
type IOMode = Read | Write | Append | ReadWrite

-- IO error types from the Haskell API
type IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError

type ErrorLocation = ErrorLocation Text
type ErrorDescription = ErrorDescription Text
type FilePath = FilePath Text

type IOError =
  IOError
    (Optional Handle)
    IOErrorType
    ErrorLocation
    ErrorDescription
    (Optional FilePath)

type SeekMode = Absolute | Relative | FromEnd

-- If the buffer size is not specified,
-- use an implementation-specific size.
type BufferMode = Line | Block (Optional Nat)

type EpochTime = EpochTime Nat

-- Either a host name e.g., "unisonweb.org" or a numeric host address
-- string consisting of a dotted decimal IPv4 address or an IPv6 address
-- e.g., "192.168.0.1".
type HostName = HostName Text

type PortNumber = Nat

-- Represents a 32-bit host address
type HostAddress = HostAddress Int

-- Internet protocol v4 socket address
type SocketAddress = SocketAddress HostAddress PortNumber


ability IO where

  -- Basic file IO
  openFile : FilePath -> IOMode ->{IO} Handle
  closeFile : Handle ->{IO} ()
  isEOF : Handle ->{IO} Boolean
  isFileOpen : Handle ->{IO} Boolean

  -- Text input and output

  --getChar : Handle ->{IO} Char
  getLine : Handle ->{IO} Text
  -- Get the entire contents of the file as text
  getText : Handle ->{IO} Text
  -- putChar : Handle -> Char ->{IO} ()
  putText : Handle -> Text ->{IO} ()

  -- Handling I/O errors.
  -- Question: can we do better?
  throw : IOError ->{IO} a
  catch : '{IO} a -> (IOError ->{IO} a) ->{IO} a

  -- File positioning
  isSeekable : Handle ->{IO} Boolean
  seek : Handle -> SeekMode -> Int ->{IO} ()
  position : Handle ->{IO} Int

  -- File buffering
  getBuffering : Handle ->{IO} (Optional BufferMode)
  setBuffering : Handle -> Optional BufferMode ->{IO} ()

  -- Should we expose mutable arrays for byte buffering?
  -- Inclined to say no, although that sounds a lot like
  -- a decision to just be slow.
  -- We'll need a byte buffer manipulation library in that case.

  -- getBytes : Handle -> Nat ->{IO} Bytes
  -- putBytes : Handle -> Bytes ->{IO} ()

  -- getBytes : Handle -> Nat -> ByteArray ->{IO} Nat
  -- putBytes : Handle -> Nat -> ByteArray ->{IO} ()

  systemTime : {IO} EpochTime


  -- File system operations
  getCurrentDirectory : {IO} FilePath
  setCurrentDirectory : FilePath ->{IO} ()
  directoryContents : FilePath ->{IO} [FilePath]
  fileExists : FilePath -> {IO} Boolean
  isDirectory : FilePath ->{IO} Boolean
  createDirectory : FilePath ->{IO} ()
  removeDirectory : FilePath ->{IO} ()
  renameDirectory : FilePath -> FilePath -> {IO} ()
  removeFile : FilePath ->{IO} ()
  renameFile : FilePath -> FilePath ->{IO} ()
  getFileTimestamp : FilePath ->{IO} EpochTime
  getFileSize : FilePath ->{IO} Nat


  -- Network I/O

  -- Glossing over address families (ipv4, ipv6),
  -- and socket types (stream, raw, etc)

  -- Creates a socket and binds it to a the given local port
  serverSocket : SocketAddress -> {IO} Socket

  -- Creates a socket connected to the given remote address
  clientSocket : SocketAddress -> {IO} Socket

  socketToHandle : Socket ->{IO} Handle
  handleToSocket : Handle ->{IO} Socket
  closeSocket : Socket ->{IO} ()

  -- Accept a connection on a socket.
  -- Returns a socket that can send and receive data on a new connection,
  -- together with the remote host information.
  accept : Socket ->{IO} (Socket, SocketAddress)

  -- Returns the number of bytes actually sent
  -- send : Socket -> Bytes ->{IO} Int

  -- scatter/gather mode network I/O
  -- sendMany : Socket -> [Bytes] ->{IO} Int

  -- Read the spefified number of bytes from the socket.
  -- receive : Socket -> Int ->{IO} Bytes
|]
