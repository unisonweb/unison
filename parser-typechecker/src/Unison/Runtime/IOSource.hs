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
import qualified Data.Map as Map
import qualified Unison.Builtin as Builtin
import qualified Unison.Reference as R
import qualified Unison.Result as Result
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF
import qualified Unison.Var as Var

typecheckedFile :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile = let
  tl :: a -> Identity (TL.TypeLookup Symbol Ann)
  tl = const $ pure (External <$ TL.builtinTypeLookup)
  r = parseAndSynthesizeFile [] tl Builtin.names "<IO.u builtin>" source
  in case runIdentity $ Result.runResultT r of
    (Nothing, notes) -> error $ "parsing failed: " <> show notes
    (Just (_ppe, Nothing), notes) -> error $ "typechecking failed" <> show notes
    (Just (_, Just file), _) -> file

typeNamed :: String -> R.Reference
typeNamed s =
  case Map.lookup (Var.nameds s) (UF.dataDeclarations' typecheckedFile) of
    Nothing -> error $ "No builtin type called: " <> s
    Just (r, _) -> r

abilityNamed :: String -> R.Reference
abilityNamed s =
  case Map.lookup (Var.nameds s) (UF.effectDeclarations' typecheckedFile) of
    Nothing -> error $ "No builtin ability called: " <> s
    Just (r, _) -> r

ioReference, bufferModeReference :: R.Reference
ioReference = abilityNamed "IO"
bufferModeReference = typeNamed "BufferMode"
-- .. todo - fill in the rest of these

source :: Text
source = fromString [r|

type Either a b = Left a | Right b

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

  rethrow : (Either IOError a) -> {IO} a
  rethrow x = case x of
    Either.Left e -> IO.throw e
    Either.Right a -> a

  printLine : Text -> {IO} ()
  printLine t =
    rethrow (IO.putText stdout t)
    rethrow (IO.putText stdout "\n")

  readLine : '{IO} Text
  readLine = '(rethrow (IO.getText stdin))

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
  openFile : FilePath -> IOMode ->{IO} (Either IOError Handle)
  closeFile : Handle ->{IO} (Either IOError ())
  isEOF : Handle ->{IO} (Either IOError Boolean)
  isFileOpen : Handle ->{IO} (Either IOError Boolean)

  -- Text input and output

  --getChar : Handle ->{IO} Char
  getLine : Handle ->{IO} (Either IOError Text)
  -- Get the entire contents of the file as text
  getText : Handle ->{IO} (Either IOError Text)
  -- putChar : Handle -> Char ->{IO} ()
  putText : Handle -> Text ->{IO} (Either IOError ())

  -- Note: `catch` is just a library function
  throw : IOError ->{IO} a

  -- File positioning
  isSeekable : Handle ->{IO} (Either IOError Boolean)
  seek : Handle -> SeekMode -> Int ->{IO} (Either IOError ())
  position : Handle ->{IO} (Either IOError Int)

  -- File buffering
  getBuffering : Handle ->{IO} Either IOError (Optional BufferMode)
  setBuffering : Handle -> Optional BufferMode ->{IO} (Either IOError ())

  -- Should we expose mutable arrays for byte buffering?
  -- Inclined to say no, although that sounds a lot like
  -- a decision to just be slow.
  -- We'll need a byte buffer manipulation library in that case.

  -- getBytes : Handle -> Nat ->{IO} Bytes
  -- putBytes : Handle -> Bytes ->{IO} ()

  -- getBytes : Handle -> Nat -> ByteArray ->{IO} Nat
  -- putBytes : Handle -> Nat -> ByteArray ->{IO} ()

  systemTime : {IO} (Either IOError EpochTime)


  -- File system operations
  getCurrentDirectory : {IO} (Either IOError FilePath)
  setCurrentDirectory : FilePath ->{IO} (Either IOError ())
  directoryContents : FilePath ->{IO} Either IOError [FilePath]
  fileExists : FilePath -> {IO} (Either IOError Boolean)
  isDirectory : FilePath ->{IO} (Either IOError Boolean)
  createDirectory : FilePath ->{IO} (Either IOError ())
  removeDirectory : FilePath ->{IO} (Either IOError ())
  renameDirectory : FilePath -> FilePath -> {IO} (Either IOError ())
  removeFile : FilePath ->{IO} (Either IOError ())
  renameFile : FilePath -> FilePath ->{IO} (Either IOError ())
  getFileTimestamp : FilePath ->{IO} (Either IOError EpochTime)
  getFileSize : FilePath ->{IO} (Either IOError Nat)


  -- Network I/O

  -- Glossing over address families (ipv4, ipv6),
  -- and socket types (stream, raw, etc)

  -- Creates a socket and binds it to a the given local port
  serverSocket : SocketAddress -> {IO} (Either IOError Socket)

  -- Creates a socket connected to the given remote address
  clientSocket : SocketAddress -> {IO} (Either IOError Socket)

  socketToHandle : Socket -> IOMode ->{IO} (Either IOError Handle)
  handleToSocket : Handle ->{IO} (Either IOError Socket)
  closeSocket : Socket ->{IO} (Either IOError ())

  -- Accept a connection on a socket.
  -- Returns a socket that can send and receive data on a new connection,
  -- together with the remote host information.
  accept : Socket ->{IO} Either IOError (Socket, SocketAddress)

  -- Returns the number of bytes actually sent
  -- send : Socket -> Bytes ->{IO} Int

  -- scatter/gather mode network I/O
  -- sendMany : Socket -> [Bytes] ->{IO} Int

  -- Read the spefified number of bytes from the socket.
  -- receive : Socket -> Int ->{IO} Bytes
|]
