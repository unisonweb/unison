{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.IOSource where

import Control.Lens (view, _1)
import Control.Monad.Identity (Identity, runIdentity)
import Data.List (elemIndex, genericIndex)
import qualified Data.Map as Map
import Text.RawString.QQ (r)
import qualified Unison.Builtin as Builtin
import Unison.Codebase.CodeLookup (CodeLookup (..))
import qualified Unison.Codebase.CodeLookup as CL
import qualified Unison.DataDeclaration as DD
import Unison.FileParsers (parseAndSynthesizeFile)
import qualified Unison.Names3 as Names
import Unison.Parser (Ann (..))
import qualified Unison.Parser as Parser
import Unison.Prelude
import qualified Unison.Reference as R
import qualified Unison.Result as Result
import Unison.Symbol (Symbol)
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF
import qualified Unison.Var as Var

typecheckedFile :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile = typecheckedFile'

typecheckedFile' :: forall v. Var.Var v => UF.TypecheckedUnisonFile v Ann
typecheckedFile' =
  let tl :: a -> Identity (TL.TypeLookup v Ann)
      tl = const $ pure (External <$ Builtin.typeLookup)
      env = Parser.ParsingEnv mempty (Names.Names Builtin.names0 mempty)
      r = parseAndSynthesizeFile [] tl env "<IO.u builtin>" source
   in case runIdentity $ Result.runResultT r of
        (Nothing, notes) -> error $ "parsing failed: " <> show notes
        (Just Left {}, notes) -> error $ "typechecking failed" <> show notes
        (Just (Right file), _) -> file

typecheckedFileTerms :: Map.Map Symbol R.Reference
typecheckedFileTerms = view _1 <$> UF.hashTerms typecheckedFile

termNamed :: String -> R.Reference
termNamed s =
  fromMaybe (error $ "No builtin term called: " <> s) $
    Map.lookup (Var.nameds s) typecheckedFileTerms

codeLookup :: CodeLookup Symbol Identity Ann
codeLookup = CL.fromUnisonFile $ UF.discardTypes typecheckedFile

typeNamedId :: String -> R.Id
typeNamedId s =
  case Map.lookup (Var.nameds s) (UF.dataDeclarationsId' typecheckedFile) of
    Nothing -> error $ "No builtin type called: " <> s
    Just (r, _) -> r

typeNamed :: String -> R.Reference
typeNamed = R.DerivedId . typeNamedId

abilityNamedId :: String -> R.Id
abilityNamedId s =
  case Map.lookup (Var.nameds s) (UF.effectDeclarationsId' typecheckedFile) of
    Nothing -> error $ "No builtin ability called: " <> s
    Just (r, _) -> r

ioHash :: R.Id
ioHash = abilityNamedId "io.IO"

ioReference,
  bufferModeReference,
  eitherReference,
  ioModeReference,
  optionReference,
  errorReference,
  errorTypeReference,
  seekModeReference,
  threadIdReference,
  socketReference,
  handleReference,
  epochTimeReference,
  isTestReference,
  isPropagatedReference,
  filePathReference,
  hostNameReference,
  serviceNameReference,
  failureReference,
  tlsFailureReference,
  ioFailureReference ::
    R.Reference
ioReference = R.DerivedId ioHash
bufferModeReference = typeNamed "io.BufferMode"
eitherReference = typeNamed "Either"
ioModeReference = typeNamed "io.Mode"
optionReference = typeNamed "Optional"
errorReference = typeNamed "io.Error"
errorTypeReference = typeNamed "io.ErrorType"
seekModeReference = typeNamed "io.SeekMode"
threadIdReference = typeNamed "io.ThreadId"
socketReference = typeNamed "io.Socket"
handleReference = typeNamed "io.Handle"
epochTimeReference = typeNamed "io.EpochTime"
isTestReference = typeNamed "IsTest"
isPropagatedReference = typeNamed "IsPropagated"
filePathReference = typeNamed "io.FilePath"
hostNameReference = typeNamed "io.HostName"
serviceNameReference = typeNamed "io.ServiceName"
failureReference = typeNamed "io2.Failure"
tlsFailureReference = typeNamed "io2.TlsFailure"
ioFailureReference = typeNamed "io2.IOFailure"

isTest :: (R.Reference, R.Reference)
isTest = (isTestReference, termNamed "metadata.isTest")

isIOTest :: (R.Reference, R.Reference)
isIOTest = (isTestReference, termNamed "metadata.isIOTest")

isPropagatedValue :: R.Reference
isPropagatedValue = termNamed "metadata.isPropagated"

eitherLeftId, eitherRightId, someId, noneId, ioErrorId, handleId, socketId, threadIdId, epochTimeId, bufferModeLineId, bufferModeBlockId, filePathId :: DD.ConstructorId
eitherLeftId = constructorNamed eitherReference "Either.Left"
eitherRightId = constructorNamed eitherReference "Either.Right"
someId = constructorNamed optionReference "Optional.Some"
noneId = constructorNamed optionReference "Optional.None"
ioErrorId = constructorNamed errorReference "io.Error.Error"
handleId = constructorNamed handleReference "io.Handle.Handle"
socketId = constructorNamed socketReference "io.Socket.Socket"
threadIdId = constructorNamed threadIdReference "io.ThreadId.ThreadId"
epochTimeId = constructorNamed epochTimeReference "io.EpochTime.EpochTime"
bufferModeLineId = constructorNamed bufferModeReference "io.BufferMode.Line"
bufferModeBlockId = constructorNamed bufferModeReference "io.BufferMode.Block"
filePathId = constructorNamed filePathReference "io.FilePath.FilePath"

mkErrorType :: Text -> DD.ConstructorId
mkErrorType = constructorNamed errorTypeReference

alreadyExistsId,
  noSuchThingId,
  resourceBusyId,
  resourceExhaustedId,
  eofId,
  illegalOperationId,
  permissionDeniedId,
  userErrorId ::
    DD.ConstructorId
alreadyExistsId = mkErrorType "io.ErrorType.AlreadyExists"
noSuchThingId = mkErrorType "io.ErrorType.NoSuchThing"
resourceBusyId = mkErrorType "io.ErrorType.ResourceBusy"
resourceExhaustedId = mkErrorType "io.ErrorType.ResourceExhausted"
eofId = mkErrorType "io.ErrorType.EOF"
illegalOperationId = mkErrorType "io.ErrorType.IllegalOperation"
permissionDeniedId = mkErrorType "io.ErrorType.PermissionDenied"
userErrorId = mkErrorType "io.ErrorType.UserError"

constructorNamed :: R.Reference -> Text -> DD.ConstructorId
constructorNamed ref name =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error $
        "There's a bug in the Unison runtime. Couldn't find type "
          <> show ref
    Just decl ->
      fromMaybe
        ( error $
            "Unison runtime bug. The type "
              <> show ref
              <> " has no constructor named "
              <> show name
        )
        . elemIndex name
        . DD.constructorNames
        $ DD.asDataDecl decl

constructorName :: R.Reference -> DD.ConstructorId -> Text
constructorName ref cid =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error $
        "There's a bug in the Unison runtime. Couldn't find type "
          <> show ref
    Just decl -> genericIndex (DD.constructorNames $ DD.asDataDecl decl) cid

-- .. todo - fill in the rest of these

source :: Text
source =
  fromString
    [r|

type Either a b = Left a | Right b

type Optional a = None | Some a

unique[b28d929d0a73d2c18eac86341a3bb9399f8550c11b5f35eabb2751e6803ccc20] type
  IsPropagated = IsPropagated

d1 Doc.++ d2 =
  use Doc
  match (d1,d2) with
    (Join ds, Join ds2) -> Join (ds List.++ ds2)
    (Join ds, _) -> Join (ds `List.snoc` d2)
    (_, Join ds) -> Join (d1 `List.cons` ds)
    _ -> Join [d1,d2]

unique[q1905679b27a97a4098bc965574da880c1074183a2c55ff1d481619c7fb8a1e1] type
  Author = { guid : GUID, name : Text }

unique[ee1c051034fa0671ea66e7c708ba552003bd3cf657bd28bf0051f1f8cdfcba53] type
  CopyrightHolder = { guid : GUID, name : Text}

unique[bed6724af0d5f47f80cdea1b6023d35f120137ee0556e57154a9fc8b62fe5fed] type
  License = { copyrightHolders : [CopyrightHolder]
            , years : [Year]
            , licenseType : LicenseType }

-- Use `Doc` here to get nice text-wrapping when viewing
-- and to avoid needing to stick hard line breaks in the license
unique[d875fa1ea7ef3adf8e29417c6c8b01a1830c4c6bd10dcca9d4196388462e0b7a] type LicenseType = LicenseType Doc

unique[cb8469a1b41a63655062226556eaccf06129a2641af61fe7edef9c485c94a870] type GUID = GUID Bytes

-- Common era years
unique[u9ae6694152966cf1b0c1f4ad901a77e1acd7bbe16595fd27b07435ac45dab05] type Year = Year Nat

-- This is linked to definitions that are considered tests
unique[e6dca08b40458b03ca1660cfbdaecaa7279b42d18257898b5fd1c34596aac36f] type
  IsTest = IsTest

-- Create references for these that can be used as metadata.
-- (Reminder: Metadata is references, not values.)
metadata.isTest = IsTest.IsTest
metadata.isPropagated = IsPropagated.IsPropagated

-- Handles are unique identifiers.
-- The implementation of IO in the runtime will supply Haskell
-- file handles and map those to Unison handles.
-- A pure implementation of I/O might use some kind of pure supply
-- of unique IDs instead.
unique[d4597403ec40fd4fbee57c62b8096f9c3d382dff01f20108546fe3530a927e86] type
  io.Handle = Handle Text

-- Ditto for sockets
unique[e1d94401fde8b2546d6dfc54e93f11e6a9285a7ea765d3255da19122a42715d3] type
  io.Socket = Socket Text

-- IO error types from the Haskell API
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

-- IO Modes from the Haskell API
type io.Mode = Read | Write | Append | ReadWrite

use io IO
use io.Handle

ability io.IO where

  -- Basic file IO
  openFile_ : io.FilePath -> io.Mode -> (Either io.Error io.Handle)
  closeFile_ : io.Handle -> (Either io.Error ())
  isFileEOF_ : io.Handle -> (Either io.Error Boolean)
  isFileOpen_ : io.Handle -> (Either io.Error Boolean)

  -- Text input and output

  --getChar : io.Handle -> Char
  getLine_ : io.Handle -> (Either io.Error Text)
  -- Get the entire contents of the file as text
  getText_ : io.Handle -> (Either io.Error Text)
  -- putChar : io.Handle -> Char -> ()
  putText_ : io.Handle -> Text -> (Either io.Error ())

  -- Throw an error as an `io.IO` effect
  throw : io.Error -> a

  -- File positioning
  isSeekable_ : io.Handle -> (Either io.Error Boolean)
  seek_ : io.Handle -> io.SeekMode -> Int -> (Either io.Error ())
  position_ : io.Handle -> (Either io.Error Int)

  -- File buffering
  getBuffering_ : io.Handle -> Either io.Error (Optional io.BufferMode)
  setBuffering_ : io.Handle -> Optional io.BufferMode -> (Either io.Error ())

  -- Should we expose mutable arrays for byte buffering?
  -- Inclined to say no, although that sounds a lot like
  -- a decision to just be slow.
  -- We'll need a byte buffer manipulation library in that case.

  -- getBytes : io.Handle -> Nat -> Bytes
  -- putBytes : io.Handle -> Bytes -> ()

  -- getBytes : io.Handle -> Nat -> ByteArray -> Nat
  -- putBytes : io.Handle -> Nat -> ByteArray -> ()

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

  --socketToHandle : Socket -> Mode -> (Either io.Error Handle)
  --handleToSocket : Handle -> (Either io.Error Socket)

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
  fork_ : '{io.IO} a -> (Either io.Error io.ThreadId)

  -- Kill a running thread
  kill_ : io.ThreadId -> (Either io.Error ())

  -- Suspend the current thread for a number of microseconds.
  delay_ : Nat -> (Either io.Error ())

  -- Safely acquire and release a resource
  bracket_ : '{io.IO} a -> (a ->{io.IO} b) -> (a ->{io.IO} c) ->{io.IO} (Either io.Error c)

-- Builtin handles: standard in, out, error

io.stdin : io.Handle
io.stdin = Handle "stdin"

io.stdout : io.Handle
io.stdout = Handle "stdout"

io.stderr : io.Handle
io.stderr = Handle "stderr"

-- Throw an I/O error on the left as an effect in `IO`
io.rethrow : (Either io.Error a) -> {IO} a
io.rethrow x = match x with
    Either.Left e -> io.IO.throw e
    Either.Right a -> a

-- Print a line to the standard output
io.printLine : Text ->{IO} ()
io.printLine t =
  io.putText stdout t
  io.putText stdout "\n"

-- Read a line from the standard input
io.readLine : '{IO} Text
io.readLine = '(io.getLine stdin)

-- Built-ins

-- Open a named file in the given mode, yielding an open file handle
io.openFile : io.FilePath -> io.Mode ->{IO} io.Handle
io.openFile f m = io.rethrow (io.IO.openFile_ f m)

-- Close an open file handle
io.closeFile : io.Handle ->{IO} ()
io.closeFile f = io.rethrow (io.IO.closeFile_ f)

-- Check whether a file handle has reached the end of the file
io.isFileEOF : io.Handle ->{IO} Boolean
io.isFileEOF h = io.rethrow (io.IO.isFileEOF_ h)

-- Check whether a file handle is open
io.isFileOpen : io.Handle ->{IO} Boolean
io.isFileOpen h = io.rethrow (io.IO.isFileOpen_ h)

-- Get a line of text from a text file handle
io.getLine : io.Handle ->{IO} Text
io.getLine h = io.rethrow (io.IO.getLine_ h)

-- Get the entire contents of a file as a single block of text
io.getText : io.Handle ->{IO} Text
io.getText h = io.rethrow (io.IO.getText_ h)

-- Write some text to a file
io.putText : io.Handle -> Text ->{IO} ()
io.putText h t = io.rethrow (io.IO.putText_ h t)

-- Get epoch system time
io.systemTime : '{IO} io.EpochTime
io.systemTime = '(io.rethrow (io.IO.systemTime_))

-- Does the file handle support `seek`?
io.isSeekable : io.Handle -> {IO} Boolean
io.isSeekable h = io.rethrow (io.IO.isSeekable_ h)

-- Seek to a position in a file handle
io.seek : io.Handle -> io.SeekMode -> Int ->{IO} ()
io.seek h m i = io.rethrow (io.IO.seek_ h m i)

-- Ask for the position of a file handle
io.position : io.Handle ->{IO} Int
io.position h = io.rethrow (io.IO.position_ h)

-- Get the buffer mode of a file handle
io.getBuffering : io.Handle ->{IO} (Optional io.BufferMode)
io.getBuffering h = io.rethrow (io.IO.getBuffering_ h)

-- Set the buffer mode for a file handle
io.setBuffering : io.Handle -> Optional io.BufferMode ->{IO} ()
io.setBuffering h bm = io.rethrow (io.IO.setBuffering_ h bm)

-- Get the path to a temporary directory managed by the operating system
io.getTemporaryDirectory : '{IO} io.FilePath
io.getTemporaryDirectory = '(io.rethrow (io.IO.getTemporaryDirectory_))

-- Get the current working directory
io.getCurrentDirectory : '{IO} io.FilePath
io.getCurrentDirectory = '(io.rethrow (io.IO.getCurrentDirectory_))

-- Set the current working directory
io.setCurrentDirectory : io.FilePath -> {IO} ()
io.setCurrentDirectory d = io.rethrow (io.IO.setCurrentDirectory_ d)

-- List the contents of a directory
io.directoryContents : io.FilePath -> {IO} [io.FilePath]
io.directoryContents d = io.rethrow (io.IO.directoryContents_ d)

-- Check if a path exists
io.fileExists : io.FilePath -> {IO} Boolean
io.fileExists d = io.rethrow (io.IO.fileExists_ d)

-- Check if a path is a directory
io.isDirectory : io.FilePath -> {IO} Boolean
io.isDirectory d = io.rethrow (io.IO.isDirectory_ d)

-- Create a directory at the given path, including parent directories
io.createDirectory : io.FilePath -> {IO} ()
io.createDirectory d = io.rethrow (io.IO.createDirectory_ d)

-- Remove the directory at the given path
io.removeDirectory : io.FilePath -> {IO} ()
io.removeDirectory d = io.rethrow (io.IO.removeDirectory_ d)

-- Move a directory from one path to another
io.renameDirectory : io.FilePath -> io.FilePath -> {IO} ()
io.renameDirectory from to = io.rethrow (io.IO.renameDirectory_ from to)

-- Remove a file from the file system
io.removeFile : io.FilePath -> {IO} ()
io.removeFile d = io.rethrow (io.IO.removeFile_ d)

-- Move a file from one path to another
io.renameFile : io.FilePath -> io.FilePath -> {IO} ()
io.renameFile from to = io.rethrow (io.IO.renameFile_ from to)

-- Get the timestamp of a file
io.getFileTimestamp : io.FilePath -> {IO} io.EpochTime
io.getFileTimestamp d = io.rethrow (io.IO.getFileTimestamp_ d)

-- Get the size of a file in bytes
io.getFileSize : io.FilePath -> {IO} Nat
io.getFileSize d = io.rethrow (io.IO.getFileSize_ d)

-- Create a socket bound to the given local port/service.
-- If a hostname is not given, this will use any available host.
io.serverSocket : Optional io.HostName -> io.ServiceName -> {IO} io.Socket
io.serverSocket host service = io.rethrow (io.IO.serverSocket_ host service)

-- Start listening for connections on the given socket.
io.listen : io.Socket -> {IO} ()
io.listen s = io.rethrow (io.IO.listen_ s)

-- Create a socket connected to the given remote address.
io.clientSocket : io.HostName -> io.ServiceName -> {IO} io.Socket
io.clientSocket host service = io.rethrow (io.IO.clientSocket_ host service)

-- Close a socket and all connections to it.
io.closeSocket : io.Socket -> {IO} ()
io.closeSocket s = io.rethrow (io.IO.closeSocket_ s)

-- Accept a connection on a socket.
-- Returns a socket that can send and receive data on a new connection
io.accept : io.Socket -> {IO} io.Socket
io.accept s = io.rethrow (io.IO.accept_ s)

-- Send some bytes to a socket.
io.send : io.Socket -> Bytes -> {IO} ()
io.send s bs = io.rethrow (io.IO.send_ s bs)

-- Read the specified number of bytes from a socket.
io.receive : io.Socket -> Nat ->{IO} (Optional Bytes)
io.receive s n = io.rethrow (io.IO.receive_ s n)

-- Fork a new thread.
io.fork : '{IO} a -> {IO} io.ThreadId
io.fork a = io.rethrow (io.IO.fork_ a)

-- Kill a running thread.
io.kill : io.ThreadId -> {IO} ()
io.kill t = io.rethrow (io.IO.kill_ t)

-- Suspend the current thread for a number of microseconds.
io.delay : Nat -> {IO} ()
io.delay n = io.rethrow (io.IO.delay_ n)

-- Safely acquire and release a resource
io.bracket : '{IO} a -> (a ->{IO} b) -> (a ->{IO} c) -> {IO} c
io.bracket acquire release what = io.rethrow (io.IO.bracket_ acquire release what)

  -- Run the given computation, and if it throws an error
  -- handle the error with the given handler.
  -- catch : '{IO} a -> (Error ->{IO} a) ->{IO} a
  -- catch c h =
  --   k io = match io with
  --            { IO.throw e } -> h e
  --            x -> x
  --   handle k in c
|]
