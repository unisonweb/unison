{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language QuasiQuotes #-}

module Unison.Runtime.IOSource where

import Unison.Prelude

import Control.Lens (view, _1)
import Control.Monad.Identity (runIdentity, Identity)
import Data.List (elemIndex, genericIndex)
import Text.RawString.QQ (r)
import Unison.Codebase.CodeLookup (CodeLookup(..))
import Unison.FileParsers (parseAndSynthesizeFile)
import Unison.Parser (Ann(..))
import Unison.Symbol (Symbol)
import qualified Data.Map as Map
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase.CodeLookup as CL
import qualified Unison.DataDeclaration as DD
import qualified Unison.Parser as Parser
import qualified Unison.Reference as R
import qualified Unison.Result as Result
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.UnisonFile as UF
import qualified Unison.Var as Var
import qualified Unison.Names3 as Names

typecheckedFile :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile = typecheckedFile'

typecheckedFile' :: forall v. Var.Var v => UF.TypecheckedUnisonFile v Ann
typecheckedFile' = let
  tl :: a -> Identity (TL.TypeLookup v Ann)
  tl = const $ pure (External <$ Builtin.typeLookup)
  env = Parser.ParsingEnv mempty (Names.Names Builtin.names0 mempty)
  r = parseAndSynthesizeFile [] tl env "<IO.u builtin>" source
  in case runIdentity $ Result.runResultT r of
    (Nothing, notes) -> error $ "parsing failed: " <> show notes
    (Just Left{}, notes) -> error $ "typechecking failed" <> show notes
    (Just (Right file), _) -> file

typecheckedFileTerms :: Map.Map Symbol R.Reference
typecheckedFileTerms = view _1 <$> UF.hashTerms typecheckedFile

termNamed :: String -> R.Reference
termNamed s = fromMaybe (error $ "No builtin term called: " <> s)
  $ Map.lookup (Var.nameds s) typecheckedFileTerms

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

ioReference, bufferModeReference, eitherReference, ioModeReference, optionReference, errorReference, errorTypeReference, seekModeReference, threadIdReference, socketReference, handleReference, epochTimeReference, isTestReference, isPropagatedReference, filePathReference
  :: R.Reference
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

isTest :: (R.Reference, R.Reference)
isTest = (isTestReference, termNamed "metadata.isTest")

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

alreadyExistsId, noSuchThingId, resourceBusyId, resourceExhaustedId, eofId, illegalOperationId, permissionDeniedId, userErrorId
  :: DD.ConstructorId
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
      error
        $  "There's a bug in the Unison runtime. Couldn't find type "
        <> show ref
    Just decl ->
      fromMaybe
          (  error
          $  "Unison runtime bug. The type "
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
      error
        $  "There's a bug in the Unison runtime. Couldn't find type "
        <> show ref
    Just decl -> genericIndex (DD.constructorNames $ DD.asDataDecl decl) cid

-- .. todo - fill in the rest of these

source :: Text
source = fromString [r|

type Either a b = Left a | Right b

type Optional a = None | Some a

unique[b28d929d0a73d2c18eac86341a3bb9399f8550c11b5f35eabb2751e6803ccc20] type
  IsPropagated = IsPropagated

d1 Doc.++ d2 =
  use Doc
  match (d1,d2) with
    (Join ds, Join ds2) -> Join (ds Sequence.++ ds2)
    (Join ds, _) -> Join (ds `Sequence.snoc` d2)
    (_, Join ds) -> Join (d1 `Sequence.cons` ds)
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

-- Builtin handles: standard in, out, error

use io Error Mode Handle IO Socket ThreadId HostName FilePath EpochTime
       BufferMode SeekMode ServiceName

use io.Handle Handle

namespace io where
  stdin : Handle
  stdin = Handle "stdin"

  stdout : Handle
  stdout = Handle "stdout"

  stderr : Handle
  stderr = Handle "stderr"

  -- Throw an I/O error on the left as an effect in `IO`
  rethrow : (Either io.Error a) -> {IO} a
  rethrow x = match x with
    Either.Left e -> io.IO.throw e
    Either.Right a -> a

  -- Print a line to the standard output
  printLine : Text ->{IO} ()
  printLine t =
    putText stdout t
    putText stdout "\n"

  -- Read a line from the standard input
  readLine : '{IO} Text
  readLine = '(getLine stdin)

  -- Built-ins

  -- Open a named file in the given mode, yielding an open file handle
  openFile : FilePath -> Mode ->{IO} Handle
  openFile f m = rethrow (io.IO.openFile_ f m)

  -- Close an open file handle
  closeFile : Handle ->{IO} ()
  closeFile f = rethrow (io.IO.closeFile_ f)

  -- Check whether a file handle has reached the end of the file
  isFileEOF : Handle ->{IO} Boolean
  isFileEOF h = rethrow (io.IO.isFileEOF_ h)

  -- Check whether a file handle is open
  isFileOpen : Handle ->{IO} Boolean
  isFileOpen h = rethrow (io.IO.isFileOpen_ h)

  -- Get a line of text from a text file handle
  getLine : Handle ->{IO} Text
  getLine h = rethrow (io.IO.getLine_ h)

  -- Get the entire contents of a file as a single block of text
  getText : Handle ->{IO} Text
  getText h = rethrow (io.IO.getText_ h)

  -- Write some text to a file
  putText : Handle -> Text ->{IO} ()
  putText h t = rethrow (io.IO.putText_ h t)

  -- Get epoch system time
  systemTime : '{IO} EpochTime
  systemTime = '(rethrow (io.IO.systemTime_))

  -- Does the file handle support `seek`?
  isSeekable : Handle -> {IO} Boolean
  isSeekable h = rethrow (io.IO.isSeekable_ h)

  -- Seek to a position in a file handle
  seek : Handle -> SeekMode -> Int ->{IO} ()
  seek h m i = rethrow (io.IO.seek_ h m i)

  -- Ask for the position of a file handle
  position : Handle ->{IO} Int
  position h = rethrow (io.IO.position_ h)

  -- Get the buffer mode of a file handle
  getBuffering : Handle ->{IO} (Optional BufferMode)
  getBuffering h = rethrow (io.IO.getBuffering_ h)

  -- Set the buffer mode for a file handle
  setBuffering : Handle -> Optional BufferMode ->{IO} ()
  setBuffering h bm = rethrow (io.IO.setBuffering_ h bm)

  -- Get the path to a temporary directory managed by the operating system
  getTemporaryDirectory : '{IO} FilePath
  getTemporaryDirectory = '(rethrow (io.IO.getTemporaryDirectory_))

  -- Get the current working directory
  getCurrentDirectory : '{IO} FilePath
  getCurrentDirectory = '(rethrow (io.IO.getCurrentDirectory_))

  -- Set the current working directory
  setCurrentDirectory : FilePath -> {IO} ()
  setCurrentDirectory d = rethrow (io.IO.setCurrentDirectory_ d)

  -- List the contents of a directory
  directoryContents : FilePath -> {IO} [FilePath]
  directoryContents d = rethrow (io.IO.directoryContents_ d)

  -- Check if a path exists
  fileExists : FilePath -> {IO} Boolean
  fileExists d = rethrow (io.IO.fileExists_ d)

  -- Check if a path is a directory
  isDirectory : FilePath -> {IO} Boolean
  isDirectory d = rethrow (io.IO.isDirectory_ d)

  -- Create a directory at the given path, including parent directories
  createDirectory : FilePath -> {IO} ()
  createDirectory d = rethrow (io.IO.createDirectory_ d)

  -- Remove the directory at the given path
  removeDirectory : FilePath -> {IO} ()
  removeDirectory d = rethrow (io.IO.removeDirectory_ d)

  -- Move a directory from one path to another
  renameDirectory : FilePath -> FilePath -> {IO} ()
  renameDirectory from to = rethrow (io.IO.renameDirectory_ from to)

  -- Remove a file from the file system
  removeFile : FilePath -> {IO} ()
  removeFile d = rethrow (io.IO.removeFile_ d)

  -- Move a file from one path to another
  renameFile : FilePath -> FilePath -> {IO} ()
  renameFile from to = rethrow (io.IO.renameFile_ from to)

  -- Get the timestamp of a file
  getFileTimestamp : FilePath -> {IO} EpochTime
  getFileTimestamp d = rethrow (io.IO.getFileTimestamp_ d)

  -- Get the size of a file in bytes
  getFileSize : FilePath -> {IO} Nat
  getFileSize d = rethrow (io.IO.getFileSize_ d)

  -- Create a socket bound to the given local port/service.
  -- If a hostname is not given, this will use any available host.
  serverSocket : Optional HostName -> ServiceName -> {IO} Socket
  serverSocket host service = rethrow (io.IO.serverSocket_ host service)

  -- Start listening for connections on the given socket.
  listen : Socket -> {IO} ()
  listen s = rethrow (io.IO.listen_ s)

  -- Create a socket connected to the given remote address.
  clientSocket : HostName -> ServiceName -> {IO} Socket
  clientSocket host service = rethrow (io.IO.clientSocket_ host service)

  -- Close a socket and all connections to it.
  closeSocket : Socket -> {IO} ()
  closeSocket s = rethrow (io.IO.closeSocket_ s)

  -- Accept a connection on a socket.
  -- Returns a socket that can send and receive data on a new connection
  accept : Socket -> {IO} Socket
  accept s = rethrow (io.IO.accept_ s)

  -- Send some bytes to a socket.
  send : Socket -> Bytes -> {IO} ()
  send s bs = rethrow (io.IO.send_ s bs)

  -- Read the specified number of bytes from a socket.
  receive : Socket -> Nat ->{IO} (Optional Bytes)
  receive s n = rethrow (io.IO.receive_ s n)

  -- Fork a new thread.
  fork : '{IO} a -> {IO} ThreadId
  fork a = rethrow (io.IO.fork_ a)

  -- Kill a running thread.
  kill : ThreadId -> {IO} ()
  kill t = rethrow (io.IO.kill_ t)

  -- Suspend the current thread for a number of microseconds.
  delay : Nat -> {IO} ()
  delay n = rethrow (io.IO.delay_ n)

  -- Safely acquire and release a resource
  bracket : '{IO} a -> (a ->{IO} b) -> (a ->{IO} c) -> {IO} c
  bracket acquire release what = rethrow (io.IO.bracket_ acquire release what)

  -- Run the given computation, and if it throws an error
  -- handle the error with the given handler.
  -- catch : '{IO} a -> (io.Error ->{IO} a) ->{IO} a
  -- catch c h =
  --   k io = match io with
  --            { IO.throw e } -> h e
  --            x -> x
  --   handle k in c

-- IO Modes from the Haskell API
type io.Mode = Read | Write | Append | ReadWrite

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

  --socketToio.Handle : Socket -> Mode -> (Either io.Error io.Handle)
  --handleToSocket : io.Handle -> (Either io.Error Socket)

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

|]
