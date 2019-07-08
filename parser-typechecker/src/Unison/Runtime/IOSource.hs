{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language QuasiQuotes #-}

module Unison.Runtime.IOSource where

import Control.Lens (view, _1)
import Control.Monad.Identity (runIdentity, Identity)
import Data.List (elemIndex, genericIndex)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
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
typecheckedFile = let
  tl :: a -> Identity (TL.TypeLookup Symbol Ann)
  tl = const $ pure (External <$ Builtin.typeLookup)
  ctorType = TL.unsafeConstructorType (Builtin.typeLookup @Symbol)
  env = Parser.ParsingEnv mempty (Names.Names Builtin.names0 mempty) ctorType
  r = parseAndSynthesizeFile [] tl env "<IO.u builtin>" source
  in case runIdentity $ Result.runResultT r of
    (Nothing, notes) -> error $ "parsing failed: " <> show notes
    (Just Nothing, notes) -> error $ "typechecking failed" <> show notes
    (Just (Just file), _) -> file

typecheckedFileTerms :: Map.Map Symbol R.Reference
typecheckedFileTerms = view _1 <$> UF.hashTerms typecheckedFile

termNamed :: String -> R.Reference
termNamed s = case Map.lookup (Var.nameds s) typecheckedFileTerms of
  Nothing -> error $ "No builtin term called: " <> s
  Just r  -> r

codeLookup :: CodeLookup Symbol Identity Ann
codeLookup = CL.fromUnisonFile $ UF.discardTypes typecheckedFile

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

ioHash, eitherHash, ioModeHash :: R.Id
ioHash = R.unsafeId ioReference
eitherHash = R.unsafeId eitherReference
ioModeHash = R.unsafeId ioModeReference

ioReference, bufferModeReference, eitherReference, ioModeReference, optionReference, errorReference, errorTypeReference, seekModeReference, threadIdReference, socketReference, handleReference, epochTimeReference, isTestReference
  :: R.Reference
ioReference = abilityNamed "IO"
bufferModeReference = typeNamed "BufferMode"
eitherReference = typeNamed "Either"
ioModeReference = typeNamed "IOMode"
optionReference = typeNamed "Optional"
errorReference = typeNamed "IOError"
errorTypeReference = typeNamed "IOErrorType"
seekModeReference = typeNamed "SeekMode"
threadIdReference = typeNamed "ThreadId"
socketReference = typeNamed "Socket"
handleReference = typeNamed "Handle"
epochTimeReference = typeNamed "EpochTime"
isTestReference = typeNamed "IsTest"

isTest :: (R.Reference, R.Reference)
isTest = (isTestReference, termNamed "links.isTest")

eitherLeftId, eitherRightId, someId, noneId, ioErrorId, handleId, socketId, threadIdId, epochTimeId
  :: DD.ConstructorId
eitherLeftId = constructorNamed eitherReference "Either.Left"
eitherRightId = constructorNamed eitherReference "Either.Right"
someId = constructorNamed optionReference "Optional.Some"
noneId = constructorNamed optionReference "Optional.None"
ioErrorId = constructorNamed errorReference "IOError.IOError"
handleId = constructorNamed handleReference "Handle.Handle"
socketId = constructorNamed socketReference "Socket.Socket"
threadIdId = constructorNamed threadIdReference "ThreadId.ThreadId"
epochTimeId = constructorNamed epochTimeReference "EpochTime.EpochTime"

mkErrorType :: Text -> DD.ConstructorId
mkErrorType = constructorNamed errorTypeReference

alreadyExistsId, noSuchThingId, resourceBusyId, resourceExhaustedId, eofId, illegalOperationId, permissionDeniedId, userErrorId
  :: DD.ConstructorId
alreadyExistsId = mkErrorType "IOErrorType.AlreadyExists"
noSuchThingId = mkErrorType "IOErrorType.NoSuchThing"
resourceBusyId = mkErrorType "IOErrorType.ResourceBusy"
resourceExhaustedId = mkErrorType "IOErrorType.ResourceExhausted"
eofId = mkErrorType "IOErrorType.EOF"
illegalOperationId = mkErrorType "IOErrorType.IllegalOperation"
permissionDeniedId = mkErrorType "IOErrorType.PermissionDenied"
userErrorId = mkErrorType "IOErrorType.UserError"

constructorNamed :: R.Reference -> Text -> DD.ConstructorId
constructorNamed ref name =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error
        $  "There's a bug in the Unison runtime. Couldn't find type "
        <> show ref
    Just decl ->
      case elemIndex name . DD.constructorNames $ TL.asDataDecl decl of
        Nothing ->
          error
            $  "Unison runtime bug. The type "
            <> show ref
            <> " has no constructor named "
            <> show name
        Just c -> c

constructorName :: R.Reference -> DD.ConstructorId -> Text
constructorName ref cid =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error
        $  "There's a bug in the Unison runtime. Couldn't find type "
        <> show ref
    Just decl -> genericIndex (DD.constructorNames $ TL.asDataDecl decl) cid

-- .. todo - fill in the rest of these

source :: Text
source = fromString [r|

type Either a b = Left a | Right b

type Optional a = None | Some a

-- This is linked to definitions that are considered tests
unique[e6dca08b40458b03ca1660cfbdaecaa7279b42d18257898b5fd1c34596aac36f] type
  IsTest = IsTest

links.isTest = IsTest.IsTest

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
  stdin : Handle
  stdin = Handle "stdin"

  stdout : Handle
  stdout = Handle "stdout"

  stderr : Handle
  stderr = Handle "stderr"

  -- Throw an I/O error on the left as an effect in `IO`
  rethrow : (Either IOError a) -> {IO} a
  rethrow x = case x of
    Either.Left e -> IO.throw e
    Either.Right a -> a

  -- Print a line to the standard output
  printLine : Text ->{IO} ()
  printLine t =
    IO.putText stdout t
    IO.putText stdout "\n"

  -- Read a line from the standard input
  readLine : '{IO} Text
  readLine = '(IO.getLine stdin)

  -- Open a named file in the given mode, yielding an open file handle
  openFile : FilePath -> IOMode ->{IO} Handle
  openFile f m = rethrow (IO.openFile_ f m)

  -- Close an open file handle
  closeFile : Handle ->{IO} ()
  closeFile f = rethrow (IO.closeFile_ f)

  -- Check whether a file handle has reached the end of the file
  isFileEOF : Handle ->{IO} Boolean
  isFileEOF h = rethrow (IO.isFileEOF_ h)

  -- Check whether a file handle is open
  isFileOpen : Handle ->{IO} Boolean
  isFileOpen h = rethrow (IO.isFileOpen_ h)

  -- Get a line of text from a text file handle
  getLine : Handle ->{IO} Text
  getLine h = rethrow (IO.getLine_ h)

  -- Get the entire contents of a file as a single block of text
  getText : Handle ->{IO} Text
  getText h = rethrow (IO.getText_ h)

  -- Write some text to a file
  putText : Handle -> Text ->{IO} ()
  putText h t = rethrow (IO.putText_ h t)

  -- Get epoch system time
  systemTime : '{IO} EpochTime
  systemTime = '(rethrow (IO.systemTime_))

  -- Run the given computation, and if it throws an error
  -- handle the error with the given handler.
  -- catch : '{IO} a -> (IOError ->{IO} a) ->{IO} a
  -- catch c h =
  --   k io = case io of
  --            { IO.throw e } -> h e
  --            x -> x
  --   handle k in c


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

type IOError = IOError IOErrorType Text

type SeekMode = Absolute | Relative | FromEnd

-- If the buffer size is not specified,
-- use an implementation-specific size.
type BufferMode = Line | Block (Optional Nat)

type EpochTime = EpochTime Nat

-- Either a host name e.g., "unisonweb.org" or a numeric host address
-- string consisting of a dotted decimal IPv4 address
-- e.g., "192.168.0.1".
type HostName = HostName Text

-- For example a port number like "8080"
type ServiceName = ServiceName Text

-- Thread IDs are strings for now. Need nominal/opaque types.
type ThreadId = ThreadId Text

ability IO where

  -- Basic file IO
  openFile_ : FilePath -> IOMode ->{IO} (Either IOError Handle)
  closeFile_ : Handle ->{IO} (Either IOError ())
  isFileEOF_ : Handle ->{IO} (Either IOError Boolean)
  isFileOpen_ : Handle ->{IO} (Either IOError Boolean)

  -- Text input and output

  --getChar : Handle ->{IO} Char
  getLine_ : Handle ->{IO} (Either IOError Text)
  -- Get the entire contents of the file as text
  getText_ : Handle ->{IO} (Either IOError Text)
  -- putChar : Handle -> Char ->{IO} ()
  putText_ : Handle -> Text ->{IO} (Either IOError ())

  -- Throw an error as an `IO` effect
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

  systemTime_ : {IO} (Either IOError EpochTime)

  -- File system operations
  getTempDirectory : {IO} (Either IOError FilePath)
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

  -- Simple TCP Networking

  -- Create a socket bound to the given local address.
  -- If a hostname is not given, this will use any available host.
  serverSocket : Optional HostName ->
                 ServiceName -> {IO} (Either IOError Socket)
  -- Start listening for connections
  listen : Socket ->{IO} (Either IOError ())

  -- Create a socket connected to the given remote address
  clientSocket : HostName ->
                 ServiceName ->{IO} (Either IOError Socket)

  closeSocket : Socket ->{IO} (Either IOError ())

  --socketToHandle : Socket -> IOMode ->{IO} (Either IOError Handle)
  --handleToSocket : Handle ->{IO} (Either IOError Socket)

  -- Accept a connection on a socket.
  -- Returns a socket that can send and receive data on a new connection
  accept : Socket ->{IO} (Either IOError Socket)

  -- Send some bytes to a socket.
  send : Socket -> Bytes ->{IO} (Either IOError ())

  -- Read the spefified number of bytes from the socket.
  receive : Socket -> Int ->{IO} (Either IOError (Optional Bytes))

  -- scatter/gather mode network I/O
  -- sendMany : Socket -> [Bytes] ->{IO} Int

  -- Threading --

  -- Fork a thread
  fork : '{IO} a ->{IO} (Either IOError ThreadId)

  -- Kill a running thread
  kill : ThreadId ->{IO} (Either IOError ())

  -- Suspend the current thread for a number of microseconds.
  delay : Nat ->{IO} (Either IOError ())

  -- Safely acquire and release a resource
  bracket : '{IO} a -> (a ->{IO} b) -> (a ->{IO} c) ->{IO} (Either IOError c)

|]
