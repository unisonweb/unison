{-# LANGUAGE OverloadedStrings #-}

module Unison.Runtime.IOSources.IOSource where

import Control.Lens (view, _1)
import Control.Monad.Identity (runIdentity, Identity)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.List (elemIndex, genericIndex)
import Unison.Codebase.CodeLookup (CodeLookup(..))
import Unison.FileParsers (parseAndSynthesizeFile)
import Unison.Parser (Ann(..))
import Unison.Symbol (Symbol)
import Unison.UnisonFile
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
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
import qualified Unison.Runtime.IOSources.IO1 as IO1
import qualified Unison.Runtime.IOSources.IO2 as IO2

type SourceFile = TypecheckedUnisonFile Symbol Ann

constructorNamed :: Text -> R.Reference -> DD.ConstructorId
constructorNamed name ref =
  case runIdentity . getTypeDeclaration allCodeLookup $ R.unsafeId ref of
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
        $ TL.asDataDecl decl

constructorName :: R.Reference -> DD.ConstructorId -> Text
constructorName ref cid =
  case runIdentity . getTypeDeclaration allCodeLookup $ R.unsafeId ref of
    Nothing ->
      error
        $  "There's a bug in the Unison runtime. Couldn't find type "
        <> show ref
    Just decl -> genericIndex (DD.constructorNames $ TL.asDataDecl decl) cid

typecheckedFile :: Text -> UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile source = let
  tl :: a -> Identity (TL.TypeLookup Symbol Ann)
  tl = const $ pure (External <$ Builtin.typeLookup)
  env = Parser.ParsingEnv mempty (Names.Names Builtin.names0 mempty)
  r = parseAndSynthesizeFile [] tl env "<IO.u builtin>" source
  in case runIdentity $ Result.runResultT r of
    (Nothing, notes) -> error $ "parsing failed: " <> show notes
    (Just Left{}, notes) -> error $ "typechecking failed" <> show notes
    (Just (Right file), _) -> file

fileTerms :: SourceFile -> Map.Map Symbol R.Reference
fileTerms file = view _1 <$> UF.hashTerms file

termNamed :: String -> SourceFile -> R.Reference
termNamed s file = fromMaybe (error $ "No builtin term called: " <> s)
  $ Map.lookup (Var.nameds s) (fileTerms file)

codeLookupFromFile :: SourceFile -> CodeLookup Symbol Identity Ann
codeLookupFromFile = CL.fromUnisonFile . UF.discardTypes

allCodeLookup :: CodeLookup Symbol Identity Ann
allCodeLookup = foldMap codeLookupFromFile ioSources

ioSources :: Map R.Id SourceFile
ioSources =
  Map.fromList
    $   (\src -> let file = typecheckedFile src in (ioHash file, file))
    <$> [IO1.source, IO2.source]

typeNamed :: String -> SourceFile -> R.Reference
typeNamed s file =
  case Map.lookup (Var.nameds s) (UF.dataDeclarations' file) of
    Nothing     -> error $ "No builtin type called: " <> s
    Just (r, _) -> r

abilityNamed :: String -> SourceFile -> R.Reference
abilityNamed s file =
  case Map.lookup (Var.nameds s) (UF.effectDeclarations' file) of
    Nothing     -> error $ "No builtin ability called: " <> s
    Just (r, _) -> r

ioHashes :: Set R.Id
ioHashes = Map.keysSet ioSources

ioSourceFiles :: [SourceFile]
ioSourceFiles = Map.elems ioSources

ioModeHashes :: Set R.Id
ioModeHashes = Set.fromList $ ioModeHash <$> ioSourceFiles

eitherHash :: R.Id
eitherHash = R.unsafeId . eitherReference $ head ioSourceFiles

ioHash, ioModeHash :: SourceFile -> R.Id
ioHash = R.unsafeId <$> ioReference
ioModeHash = R.unsafeId <$> ioModeReference

mkErrorType :: Text -> SourceFile -> DD.ConstructorId
mkErrorType ctorName = constructorNamed ctorName <$> errorTypeReference

alreadyExistsId, noSuchThingId, resourceBusyId, resourceExhaustedId, eofId, illegalOperationId, permissionDeniedId, userErrorId
  :: SourceFile -> DD.ConstructorId
alreadyExistsId = mkErrorType "io.ErrorType.AlreadyExists"
noSuchThingId = mkErrorType "io.ErrorType.NoSuchThing"
resourceBusyId = mkErrorType "io.ErrorType.ResourceBusy"
resourceExhaustedId = mkErrorType "io.ErrorType.ResourceExhausted"
eofId = mkErrorType "io.ErrorType.EOF"
illegalOperationId = mkErrorType "io.ErrorType.IllegalOperation"
permissionDeniedId = mkErrorType "io.ErrorType.PermissionDenied"
userErrorId = mkErrorType "io.ErrorType.UserError"

ioReference, bufferModeReference, eitherReference, ioModeReference, optionReference, errorReference, errorTypeReference, seekModeReference, threadIdReference, socketReference, handleReference, epochTimeReference, isTestReference, filePathReference, docReference, linkReference
  :: SourceFile -> R.Reference
ioReference = abilityNamed "io.IO"
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
filePathReference = typeNamed "io.FilePath"
docReference = typeNamed "Doc"
linkReference = typeNamed "Link"

eitherLeftId, eitherRightId, someId, noneId, ioErrorId, handleId, socketId, threadIdId, epochTimeId, bufferModeLineId, bufferModeBlockId, filePathId
  :: SourceFile -> DD.ConstructorId
eitherLeftId = constructorNamed "Either.Left" <$> eitherReference
eitherRightId = constructorNamed "Either.Right" <$> eitherReference
someId = constructorNamed "Optional.Some" <$> optionReference
noneId = constructorNamed "Optional.None" <$> optionReference
ioErrorId = constructorNamed "io.Error.Error" <$> errorReference
handleId = constructorNamed "io.Handle.Handle" <$> handleReference
socketId = constructorNamed "io.Socket.Socket" <$> socketReference
threadIdId = constructorNamed "io.ThreadId.ThreadId" <$> threadIdReference
epochTimeId = constructorNamed "io.EpochTime.EpochTime" <$> epochTimeReference
bufferModeLineId =
  constructorNamed "io.BufferMode.Line" <$> bufferModeReference
bufferModeBlockId =
  constructorNamed "io.BufferMode.Block" <$> bufferModeReference
filePathId = constructorNamed "io.FilePath.FilePath" <$> filePathReference

isTest :: (R.Reference, R.Reference)
isTest =
  ((,) <$> isTestReference <*> termNamed "metadata.isTest") $ head ioSourceFiles

