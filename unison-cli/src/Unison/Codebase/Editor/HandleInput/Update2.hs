module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,
  )
where

import Control.Lens ((^.))
import Control.Monad.RWS (ask)
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Reference (Reference, ReferenceType)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as NamesUtils
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output (Output (ParseErrors))
import Unison.Codebase.Path qualified as Path
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.DataDeclaration qualified as Decl
import Unison.FileParsers qualified as FileParsers
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Server.Backend qualified as Backend
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile.Type qualified as UF
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Var (Var)

data Defns terms types = Defns
  { terms :: !terms,
    types :: !types
  }
  deriving stock (Generic, Show)

-- deriving (Semigroup) via GenericSemigroupMonoid (Defns terms types)

handleUpdate2 :: Cli ()
handleUpdate2 = do
  -- - confirm all aliases updated together?
  tuf <- Cli.expectLatestTypecheckedFile

  -- - get add/updates from TUF
  let termAndDeclNames :: Defns (Set Name) (Set Name) = getTermAndDeclNames tuf
  -- - construct new UF with dependents
  names :: Names <- NamesUtils.getBasicPrettyPrintNames

  (dependents, pped) <- Cli.runTransactionWithRollback \_abort -> do
    dependents <- Ops.dependentsWithinScope (namespaceReferences names) (getExistingReferencesNamed termAndDeclNames names)
    -- - construct PPE for printing UF* for typechecking (whatever data structure we decide to print)
    pped <- Codebase.hashLength <&> (`PPE.fromNamesDecl` (NamesWithHistory.fromCurrentNames names))
    pure (dependents, pped)

  bigUf :: UnisonFile Symbol Ann <- buildBigUnisonFile tuf dependents names

  -- - typecheck it
  prettyParseTypecheck bigUf pped >>= \case
    Left bigUfText -> prependTextToScratchFile bigUfText
    Right tuf -> saveTuf tuf

-- travis
prependTextToScratchFile :: Text -> Cli ()
prependTextToScratchFile textUf = do
  liftIO $ putStrLn (Text.unpack textUf)

prettyParseTypecheck :: UnisonFile Symbol Ann -> PrettyPrintEnvDecl -> Cli (Either Text (TypecheckedUnisonFile Symbol Ann))
prettyParseTypecheck bigUf pped = do
  typecheck <- mkTypecheckFnCli
  let prettyUf = Output.prettyUnisonFile pped bigUf
  let stringUf = Pretty.toPlain 80 prettyUf
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) rootBranch
  Cli.Env {generateUniqueName} <- ask
  uniqueName <- liftIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid currentPath,
            names = parseNames
          }
  Cli.runTransaction do
    Parsers.parseFile "<update>" stringUf parsingEnv >>= \case
      Left {} -> pure $ Left (Text.pack stringUf)
      Right reparsedUf ->
        typecheck reparsedUf <&> \case
          Just reparsedTuf -> Right reparsedTuf
          Nothing -> Left (Text.pack stringUf)

mkTypecheckFnCli :: Cli (UnisonFile Symbol Ann -> Transaction (Maybe (TypecheckedUnisonFile Symbol Ann)))
mkTypecheckFnCli = do
  Cli.Env {codebase, generateUniqueName} <- ask
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  let parseNames = Backend.getCurrentParseNames (Backend.Within (Path.unabsolute currentPath)) rootBranch
  pure (mkTypecheckFn codebase generateUniqueName currentPath parseNames)

mkTypecheckFn ::
  Codebase.Codebase IO Symbol Ann ->
  IO Parser.UniqueName ->
  Path.Absolute ->
  NamesWithHistory.NamesWithHistory ->
  UnisonFile Symbol Ann ->
  Transaction (Maybe (TypecheckedUnisonFile Symbol Ann))
mkTypecheckFn codebase generateUniqueName currentPath parseNames unisonFile = do
  uniqueName <- Sqlite.unsafeIO generateUniqueName
  let parsingEnv =
        Parser.ParsingEnv
          { uniqueNames = uniqueName,
            uniqueTypeGuid = Cli.loadUniqueTypeGuid currentPath,
            names = parseNames
          }
  typecheckingEnv <-
    computeTypecheckingEnvironment (FileParsers.ShouldUseTndr'Yes parsingEnv) codebase [] unisonFile
  let Result.Result _notes maybeTypecheckedUnisonFile = FileParsers.synthesizeFile typecheckingEnv unisonFile
  pure maybeTypecheckedUnisonFile

-- save definitions and namespace
saveTuf :: TypecheckedUnisonFile v a -> Cli a0
saveTuf tuf = do
  wundefined "todo: save definitions"
  wundefined "todo: build and cons namespace"

-- | get references from `names` that have the same names as in `defns`
-- For constructors, we get the type reference.
getExistingReferencesNamed :: Defns (Set Name) (Set Name) -> Names -> Set Reference
getExistingReferencesNamed defns names = fromTerms <> fromTypes
  where
    fromTerms = foldMap (\n -> Set.map Referent.toReference $ Relation.lookupDom n $ Names.terms names) (defns ^. #terms)
    fromTypes = foldMap (\n -> Relation.lookupDom n $ Names.types names) (defns ^. #types)

-- mitchell
buildBigUnisonFile :: TypecheckedUnisonFile Symbol Ann -> Map Reference.Id Reference.ReferenceType -> Names -> Cli a0
buildBigUnisonFile = wundefined

namespaceReferences :: Names -> Set Reference.Id
namespaceReferences names = fromTerms <> fromTypes
  where
    fromTerms = Set.mapMaybe Referent.toReferenceId (Relation.ran $ Names.terms names)
    fromTypes = Set.mapMaybe Reference.toId (Relation.ran $ Names.types names)

getTermAndDeclNames :: Var v => TypecheckedUnisonFile v a -> Defns (Set Name) (Set Name)
getTermAndDeclNames tuf = Defns (terms <> effectCtors <> dataCtors) (effects <> datas)
  where
    terms = keysToNames $ UF.hashTermsId tuf
    effects = keysToNames $ UF.effectDeclarationsId' tuf
    datas = keysToNames $ UF.dataDeclarationsId' tuf
    effectCtors = foldMap ctorsToNames $ fmap (Decl.toDataDecl . snd) $ UF.effectDeclarationsId' tuf
    dataCtors = foldMap ctorsToNames $ fmap snd $ UF.dataDeclarationsId' tuf
    keysToNames = Set.map Name.unsafeFromVar . Map.keysSet
    ctorsToNames = Set.fromList . map Name.unsafeFromVar . Decl.constructorVars

-- namespace:
-- type Foo = Bar Nat
-- baz = 4
-- qux = baz + 1

-- unison file:
-- Foo.Bar = 3
-- baz = 5
