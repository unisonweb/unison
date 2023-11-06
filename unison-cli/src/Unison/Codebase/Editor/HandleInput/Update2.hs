{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,
  )
where

import Control.Lens (over, (^.))
import Control.Lens qualified as Lens
import Control.Monad.RWS (ask)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra ((|>))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec (MonadParsec (eof))
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
import Unison.Codebase.Branch.Type (Branch0)
import Unison.Codebase.Editor.Output (Output (ParseErrors))
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Type (Codebase)
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.DataDeclaration (DataDeclaration, Decl)
import Unison.DataDeclaration qualified as Decl
import Unison.FileParsers qualified as FileParsers
import Unison.Hash (Hash)
import Unison.Name (Name)
import Unison.Name.Forward (ForwardName (..))
import Unison.Name.Forward qualified as ForwardName
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann (Ann))
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Result qualified as Result
import Unison.Server.Backend qualified as Backend
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.Parser qualified as Parser
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.Util.ManyToOne (ManyToOne (ManyToOne))
import Unison.Util.ManyToOne qualified as ManyToOne
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
  Cli.Env {codebase} <- ask
  -- - confirm all aliases updated together?
  tuf <- Cli.expectLatestTypecheckedFile

  -- - get add/updates from TUF
  let termAndDeclNames :: Defns (Set Name) (Set Name) = getTermAndDeclNames tuf
  -- - construct new UF with dependents
  names :: Names <- NamesUtils.getBasicPrettyPrintNames

  (pped, bigUf) <- Cli.runTransactionWithRollback \_abort -> do
    dependents <- Ops.dependentsWithinScope (namespaceReferences names) (getExistingReferencesNamed termAndDeclNames names)
    -- - construct PPE for printing UF* for typechecking (whatever data structure we decide to print)
    pped <- Codebase.hashLength <&> (`PPE.fromNamesDecl` (NamesWithHistory.fromCurrentNames names))
    bigUf <- buildBigUnisonFile codebase tuf dependents names
    pure (pped, bigUf)

  -- - typecheck it
  prettyParseTypecheck bigUf pped >>= \case
    Left bigUfText -> prependTextToScratchFile bigUfText >> pure pure
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
saveTuf :: TypecheckedUnisonFile Symbol Ann -> Cli (Branch0 m -> Cli (Branch0 m))
saveTuf tuf = do
  Cli.Env {codebase} <- ask
  Cli.runTransaction $ Codebase.addDefsToCodebase codebase tuf
  -- need to add the tuf contents to the current namespace
  -- types and term refs just overwrite; ctors of replaced decls go away
  --
  wundefined "todo: build and cons namespace"
  where
    -- \| for each decl in the tuf, delete the constructors of whatever decl currently has that name in the branch.
    deleteReplacedDeclCtors tuf branch0 = wundefined
    addNewDecls tuf branch0 = wundefined
    addNewTerms tuf branch0 = wundefined

-- | get references from `names` that have the same names as in `defns`
-- For constructors, we get the type reference.
getExistingReferencesNamed :: Defns (Set Name) (Set Name) -> Names -> Set Reference
getExistingReferencesNamed defns names = fromTerms <> fromTypes
  where
    fromTerms = foldMap (\n -> Set.map Referent.toReference $ Relation.lookupDom n $ Names.terms names) (defns ^. #terms)
    fromTypes = foldMap (\n -> Relation.lookupDom n $ Names.types names) (defns ^. #types)

-- mitchell
buildBigUnisonFile :: Codebase IO Symbol Ann -> TypecheckedUnisonFile Symbol Ann -> Map Reference.Id ReferenceType -> Names -> Transaction (UnisonFile Symbol Ann)
buildBigUnisonFile c tuf dependents names =
  -- for each dependent, add its definition with all its names to the UnisonFile
  foldM addComponent (UF.discardTypes tuf) (Map.toList dependents')
  where
    dependents' :: Map Hash ReferenceType = Map.mapKeys (\(Reference.Id h _pos) -> h) dependents
    addComponent :: UnisonFile Symbol Ann -> (Hash, ReferenceType) -> Transaction (UnisonFile Symbol Ann)
    addComponent uf (h, rt) = case rt of
      Reference.RtTerm -> addTermComponent h uf
      Reference.RtType -> addDeclComponent h uf
    ctorsForward :: Map ForwardName (Referent, Name)
    ctorsForward = Map.fromList $ [(ForwardName.fromName name, (r, name)) | (name, r@Referent.Con {}) <- Relation.toList names.terms]
    addTermComponent :: Hash -> UnisonFile Symbol Ann -> Transaction (UnisonFile Symbol Ann)
    addTermComponent h uf = do
      termComponent <- Codebase.unsafeGetTermComponent c h
      pure $ foldl' addTermElement uf (zip termComponent [0 ..])
      where
        addTermElement :: UnisonFile Symbol Ann -> ((Term Symbol Ann, Type Symbol Ann), Reference.Pos) -> UnisonFile Symbol Ann
        addTermElement uf ((tm, tp), i) = do
          let r :: Referent = Referent.Ref $ Reference.Derived h i
              termNames = Relation.lookupRan r (names.terms)
          foldl' (addDefinition tm) uf termNames
        addDefinition :: Term Symbol Ann -> UnisonFile Symbol Ann -> Name -> UnisonFile Symbol Ann
        addDefinition tm uf name =
          if nameExists uf name
            then uf
            else uf {UF.terms = (Name.toVar name, Ann.External, tm) : uf.terms}
        nameExists uf name = any (\(v, _, _) -> v == Name.toVar name) uf.terms

    -- given a dependent hash, include that component in the scratch file
    -- todo: wundefined: skip any definitions that already have names
    addDeclComponent :: Hash -> UnisonFile Symbol Ann -> Transaction (UnisonFile Symbol Ann)
    addDeclComponent h uf = do
      declComponent <- fromJust <$> Codebase.getDeclComponent h
      pure $ foldl' addDeclElement uf (zip declComponent [0 ..])
      where
        -- for each name a decl has, update its constructor names according to what exists in the namespace
        addDeclElement :: UnisonFile Symbol Ann -> (Decl Symbol Ann, Reference.Pos) -> UnisonFile Symbol Ann
        addDeclElement uf (decl, i) = do
          let declNames = Relation.lookupRan (Reference.Derived h i) (names.types)
          -- look up names for this decl's constructor based on the decl's name, and embed them in the decl definition.
          foldl' (addRebuiltDefinition decl) uf declNames
          where
            addRebuiltDefinition decl uf name = case decl of
              Left ed -> uf {UF.effectDeclarationsId = Map.insert (Name.toVar name) (Reference.Id h i, Decl.EffectDeclaration $ overwriteConstructorNames names name ed.toDataDecl) uf.effectDeclarationsId}
              Right dd -> uf {UF.dataDeclarationsId = Map.insert (Name.toVar name) (Reference.Id h i, overwriteConstructorNames names name dd) uf.dataDeclarationsId}
        overwriteConstructorNames :: Names -> Name -> DataDeclaration Symbol Ann -> DataDeclaration Symbol Ann
        overwriteConstructorNames names name dd =
          let constructorNames :: [Symbol] = Name.toVar <$> findCtorNames (Decl.constructorCount dd) name
              swapConstructorNames oldCtors =
                let (annotations, _vars, types) = unzip3 oldCtors
                 in zip3 annotations constructorNames types
           in over Decl.constructors_ swapConstructorNames dd
        -- \| given a decl name, find names for all of its constructors, in order.
        findCtorNames :: Int -> Name -> [Name]
        findCtorNames expectCount n = wundefined

-- let f = ForwardName.fromName n
--     (_, rest1) = Map.split f ctorsForward
--     (rest2, _) = Map.split (incrementLastSegmentChar f) rest1
--     -- go through `rest`, looking for constructor references that match `h` above, and adding the
--     -- name to the constructorid map if there's no mapping yet or if the name has fewer segments than
--     -- existing mapping
--     insertShortest :: Map ConstructorId Name -> (ForwardName, (Referent, Name)) -> Map Int Name
--     insertShortest m (fname, (Referent.ConId _ _ constructorId?, name)) = wundefined
--     m = foldl' insertShortest mempty (Map.toList rest2)
--  in wundefined

-- >>> incrementLastSegmentChar $ ForwardName.fromName $ Name.unsafeFromText "foo.bar.bam"
-- ForwardName {toList = "foo" :| ["bar","ban"]}
incrementLastSegmentChar :: ForwardName -> ForwardName
incrementLastSegmentChar (ForwardName segments) =
  let (initSegments, lastSegment) = (NonEmpty.init segments, NonEmpty.last segments)
      incrementedLastSegment = incrementLastCharInSegment lastSegment
   in ForwardName $ maybe (NonEmpty.singleton incrementedLastSegment) (|> incrementedLastSegment) (NonEmpty.nonEmpty initSegments)
  where
    incrementLastCharInSegment :: NameSegment -> NameSegment
    incrementLastCharInSegment (NameSegment text) =
      let incrementedText =
            if Text.null text
              then text
              else Text.init text `Text.append` Text.singleton (succ $ Text.last text)
       in NameSegment incrementedText

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
