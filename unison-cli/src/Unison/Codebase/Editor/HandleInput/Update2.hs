module Unison.Codebase.Editor.HandleInput.Update2
  ( handleUpdate2,
  )
where

import Control.Lens ((^.))
import Data.Set qualified as Set
import U.Codebase.Reference (Reference, ReferenceType)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as NamesUtils
import Unison.Codebase qualified as Codebase
import Unison.CommandLine.OutputMessages qualified as Output
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Test.Common qualified as Typechecker
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set

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

  bigUf <- buildBigUnisonFile tuf dependents names

  -- - typecheck it
  typecheckBigUf bigUf pped >>= \case
    Left bigUfText -> prependTextToScratchFile bigUfText
    Right tuf -> saveTuf tuf

-- travis
prependTextToScratchFile :: Text -> Cli a0
prependTextToScratchFile bigUfText = wundefined

typecheckBigUf :: UnisonFile v a -> PrettyPrintEnvDecl -> Identity (Either Text (TypecheckedUnisonFile v a))
typecheckBigUf bigUf pped = do
  let prettyUf = Output.prettyUnisonFile pped bigUf
  error "parseAndSynthesizeAsFile" [] "update" (P.toPlain 80 prettyUf)

  wundefined

-- save definitions and namespace
saveTuf :: TypecheckedUnisonFile v a -> Cli a0
saveTuf = wundefined

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

getExistingReferences :: Defns (Set Name) (Set Name) -> Cli (Set Reference)
getExistingReferences = wundefined

getTermAndDeclNames :: TypecheckedUnisonFile v a -> Defns (Set Name) (Set Name)
getTermAndDeclNames = wundefined

-- namespace:
-- type Foo = Bar Nat
-- baz = 4
-- qux = baz + 1

-- unison file:
-- Foo.Bar = 3
-- baz = 5
