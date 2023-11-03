module Unison.Codebase.Editor.HandleInput.Update
  ( handleUpdate2,
  )
where

import U.Codebase.Reference (Reference, ReferenceType)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.NamesUtils qualified as NamesUtils
import Unison.Codebase qualified as Codebase
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl.Names qualified as PPE
import Unison.Symbol (Symbol)
import Unison.UnisonFile.Type (TypecheckedUnisonFile, UnisonFile)

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

  dependents :: Map Reference.Id ReferenceType <- Ops.dependentsWithinScope <$> namespaceReferences names <*> getExistingReferencesNamed termAndDeclNames names
  bigUf <- buildBigUnisonFile tuf dependents names
  -- - construct PPE for printing UF* for typechecking (whatever data structure we decide to print)
  ppe <- Codebase.hashLength <&> (`PPE.fromNamesDecl` (NamesWithHistory.fromCurrentNames names))
  -- - typecheck it
  typecheckBigUf bigUf >>= \case
    Left bigUfText -> prependTextToScratchFile bigUfText
    Right tuf -> saveTuf tuf

-- travis
prependTextToScratchFile :: Text -> Cli a0
prependTextToScratchFile bigUfText = wundefined

typecheckBigUf :: UnisonFile v a -> Cli (Either Text (TypecheckedUnisonFile v a))
typecheckBigUf = wundefined

-- save definitions and namespace
saveTuf :: TypecheckedUnisonFile v a -> Cli a0
saveTuf = wundefined

-- arya
getExistingReferencesNamed :: Defns (Set Name) (Set Name) -> Names -> Cli (Set Reference)
getExistingReferencesNamed = wundefined

-- mitchell
buildBigUnisonFile :: TypecheckedUnisonFile Symbol Ann -> Map Reference.Id Reference.ReferenceType -> Names -> Cli a0
buildBigUnisonFile = wundefined

namespaceReferences :: Names -> Cli (Set Reference.Id)
namespaceReferences = wundefined

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
