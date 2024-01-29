module Unison.Codebase.Editor.HandleInput.DebugDefinition
  ( debugTerm,
    debugDecl,
  )
where

import Control.Monad.Reader
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output (Output (..))
import Unison.ConstructorReference (GConstructorReference (ConstructorReference))
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent

debugTermReference :: Bool -> TermReference -> Cli ()
debugTermReference verbose ref = do
  Cli.Env {codebase} <- ask
  case ref of
    Reference.DerivedId refId -> do
      Cli.runTransaction (Codebase.getTerm codebase refId) >>= \case
        Nothing -> Cli.respond $ TermNotFound' (Reference.toShortHash ref)
        Just term -> do
          Cli.respond $ DebugTerm verbose (Right term)
    Reference.Builtin builtinTxt -> do
      Cli.respond $ DebugTerm verbose (Left builtinTxt)

debugTypeReference :: Bool -> TypeReference -> Maybe ConstructorId -> Cli ()
debugTypeReference verbose ref mayConId = do
  Cli.Env {codebase} <- ask
  case ref of
    Reference.DerivedId refId -> do
      Cli.runTransaction (Codebase.getTypeDeclaration codebase refId) >>= \case
        Nothing -> Cli.respond $ TypeNotFound' (Reference.toShortHash ref)
        Just decl -> do
          Cli.respond $ DebugDecl verbose (Right decl) mayConId
    Reference.Builtin builtinTxt -> do
      Cli.respond $ DebugDecl verbose (Left builtinTxt) mayConId

debugTerm :: Bool -> HQ.HashQualified Name -> Cli ()
debugTerm verbose hqName = do
  names <- Cli.currentNames
  let matches = Names.lookupHQTerm Names.IncludeSuffixes hqName names
  for_ matches \case
    Referent.Ref termReference -> debugTermReference verbose termReference
    Referent.Con (ConstructorReference typeRef conId) _conTyp -> debugTypeReference verbose typeRef (Just conId)

debugDecl :: Bool -> HQ.HashQualified Name -> Cli ()
debugDecl verbose hqName = do
  names <- Cli.currentNames
  let matches = Names.lookupHQType Names.IncludeSuffixes hqName names
  for_ matches \typeRef -> debugTypeReference verbose typeRef Nothing
