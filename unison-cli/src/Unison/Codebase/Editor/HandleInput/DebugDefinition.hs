module Unison.Codebase.Editor.HandleInput.DebugDefinition
  ( debugTerm,
    debugDecl,
    intermediateCode,
  )
where

import Control.Monad.Reader
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output (Output (..))
import Unison.Codebase.Execute qualified as Codebase (codebaseToCodeLookup)
import Unison.Codebase.Runtime qualified as Runtime
import Unison.ConstructorReference (GConstructorReference (ConstructorReference))
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (TermReference, TypeReference)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.Pretty qualified as P

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

debugTypeReference :: TypeReference -> Maybe ConstructorId -> Cli ()
debugTypeReference ref mayConId = do
  Cli.Env {codebase} <- ask
  case ref of
    Reference.DerivedId refId -> do
      Cli.runTransaction (Codebase.getTypeDeclaration codebase refId) >>= \case
        Nothing -> Cli.respond $ TypeNotFound' (Reference.toShortHash ref)
        Just decl -> do
          Cli.respond $ DebugDecl (Right decl) mayConId
    Reference.Builtin builtinTxt -> do
      Cli.respond $ DebugDecl (Left builtinTxt) mayConId

debugTerm :: Bool -> HQ.HashQualified Name -> Cli ()
debugTerm verbose hqName = do
  names <- Cli.currentNames
  let matches = Names.lookupHQTerm Names.IncludeSuffixes hqName names
  for_ matches \case
    Referent.Ref termReference -> debugTermReference verbose termReference
    Referent.Con (ConstructorReference typeRef conId) _conTyp -> debugTypeReference typeRef (Just conId)

debugDecl :: HQ.HashQualified Name -> Cli ()
debugDecl hqName = do
  names <- Cli.currentNames
  let matches = Names.lookupHQType Names.IncludeSuffixes hqName names
  for_ matches \typeRef -> debugTypeReference typeRef Nothing

-- | Show the intermediate code (ANF) of a term, the other terms of its cycle,
-- and the lambdas lifted out of them; optimized when the flag is set.
intermediateCode :: Bool -> HQ.HashQualified Name -> Cli ()
intermediateCode optimized hqName = do
  Cli.Env {codebase, runtime} <- ask
  names <- Cli.currentNames
  let ppe = (PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)).suffixifiedPPE
      codeLookup = () <$ Codebase.codebaseToCodeLookup codebase
      matches = toList (Names.lookupHQTerm Names.IncludeSuffixes hqName names)
      refs = [r | Referent.Ref r <- matches]
  when (null matches) $ Cli.returnEarly (SearchTermsNotFound [hqName])
  -- a constructor is code the runtime builds in, not a group of its own
  when (null refs) . Cli.returnEarly . Literal . P.wrap $
    P.text (HQ.toText hqName) <> " is a constructor, which has no intermediate code of its own."
  for_ refs \ref ->
    liftIO (Runtime.intermediateCode runtime codeLookup ppe optimized ref)
      >>= either (Cli.returnEarly . EvaluationFailure id) (Cli.respond . Literal)
