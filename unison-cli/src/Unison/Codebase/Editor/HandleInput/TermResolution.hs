module Unison.Codebase.Editor.HandleInput.TermResolution
  ( lookupTermRefs,
    resolveCon,
    resolveTerm,
    resolveTermRef,
    resolveMainRef,
  )
where

import Control.Monad.Reader (ask)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Editor.Output (NumberedOutput (..), Output (..))
import Unison.Codebase.MainTerm qualified as MainTerm
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Runtime
import Unison.ConstructorReference
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Reference (Reference, TermReference)
import Unison.Referent (Referent, pattern Con, pattern Ref)
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Prelude hiding (unzip)

lookupTerm :: HQ.HashQualified Name -> Names -> [Referent]
lookupTerm hq parseNames = Set.toList (Names.lookupHQTerm Names.IncludeSuffixes hq parseNames)

lookupCon ::
  HQ.HashQualified Name ->
  Names ->
  ([ConstructorReference], [Referent])
lookupCon hq parseNames =
  unzip . catMaybes . fmap extract $ lookupTerm hq parseNames
  where
    extract rt@(Con rf _) = Just (rf, rt)
    extract _ = Nothing

lookupTermRefs ::
  HQ.HashQualified Name -> Names -> ([Reference], [Referent])
lookupTermRefs hq parseNames =
  unzip . catMaybes . fmap extract $ lookupTerm hq parseNames
  where
    extract rt@(Ref rf) = Just (rf, rt)
    extract _ = Nothing

resolveTerm :: HQ.HashQualified Name -> Cli Referent
resolveTerm name = do
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  case lookupTerm name names of
    [] -> Cli.returnEarly . either TermNotFound' (TermNotFound . fmap Path.parentOfName) $ HQ'.fromHQ name
    [rf] -> pure rf
    rfs -> Cli.returnEarly . TermAmbiguous suffixifiedPPE name $ Set.fromList rfs

resolveCon :: HQ.HashQualified Name -> Cli ConstructorReference
resolveCon name = do
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  case lookupCon name names of
    ([], _) -> Cli.returnEarly . either TermNotFound' (TermNotFound . fmap Path.parentOfName) $ HQ'.fromHQ name
    ([co], _) -> pure co
    (_, rfts) -> Cli.returnEarly . TermAmbiguous suffixifiedPPE name $ Set.fromList rfts

resolveTermRef :: HQ.HashQualified Name -> Cli TermReference
resolveTermRef name = do
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let suffixifiedPPE = PPED.suffixifiedPPE pped
  case lookupTermRefs name names of
    ([], _) -> Cli.returnEarly . either TermNotFound' (TermNotFound . fmap Path.parentOfName) $ HQ'.fromHQ name
    ([rf], _) -> pure rf
    (_, rfts) -> Cli.returnEarly . TermAmbiguous suffixifiedPPE name $ Set.fromList rfts

resolveMainRef :: Text -> HQ.HashQualified Name -> Cli (HQ.HashQualified Name, TermReference, Term Symbol Ann, Type Symbol Ann)
resolveMainRef what mainName = do
  Cli.Env {codebase, runtime} <- ask
  let mainType = Runtime.mainType runtime
  names <- Cli.currentNames
  let pped = PPED.makePPED (PPE.hqNamer 10 names) (PPE.suffixifyByHash names)
  let ppe = pped.suffixifiedPPE
  mainTermResult <-
    MainTerm.getMainTerm
      (liftIO . Codebase.runTransaction codebase . Codebase.getTypeOfTerm codebase)
      names
      mainName
      mainType
  case mainTermResult of
    MainTerm.Success mainName1 ref term ty -> pure (mainName1, ref, term, ty)
    MainTerm.NotFound -> Cli.returnEarly (NoMainFunction mainName ppe [mainType])
    MainTerm.BadType terms ->
      Cli.returnEarly $
        BadMainFunction
          what
          (map (\(s, _, t) -> (s, t)) terms)
          ppe
          [mainType]
    MainTerm.Ambiguous terms -> do
      Cli.respondNumbered $
        AmbiguousMainFunction
          what
          (map (\(s, _, t) -> (s, t)) terms)
          ppe
      Cli.returnEarlyWithoutOutput
