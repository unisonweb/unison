
module Unison.Codebase.Editor.HandleInput.TermResolution
  ( lookupTermRefs
  , lookupTermRefWithType
  , resolveCon
  , resolveTermRef
  , resolveMainRef
  ) where

import Control.Lens ((<&>))
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.Maybe (catMaybes, fromJust)
import Data.Set (toList, fromList)

import qualified Unison.HashQualified as HQ
import Unison.ConstructorReference
import Unison.Name (Name)
import Unison.Names (Names)
import Unison.NamesWithHistory
  (NamesWithHistory(..), lookupHQTerm)
import Unison.Reference (Reference)
import Unison.Referent (Referent, pattern Ref, pattern Con)
import Unison.Codebase.Path (hqSplitFromName')
import Unison.Cli.Monad (Cli)
import Unison.Cli.NamesUtils (basicParseNames,basicPrettyPrintNamesA)
import Unison.Parser.Ann (Ann)
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv.Names (fromSuffixNames)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.Output (Output(..))
import qualified Unison.Codebase.Runtime as Runtime
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Cli.Monad as Cli
import Unison.Symbol (Symbol)
import Unison.Type (Type)

addHistory names = NamesWithHistory names mempty

lookupTerm :: HQ.HashQualified Name -> Names -> [Referent]
lookupTerm hq parseNames = toList (lookupHQTerm hq hnames)
  where
    hnames = addHistory parseNames

lookupCon
  :: HQ.HashQualified Name
  -> Names
  -> ([ConstructorReference], [Referent])
lookupCon hq parseNames =
  unzip . catMaybes . fmap extract $ lookupTerm hq parseNames
  where
    extract rt@(Con rf _) = Just (rf, rt)
    extract _ = Nothing

lookupTermRefs
  :: HQ.HashQualified Name -> Names -> ([Reference], [Referent])
lookupTermRefs hq parseNames =
  unzip . catMaybes . fmap extract $ lookupTerm hq parseNames
  where
    extract rt@(Ref rf) = Just (rf, rt)
    extract _ = Nothing

lookupTermRefWithType
  :: Codebase.Codebase IO Symbol Ann
  -> HQ.HashQualified Name
  -> Cli [(Reference,Type Symbol Ann)]
lookupTermRefWithType codebase name = do
  nms <- basicParseNames
  fmap catMaybes . traverse annot . fst $ lookupTermRefs name nms
  where
    annot tm =
      fmap ((,) tm) <$> liftIO (Codebase.getTypeOfTerm codebase tm)

resolveTerm :: HQ.HashQualified Name -> Cli Referent
resolveTerm name = basicParseNames >>= \nms ->
  case lookupTerm name nms of
    [] -> Cli.returnEarly (TermNotFound $ fromJust parsed)
      where parsed = hqSplitFromName' =<< HQ.toName name
    [rf] -> pure rf
    rfs -> Cli.returnEarly (TermAmbiguous name (fromList rfs))

resolveCon :: HQ.HashQualified Name -> Cli ConstructorReference
resolveCon name = basicParseNames >>= \nms ->
  case lookupCon name nms of
    ([], _) -> Cli.returnEarly (TermNotFound $ fromJust parsed)
      where parsed = hqSplitFromName' =<< HQ.toName name
    ([co], _) -> pure co
    (_, rfts) -> Cli.returnEarly (TermAmbiguous name (fromList rfts))

resolveTermRef :: HQ.HashQualified Name -> Cli Reference
resolveTermRef name = basicParseNames >>= \nms ->
  case lookupTermRefs name nms of
    ([], _) -> Cli.returnEarly (TermNotFound $ fromJust parsed)
      where parsed = hqSplitFromName' =<< HQ.toName name
    ([rf], _) -> pure rf
    (_, rfts) -> Cli.returnEarly (TermAmbiguous name (fromList rfts))

resolveMainRef :: HQ.HashQualified Name -> Cli (Reference, PrettyPrintEnv)
resolveMainRef main = do
  Cli.Env {codebase, runtime} <- ask
  let mainType = Runtime.mainType runtime
      smain = HQ.toString main
  parseNames <- basicPrettyPrintNamesA
  k <- liftIO (Codebase.hashLength codebase)
  let ppe = fromSuffixNames k (addHistory parseNames)
  lookupTermRefWithType codebase main >>= \case
    [(rf, ty)]
      | Typechecker.fitsScheme ty mainType -> pure (rf, ppe)
      | otherwise -> Cli.returnEarly (BadMainFunction smain ty ppe [mainType])
    _ -> Cli.returnEarly (NoMainFunction smain ppe [mainType])

