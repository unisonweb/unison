module Unison.Codebase.Editor.HandleInput.AliasType (handleAliasType) where

import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.BranchUtil qualified as BranchUtil
import Unison.Codebase.Editor.Output
import Unison.Codebase.Path (Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Prelude
import Unison.Referent qualified as Referent
import Unison.Server.Backend qualified as Backend
import Unison.ShortHash qualified as SH
import Unison.Util.Set qualified as Set

handleAliasType :: Bool -> Either SH.ShortHash (HQ'.HashQualified (Path.Split Path')) -> Path.Split Path' -> Cli ()
handleAliasType force src' dest' = do
  src <- traverse (traverse Cli.resolveSplit') src'
  srcTypes <-
    either
      (Cli.runTransaction . Backend.typeReferencesByShortHash)
      Cli.getTypesAt
      src
  srcType <-
    Set.asSingleton srcTypes & onNothing do
      Cli.returnEarly =<< case (Set.null srcTypes, src') of
        (True, Left hash) -> pure (TypeNotFound' hash)
        (True, Right name) -> pure (TypeNotFound name)
        (False, Left hash) -> pure (HashAmbiguous hash (Set.map Referent.Ref srcTypes))
        (False, Right name) -> do
          hqLength <- Cli.runTransaction Codebase.hashLength
          pure (DeleteNameAmbiguous hqLength name Set.empty srcTypes)
  dest <- Cli.resolveSplit' dest'
  destTypes <- Cli.getTypesAt $ HQ'.NameOnly dest
  when (not force && not (Set.null destTypes)) do
    Cli.returnEarly (TypeAlreadyExists dest' destTypes)
  description <- inputDescription force src' dest'
  Cli.stepAt description (BranchUtil.makeAddTypeName dest srcType)
  Cli.respond Success

inputDescription :: Bool -> HQ'.HashOrHQ (Path.Split Path') -> Path.Split Path' -> Cli Text
inputDescription force src0 dest0 = do
  src <- hhqs' src0
  dest <- ps' dest0
  pure ((if force then "debug.alias.type.force " else "alias.term ") <> src <> " " <> dest)

hhqs' :: HQ'.HashOrHQ (Path.Split Path') -> Cli Text
hhqs' = either (pure . SH.toText) hqs'

hqs' :: HQ'.HashQualified (Path.Split Path') -> Cli Text
hqs' = pure . HQ'.toTextWith (Path.toText . Path.unsplit)

ps' :: Path.Split Path' -> Cli Text
ps' = p' . Path.unsplit

p' :: Path' -> Cli Text
p' = fmap (into @Text) . Cli.resolvePath'
