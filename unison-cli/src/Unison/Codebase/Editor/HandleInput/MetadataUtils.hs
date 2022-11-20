-- | Helpers/utils that have to do with term/type metadata.
module Unison.Codebase.Editor.HandleInput.MetadataUtils
  ( addDefaultMetadata,
    manageLinks,
  )
where

import Control.Lens
import Control.Monad.Reader (ask)
import qualified Data.Set as Set
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli
import Unison.Cli.UnisonConfigUtils (defaultMetadataKey)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.Output
import qualified Unison.Codebase.Editor.Output.BranchDiff as OBranchDiff
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import qualified Unison.Codebase.Editor.SlurpComponent as SC
import qualified Unison.Codebase.Metadata as Metadata
import qualified Unison.Codebase.Path as Path
import qualified Unison.CommandLine.InputPatterns as InputPatterns
import qualified Unison.HashQualified as HQ
import qualified Unison.Hashing.V2.Convert as Hashing
import Unison.Name (Name)
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Server.Backend as Backend
import qualified Unison.Syntax.Name as Name (unsafeFromVar)
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Set as Set

-- Add default metadata to all added types and terms in a slurp component.
--
-- No-op if the slurp component is empty.
addDefaultMetadata :: SlurpComponent -> Cli ()
addDefaultMetadata adds =
  when (not (SC.isEmpty adds)) do
    Cli.time "add-default-metadata" do
      currentPath' <- Cli.getCurrentPath

      let addedVs = Set.toList $ SC.types adds <> SC.terms adds
          addedNs = traverse (Path.hqSplitFromName' . Name.unsafeFromVar) addedVs
      case addedNs of
        Nothing ->
          error $
            "I couldn't parse a name I just added to the codebase! "
              <> "-- Added names: "
              <> show addedVs
        Just addedNames ->
          resolveDefaultMetadata currentPath' >>= \case
            [] -> pure ()
            dm -> do
              traverse InputPatterns.parseHashQualifiedName dm & \case
                Left err -> do
                  Cli.respond $
                    ConfiguredMetadataParseError
                      (Path.absoluteToPath' currentPath')
                      (show dm)
                      err
                Right defaultMeta -> do
                  manageLinks True addedNames defaultMeta Metadata.insert

-- | Add/remove links between definitions and metadata.
-- `silent` controls whether this produces any output to the user.
-- `srcs` is (names of the) definitions to pass to `op`
-- `mdValues` is (names of the) metadata to pass to `op`
-- `op` is the operation to add/remove/alter metadata mappings.
--   e.g. `Metadata.insert` is passed to add metadata links.
manageLinks ::
  Bool ->
  [Path.HQSplit'] ->
  [HQ.HashQualified Name] ->
  ( forall r.
    Ord r =>
    (r, Metadata.Type, Metadata.Value) ->
    Branch.Star r NameSegment ->
    Branch.Star r NameSegment
  ) ->
  Cli ()
manageLinks silent srcs' metadataNames op = do
  metadatas <- traverse resolveMetadata metadataNames
  before <- Cli.getRootBranch0
  srcs <- traverse Cli.resolveSplit' srcs'
  srcle <- Monoid.foldMapM Cli.getTermsAt srcs
  srclt <- Monoid.foldMapM Cli.getTypesAt srcs
  for_ metadatas \case
    Left errOutput -> Cli.respond errOutput
    Right (mdType, mdValue) -> do
      let step =
            let tmUpdates terms = foldl' go terms srcle
                  where
                    go terms src = op (src, mdType, mdValue) terms
                tyUpdates types = foldl' go types srclt
                  where
                    go types src = op (src, mdType, mdValue) types
             in over Branch.terms tmUpdates . over Branch.types tyUpdates
      let steps = map (\(path, _hq) -> (Path.unabsolute path, step)) srcs
      Cli.stepManyAtNoSync steps
  if silent
    then Cli.respond DefaultMetadataNotification
    else do
      after <- Cli.getRootBranch0
      (ppe, diff) <- diffHelper before after
      if OBranchDiff.isEmpty diff
        then Cli.respond NoOp
        else
          Cli.respondNumbered $
            ShowDiffNamespace
              (Right Path.absoluteEmpty)
              (Right Path.absoluteEmpty)
              ppe
              diff

-- | Resolve a metadata name to its type/value, or return early if no such metadata is found.
resolveMetadata :: HQ.HashQualified Name -> Cli (Either Output (Metadata.Type, Metadata.Value))
resolveMetadata name = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath' <- Cli.getCurrentPath
  schLength <- Cli.runTransaction Codebase.branchHashLength

  let ppe :: PPE.PrettyPrintEnv
      ppe =
        Backend.basicSuffixifiedNames schLength root' (Backend.Within $ Path.unabsolute currentPath')

  terms <- getHQTerms name
  ref <-
    case Set.asSingleton terms of
      Just (Referent.Ref ref) -> pure ref
      -- FIXME: we want a different error message if the given name is associated with a data constructor (`Con`).
      _ -> Cli.returnEarly (MetadataAmbiguous name ppe (Set.toList terms))
  Cli.runTransaction ((Codebase.getTypeOfTerm codebase ref)) <&> \case
    Just ty -> Right (Hashing.typeToReference ty, ref)
    Nothing -> Left (MetadataMissingType ppe (Referent.Ref ref))

resolveDefaultMetadata :: Path.Absolute -> Cli [String]
resolveDefaultMetadata path = do
  let superpaths = Path.ancestors path
  xs <-
    for
      superpaths
      ( \path -> do
          mayNames <- Cli.getConfig @[String] (defaultMetadataKey path)
          pure . join $ toList mayNames
      )
  pure . join $ toList xs

-- | Get the set of terms related to a hash-qualified name.
getHQTerms :: HQ.HashQualified Name -> Cli (Set Referent)
getHQTerms = \case
  HQ.NameOnly n -> do
    root0 <- Cli.getRootBranch0
    currentPath' <- Cli.getCurrentPath
    -- absolute-ify the name, then lookup in deepTerms of root
    let path =
          n
            & Path.fromName'
            & Path.resolve currentPath'
            & Path.unabsolute
            & Path.unsafeToName
    pure $ R.lookupRan path (Branch.deepTerms root0)
  HQ.HashOnly sh -> hashOnly sh
  HQ.HashQualified _ sh -> hashOnly sh
  where
    hashOnly sh = do
      Cli.Env {codebase} <- ask
      liftIO (Backend.termReferentsByShortHash codebase sh)
