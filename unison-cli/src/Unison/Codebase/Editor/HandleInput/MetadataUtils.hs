-- | Helpers/utils that have to do with term/type metadata.
module Unison.Codebase.Editor.HandleInput.MetadataUtils
  ( addDefaultMetadata,
    manageLinks,
  )
where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader (ask)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.UnisonConfigUtils (defaultMetadataKey)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.NamespaceDiffUtils (diffHelper)
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.Output.BranchDiff qualified as OBranchDiff
import Unison.Codebase.Editor.SlurpComponent (SlurpComponent (..))
import Unison.Codebase.Editor.SlurpComponent qualified as SC
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Path qualified as Path
import Unison.CommandLine.InputPatterns qualified as InputPatterns
import Unison.HashQualified qualified as HQ
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.Name (Name)
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Server.Backend qualified as Backend
import Unison.Syntax.Name qualified as Name (unsafeFromVar)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Relation qualified as R
import Unison.Util.Set qualified as Set

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
    (Ord r) =>
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

-- | Resolve a metadata name to its type/value, or fail if it's missing or ambiguous.
resolveMetadata :: HQ.HashQualified Name -> Cli (Either Output (Metadata.Type, Metadata.Value))
resolveMetadata name = do
  Cli.Env {codebase} <- ask
  root' <- Cli.getRootBranch
  currentPath' <- Cli.getCurrentPath
  schLength <- Cli.runTransaction Codebase.branchHashLength

  let ppe :: PPE.PrettyPrintEnv
      ppe =
        Backend.basicSuffixifiedNames schLength root' (Path.unabsolute currentPath')

  terms <- getHQTerms name
  runExceptT $ do
    ref <-
      case Set.asSingleton terms of
        Just (Referent.Ref ref) -> pure ref
        -- FIXME: we want a different error message if the given name is associated with a data constructor (`Con`).
        _ -> throwError (MetadataAmbiguous name ppe (Set.toList terms))
    lift (Cli.runTransaction ((Codebase.getTypeOfTerm codebase ref))) >>= \case
      Just ty -> pure (Hashing.typeToReference ty, ref)
      Nothing -> throwError (MetadataMissingType ppe (Referent.Ref ref))

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
      Cli.runTransaction (Backend.termReferentsByShortHash codebase sh)
