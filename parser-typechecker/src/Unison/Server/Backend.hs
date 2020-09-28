{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Server.Backend where

import           Control.Monad.Except           ( ExceptT(..), throwError )
import           Data.List.Extra                ( sort )
import           Data.Bifunctor                 ( first )
import qualified Data.Map                      as Map

import           Unison.Prelude
import qualified Unison.DataDeclaration        as DD
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Codebase.Branch         ( Branch)
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.Path           ( Path )
import           Unison.HashQualified'         as HQ'
import           Unison.NameSegment             ( NameSegment )
import           Unison.Name                   as Name
import qualified Unison.Names2                 as Names
import qualified Unison.Names3                 as Names3
import           Unison.Names3                  ( Names(..)
                                                , Names0
                                                )
import           Unison.Parser                  ( Ann )
import qualified Unison.PrettyPrintEnv         as PPE
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import           Unison.Type                    ( Type )
import qualified Unison.Util.Relation          as R
import qualified Unison.Util.Star3             as Star3
import           Unison.Var                     ( Var )

data ShallowListEntry v a
  = ShallowTermEntry Referent HQ'.HQSegment (Maybe (Type v a))
  | ShallowTypeEntry Reference HQ'.HQSegment
  | ShallowBranchEntry NameSegment Int -- number of child definitions
  | ShallowPatchEntry NameSegment
  deriving (Eq, Ord, Show, Generic)

data BackendError
  = NoSuchNamespace Path.Absolute
  | BadRootBranch Codebase.GetRootBranchError

type Backend m a = ExceptT BackendError m a

-- implementation detail of basicParseNames0 and basicPrettyPrintNames0
basicNames0' :: Branch m -> Path -> (Names0, Names0)
basicNames0' root path = (parseNames00, prettyPrintNames00)
 where
  root0                    = Branch.head root
  currentBranch            = fromMaybe Branch.empty $ Branch.getAt path root
  absoluteRootNames0       = Names3.makeAbsolute0 (Branch.toNames0 root0)
  currentBranch0           = Branch.head currentBranch
  currentPathNames0        = Branch.toNames0 currentBranch0
  -- all names, but with local names in their relative form only, rather
  -- than absolute; external names appear as absolute
  currentAndExternalNames0 = currentPathNames0
    `Names3.unionLeft0` absDot externalNames   where
    absDot        = Names.prefix0 (Name.unsafeFromText "")
    externalNames = rootNames `Names.difference` pathPrefixed currentPathNames0
    rootNames     = Branch.toNames0 root0
    pathPrefixed  = case path of
      Path.Path (toList -> []) -> id
      p                        -> Names.prefix0 (Path.toName p)
  -- parsing should respond to local and absolute names
  parseNames00       = currentPathNames0 <> absoluteRootNames0
  -- pretty-printing should use local names where available
  prettyPrintNames00 = currentAndExternalNames0

basicSuffixifiedNames :: Int -> Branch m -> Path -> PPE.PrettyPrintEnv
basicSuffixifiedNames hashLength root path =
  let names0 = basicPrettyPrintNames0 root path
  in  PPE.suffixifiedPPE . PPE.fromNamesDecl hashLength $ Names names0 mempty

basicPrettyPrintNames0 :: Branch m -> Path -> Names0
basicPrettyPrintNames0 root = snd . basicNames0' root

loadReferentType
  :: (Applicative m, Var v)
  => Codebase m v Ann
  -> Referent
  -> m (Maybe (Type v Ann))
loadReferentType codebase = \case
  Referent.Ref r       -> Codebase.getTypeOfTerm codebase r
  Referent.Con r cid _ -> getTypeOfConstructor r cid
 where
  getTypeOfConstructor (Reference.DerivedId r) cid = do
    maybeDecl <- Codebase.getTypeDeclaration codebase r
    pure $ case maybeDecl of
      Nothing   -> Nothing
      Just decl -> DD.typeOfConstructor (either DD.toDataDecl id decl) cid
  getTypeOfConstructor r cid =
    error
      $  "Don't know how to getTypeOfConstructor "
      ++ show r
      ++ " "
      ++ show cid

findShallow
  :: (Monad m, Var v)
  => Codebase m v Ann
  -> Path.Absolute
  -> Backend m [ShallowListEntry v Ann]
findShallow codebase path' = do
  let path = Path.unabsolute path'
  hashLength <- lift $ Codebase.hashLength codebase
  root <- ExceptT . (first BadRootBranch <$>) $ Codebase.getRootBranch codebase
  b0 <-
    maybe (throwError . NoSuchNamespace $ Path.Absolute path) pure
    $   Branch.head
    <$> Branch.getAt path root
  let hqTerm b0 ns r =
        let refs = Star3.lookupD1 ns . Branch._terms $ b0
        in  case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLength $ HQ'.fromNamedReferent ns r
      hqType b0 ns r =
        let refs = Star3.lookupD1 ns . Branch._types $ b0
        in  case length refs of
              1 -> HQ'.fromName ns
              _ -> HQ'.take hashLength $ HQ'.fromNamedReference ns r
      defnCount b =
        (R.size . Branch.deepTerms $ Branch.head b)
          + (R.size . Branch.deepTypes $ Branch.head b)
  termEntries <- for (R.toList . Star3.d1 $ Branch._terms b0) $ \(r, ns) -> do
    ot <- lift $ loadReferentType codebase r
    pure $ ShallowTermEntry r (hqTerm b0 ns r) ot
  let
    typeEntries =
      [ ShallowTypeEntry r (hqType b0 ns r)
      | (r, ns) <- R.toList . Star3.d1 $ Branch._types b0
      ]
    branchEntries =
      [ ShallowBranchEntry ns (defnCount b)
      | (ns, b) <- Map.toList $ Branch._children b0
      ]
    patchEntries =
      [ ShallowPatchEntry ns
      | (ns, (_h, _mp)) <- Map.toList $ Branch._edits b0
      ]
  pure . sort $ termEntries ++ typeEntries ++ branchEntries ++ patchEntries

