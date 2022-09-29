{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.Completion where

import Control.Comonad.Cofree
import Control.Lens hiding (List, (:<))
import Control.Monad.Reader
import Data.Bifunctor (second)
import Data.List.Extra (nubOrdOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.Debug as Debug
import qualified Unison.HashQualified' as HQ'
import Unison.LSP.Types
import qualified Unison.LSP.VFS as VFS
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import qualified Unison.NameSegment as NameSegment
import Unison.Names (Names (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.Referent as Referent
import qualified Unison.Util.Relation as Relation

-- | TODO: move this to a configuration param.
--
-- 'Nothing' will load ALL available completions, which is slower, but may provide a better
-- solution for some users.
--
-- 'Just n' will only fetch the first 'n' completions and will prompt the client to ask for
-- more completions after more typing.
completionLimit :: Maybe Int
completionLimit = Just 100

completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond =
  respond . maybe (Right $ InL mempty) (Right . InR) =<< runMaybeT do
    (range, prefix) <- MaybeT $ VFS.completionPrefix (m ^. params)
    Debug.debugM Debug.LSP "PREFIX" prefix
    ppe <- PPED.suffixifiedPPE <$> lift globalPPE
    completions <- lift getCompletions
    let defMatches = matchCompletions completions prefix
    let (isIncomplete, defCompletions) =
          defMatches
            & nubOrdOn (\(p, _name, ref) -> (p, ref))
            & fmap (over _1 Path.toText)
            & ( let x = Text.dropWhileEnd (== '.') prefix
                 in filter (\(path, _name, _ref) -> path /= x) -- Filter out completions that already match
              )
            & case completionLimit of
              Nothing -> (False,)
              Just n -> takeCompletions n
    let defCompletionItems =
          defCompletions
            & mapMaybe \(path, fqn, dep) ->
              let biasedPPE = PPE.biasTo [fqn] ppe
                  hqName = LD.fold (PPE.types biasedPPE) (PPE.terms biasedPPE) dep
               in hqName <&> \hqName -> mkDefCompletionItem range path (HQ'.toText hqName) dep
    pure . CompletionList isIncomplete . List $ defCompletionItems
  where
    takeCompletions :: Int -> [a] -> (Bool, [a])
    takeCompletions 0 xs = (not $ null xs, [])
    takeCompletions _ [] = (False, [])
    takeCompletions n (x : xs) = second (x :) $ takeCompletions (pred n) xs

mkDefCompletionItem :: Range -> Text -> Text -> LabeledDependency -> CompletionItem
mkDefCompletionItem range path suffixified dep =
  CompletionItem
    { _label = lbl,
      _kind = case dep of
        LD.TypeReference _ref -> Just CiClass
        LD.TermReferent ref -> case ref of
          Referent.Con {} -> Just CiConstructor
          Referent.Ref {} -> Just CiValue,
      _tags = Nothing,
      _detail = Just detail,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      -- Sort def completions after path completions
      _sortText = Just ("1" <> lbl),
      _filterText = Just path,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Just (CompletionEditText $ TextEdit range suffixified),
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Nothing
    }
  where
    (lbl, detail) =
      if Text.length path > Text.length suffixified
        then (path, suffixified)
        else (suffixified, path)

mkPathCompletionItem :: Range -> Path -> CompletionItem
mkPathCompletionItem range path =
  CompletionItem
    { _label = lbl <> ".",
      _kind = Just CiModule,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      -- Sort path completions first
      _sortText = Just ("0" <> lbl),
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Just (CompletionEditText $ TextEdit range lbl),
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Nothing
    }
  where
    lbl = Path.toText path

namesToCompletionTree :: Names -> CompletionTree
namesToCompletionTree Names {terms, types} =
  let typeCompls =
        Relation.domain types
          & ifoldMap (\name refs -> refs & Set.map \ref -> (name, LD.typeRef ref))
      termCompls =
        Relation.domain terms
          & ifoldMap (\name refs -> refs & Set.map \ref -> (name, LD.referent ref))
   in foldMap (uncurry nameToCompletionTree) (typeCompls <> termCompls)

nameToCompletionTree :: Name -> LabeledDependency -> CompletionTree
nameToCompletionTree name ref =
  let (lastSegment :| prefix) = Name.reverseSegments name
      complMap = helper (Map.singleton lastSegment (Set.singleton (name, ref) :< mempty)) prefix
   in CompletionTree (mempty :< complMap)
  where
    helper ::
      Map
        NameSegment
        (Cofree (Map NameSegment) (Set (Name, LabeledDependency))) ->
      [NameSegment] ->
      Map
        NameSegment
        (Cofree (Map NameSegment) (Set (Name, LabeledDependency)))
    helper subMap revPrefix = case revPrefix of
      [] -> subMap
      (ns : rest) ->
        mergeSubmaps (helper (Map.singleton ns (mempty :< subMap)) rest) subMap
      where
        mergeSubmaps = Map.unionWith (\a b -> unCompletionTree $ CompletionTree a <> CompletionTree b)

matchCompletions :: CompletionTree -> Text -> [(Path, Name, LabeledDependency)]
matchCompletions (CompletionTree tree) txt =
  matchSegments segments (Set.toList <$> tree)
  where
    segments :: [Text]
    segments =
      Text.splitOn "." txt
        & filter (not . Text.null)
    matchSegments :: [Text] -> Cofree (Map NameSegment) [(Name, LabeledDependency)] -> [(Path, Name, LabeledDependency)]
    matchSegments xs (currentMatches :< subtreeMap) =
      case xs of
        [] ->
          let current = currentMatches <&> (\(name, def) -> (Path.empty, name, def))
           in (current <> mkDefMatches subtreeMap)
        [prefix] ->
          Map.dropWhileAntitone ((< prefix) . NameSegment.toText) subtreeMap
            & Map.takeWhileAntitone (Text.isPrefixOf prefix . NameSegment.toText)
            & \matchingSubtrees ->
              let subMatches = ifoldMap (\ns subTree -> matchSegments [] subTree & consPathPrefix ns) matchingSubtrees
               in subMatches
        (ns : rest) ->
          foldMap (matchSegments rest) (Map.lookup (NameSegment ns) subtreeMap)
            & consPathPrefix (NameSegment ns)
    consPathPrefix :: NameSegment -> ([(Path, Name, LabeledDependency)]) -> [(Path, Name, LabeledDependency)]
    consPathPrefix ns = over (mapped . _1) (Path.cons ns)
    mkDefMatches :: Map NameSegment (Cofree (Map NameSegment) [(Name, LabeledDependency)]) -> [(Path, Name, LabeledDependency)]
    mkDefMatches xs = do
      (ns, (matches :< rest)) <- Map.toList xs
      let childMatches = mkDefMatches rest <&> over _1 (Path.cons ns)
      let currentMatches = matches <&> \(name, dep) -> (Path.singleton ns, name, dep)
      currentMatches <> childMatches
