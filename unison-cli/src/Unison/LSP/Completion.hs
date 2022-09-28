{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.LSP.Completion where

import Control.Comonad.Cofree
import Control.Lens hiding (List, (:<))
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String.Here.Uninterpolated (here)
import qualified Data.Text as Text
import Language.LSP.Types
import Language.LSP.Types.Lens
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
import qualified Unison.Util.Relation as Relation

-- | Rudimentary auto-completion handler
--
-- TODO:
-- * Rewrite this to use an index rather than fuzzy searching ALL names
-- * Respect ucm's current path
-- * Provide namespaces as auto-complete targets
-- * Auto-complete minimally suffixed names
-- * Include docs in completion details?
completionHandler :: RequestMessage 'TextDocumentCompletion -> (Either ResponseError (ResponseResult 'TextDocumentCompletion) -> Lsp ()) -> Lsp ()
completionHandler m respond =
  respond . maybe (Right $ InL mempty) (Right . InR) =<< runMaybeT do
    (range, prefix) <- MaybeT $ VFS.completionPrefix (m ^. params)
    ppe <- PPED.suffixifiedPPE <$> lift globalPPE
    completions <- lift getCompletions
    let matches =
          matchCompletions completions prefix
            & Set.toList
            & mapMaybe \(name, dep) ->
              let biasedPPE = PPE.biasTo [name] ppe
                  hqName = LD.fold (PPE.types biasedPPE) (PPE.terms biasedPPE) dep
               in mkCompletionItem . HQ'.toText <$> hqName
    let isIncomplete = False -- TODO: be smarter about this
    pure . CompletionList isIncomplete . List $ snippetCompletions prefix range <> matches

snippetCompletions :: Text -> Range -> [CompletionItem]
snippetCompletions prefix range =
  [ ("handler", handlerTemplate),
    ("cases", casesTemplate),
    ("match-with", matchWithTemplate)
  ]
    & filter (Text.isPrefixOf prefix . fst)
    & fmap toCompletion
  where
    toCompletion :: (Text, Text) -> CompletionItem
    toCompletion (pat, snippet) =
      (mkCompletionItem pat)
        { _insertTextFormat = Just Snippet,
          _insertTextMode = Just AdjustIndentation,
          _textEdit = Just $ CompletionEditText (TextEdit range snippet)
        }
    handlerTemplate =
      [here|
handle${1:Ability} : Request (${1:Ability} ${2}) a -> a
handle${1:Ability} = cases
  {${3} -> continue} -> do
    ${4}
    |]
    casesTemplate =
      [here|
cases
  ${1} -> do
    ${2}
    |]
    matchWithTemplate =
      [here|
match ${1} with
  ${2} -> do
    ${3}
    |]

mkCompletionItem :: Text -> CompletionItem
mkCompletionItem lbl =
  CompletionItem
    { _label = lbl,
      _kind = Nothing,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Nothing
    }

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
        let newSubMap = Map.unionWith (\a b -> unCompletionTree $ CompletionTree a <> CompletionTree b) (Map.singleton ns (mempty :< subMap)) subMap
         in helper newSubMap rest

matchCompletions :: CompletionTree -> Text -> Set (Name, LabeledDependency)
matchCompletions (CompletionTree tree) txt =
  matchSegments segments tree
  where
    segments :: [Text]
    segments =
      Text.splitOn "." txt
        & filter (not . Text.null)
    matchSegments :: [Text] -> Cofree (Map NameSegment) (Set (Name, LabeledDependency)) -> Set (Name, LabeledDependency)
    matchSegments xs (currentMatches :< subtreeMap) =
      case xs of
        [] -> currentMatches <> mkMatches subtreeMap
        [ns] ->
          Map.dropWhileAntitone (not . Text.isPrefixOf ns . NameSegment.toText) subtreeMap
            & Map.takeWhileAntitone (Text.isPrefixOf ns . NameSegment.toText)
            & mkMatches
        (ns : rest) ->
          foldMap (matchSegments rest) (Map.lookup (NameSegment ns) subtreeMap)
    mkMatches :: Map NameSegment (Cofree (Map NameSegment) (Set (Name, LabeledDependency))) -> Set (Name, LabeledDependency)
    mkMatches xs = foldMap fold (Map.elems xs)
